#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

// Global symbols
Symbol
    arg, arg2, Bool, concat, cool_abort, copy, Int, in_int, in_string,
    IO, length, Main, main_meth, No_class, No_type, Object, out_int,
    out_string, prim_slot, self, SELF_TYPE, Str, str_field, substr,
    type_name, val;

static void init_common_symbols(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}

static char *gc_init_names[] = { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] = { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };

// --- BoolConst Definitions Early ---
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }
void BoolConst::code_ref(ostream& s) const { s << BOOLCONST_PREFIX << val; }
void BoolConst::code_def(ostream& s, int tag) {
    s << WORD << "-1" << endl;
    code_ref(s); s << LABEL 
      << WORD << tag << endl 
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl 
      << WORD;
    s << Bool << DISPTAB_SUFFIX << endl;
    s << WORD << val << endl;
}

BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

// --- MIPS Emit Helpers ---

static void emit_load(char *dest, int offset, char *src, ostream& s) {
    s << LW << dest << " " << offset * WORD_SIZE << "(" << src << ")" << endl;
}

static void emit_store(char *src, int offset, char *dest, ostream& s) {
    s << SW << src << " " << offset * WORD_SIZE << "(" << dest << ")" << endl;
}

static void emit_li(char *dest, int val, ostream& s) { 
    s << LI << dest << " " << val << endl; 
}

static void emit_la(char *dest, char *addr, ostream& s) { 
    s << LA << dest << " " << addr << endl; 
}

static void emit_move(char *dest, char *src, ostream& s) { 
    s << MOVE << dest << " " << src << endl; 
}

static void emit_op(const char* op, char *dest, char *src1, char *src2, ostream& s) {
    s << op << " " << dest << " " << src1 << " " << src2 << endl;
}

static void emit_neg(char *dest, char *src1, ostream& s) { 
    s << NEG << dest << " " << src1 << endl; 
}

static void emit_add(char *dest, char *src1, char *src2, ostream& s) { 
    s << ADD << dest << " " << src1 << " " << src2 << endl; 
}

static void emit_addu(char *dest, char *src1, char *src2, ostream& s) { 
    s << ADDU << dest << " " << src1 << " " << src2 << endl; 
}

static void emit_addiu(char *dest, char *src, int imm, ostream& s) { 
    s << ADDIU << dest << " " << src << " " << imm << endl; 
}

static void emit_addi(char *dest, char *src, int imm, ostream& s) { 
    emit_addiu(dest, src, imm, s);
}

static void emit_div(char *dest, char *src1, char *src2, ostream& s) { 
    s << DIV << dest << " " << src1 << " " << src2 << endl; 
}

static void emit_mul(char *dest, char *src1, char *src2, ostream& s) { 
    s << MUL << dest << " " << src1 << " " << src2 << endl; 
}

static void emit_sub(char *dest, char *src1, char *src2, ostream& s) { 
    s << SUB << dest << " " << src1 << " " << src2 << endl; 
}

static void emit_sll(char *dest, char *src1, int num, ostream& s) { 
    s << SLL << dest << " " << src1 << " " << num << endl; 
}

static void emit_jal(char *addr, ostream &s) { 
    s << JAL << addr << endl; 
}

static void emit_jalr(char *reg, ostream& s) { 
    s << JALR << "\t" << reg << endl; 
}

static void emit_ret(ostream& s) { 
    s << RET << endl; 
}

static void emit_gc_check(char *a1, ostream& s) {
    if (a1 != A1) emit_move(A1, a1, s);
    emit_jal("_GenGC_Assign", s); 
}

static void emit_push(char *reg, ostream& s) {
    emit_store(reg, 0, SP, s);
    emit_addi(SP, SP, -4, s);
}

static void emit_partial_load_address(char *dest_reg, ostream& s) { 
    s << LA << dest_reg << " "; 
}

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s) {
    emit_partial_load_address(dest,s);
    b.code_ref(s);
    s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s) {
    emit_partial_load_address(dest, s);
    str->code_ref(s);
    s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s) {
    emit_partial_load_address(dest, s);
    i->code_ref(s);
    s << endl;
}

static void emit_disptable_ref(Symbol sym, ostream& s) { 
    s << sym << DISPTAB_SUFFIX; 
}

static void emit_init_ref(Symbol sym, ostream& s) { 
    s << sym << CLASSINIT_SUFFIX; 
}

static void emit_protobj_ref(Symbol sym, ostream& s) { 
    s << sym << PROTOBJ_SUFFIX; 
}

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s) { 
    s << classname << METHOD_SEP << methodname; 
}

static void emit_label_ref(int l, ostream &s) { 
    s << "label" << l; 
}

static void emit_label_def(int l, ostream &s) {
    emit_label_ref(l,s);
    s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s) {
    s << BEQZ << source << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s) {
    s << BEQ << src1 << " " << src2 << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s) {
    s << BNE << src1 << " " << src2 << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s) {
    s << BLEQ << src1 << " " << src2 << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s) {
    s << BLT << src1 << " " << src2 << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s) {
    s << BLT << src1 << " " << imm << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s) {
    s << BGT << src1 << " " << imm << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_branch(int l, ostream& s) {
    s << BRANCH;
    emit_label_ref(l,s);
    s << endl;
}

static void emit_fetch_int(char *dest, char *source, ostream& s) { 
    emit_load(dest, DEFAULT_OBJFIELDS, source, s); 
}

// --- ScopeTracker Implementation ---

int ScopeTracker::find_attribute(Symbol sym) {
    if (current_class) {
        std::map<Symbol, int>& idx_map = current_class->get_attr_idx_map();
        if (idx_map.find(sym) != idx_map.end()) {
            return idx_map[sym];
        }
    }
    return -1;
}

// Global pointer to class table
extern CgenClassTable *codegen_classtable;
CgenClassTable *codegen_classtable = NULL;

void program_class::cgen(ostream &os)
{
    init_common_symbols();
    // 1. 先构建对象，确保全局变量 codegen_classtable 被赋值
    codegen_classtable = new CgenClassTable(classes, os);
    // 2. 再调用代码生成，此时 codegen_classtable 已经不是 NULL 了
    codegen_classtable->code();
}

// --- String/Int Definitions ---

void StringEntry::code_ref(ostream& s) { s << STRCONST_PREFIX << index; }
void StringEntry::code_def(ostream& s, int tag) {
    IntEntryP lensym = inttable.add_int(len);
    s << WORD << "-1" << endl;
    code_ref(s); s << LABEL 
      << WORD << tag << endl 
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl 
      << WORD;
    s << Str << DISPTAB_SUFFIX << endl;
    s << WORD; lensym->code_ref(s); s << endl;
    emit_string_constant(s, str);
    s << ALIGN;
}

void StrTable::code_string_table(ostream& s, int tag) {
    for (List<StringEntry> *l = tbl; l; l = l->tl()) l->hd()->code_def(s, tag);
}

void IntEntry::code_ref(ostream &s) { s << INTCONST_PREFIX << index; }
void IntEntry::code_def(ostream &s, int tag) {
    s << WORD << "-1" << endl;
    code_ref(s); s << LABEL 
      << WORD << tag << endl 
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl 
      << WORD;
    s << Int << DISPTAB_SUFFIX << endl;
    s << WORD << str << endl;
}

void IntTable::code_string_table(ostream &s, int tag) {
    for (List<IntEntry> *l = tbl; l; l = l->tl()) l->hd()->code_def(s, tag);
}

// --- CgenClassTable Implementation ---

CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s) {
    stringclasstag = 4; intclasstag = 2; boolclasstag = 3;

    enterscope(); 
    if (cgen_debug) cout << "Building CgenClassTable" << endl;

    install_basic_classes();
    install_classes(classes);
    build_inheritance_tree();

    int next_tag = 0;
    assign_tags(root(), next_tag);

    // 注意：删除了这里的 code() 调用，移到了 program_class::cgen 中
}

void CgenClassTable::code() {
    emit_global_data();
    emit_gc_select();
    emit_constants();
    emit_class_name_tab();
    emit_class_obj_tab();
    emit_dispatch_tabs();
    emit_prototypes();
    emit_global_text();
    emit_initializers();
    emit_methods();
    
    // 生成完代码后退出作用域
    exitscope();
}

void CgenClassTable::install_basic_classes() {
    Symbol filename = stringtable.add_string("<basic class>");

    addid(No_class, new CgenNode(class_(No_class,No_class,nil_Features(),filename), Basic,this));
    addid(SELF_TYPE, new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename), Basic,this));
    addid(prim_slot, new CgenNode(class_(prim_slot,No_class,nil_Features(),filename), Basic,this));

    install_class(new CgenNode(class_(Object, No_class,
        append_Features(append_Features(
        single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
        single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
        single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
        filename), Basic,this));

    install_class(new CgenNode(class_(IO, Object,
        append_Features(append_Features(append_Features(
        single_Features(method(out_string, single_Formals(formal(arg, Str)), SELF_TYPE, no_expr())),
        single_Features(method(out_int, single_Formals(formal(arg, Int)), SELF_TYPE, no_expr()))),
        single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
        single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
        filename), Basic,this));

    install_class(new CgenNode(class_(Int, Object, single_Features(attr(val, prim_slot, no_expr())), filename), Basic,this));
    install_class(new CgenNode(class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename), Basic,this));
    install_class(new CgenNode(class_(Str, Object,
        append_Features(append_Features(append_Features(append_Features(
        single_Features(attr(val, Int, no_expr())),
        single_Features(attr(str_field, prim_slot, no_expr()))),
        single_Features(method(length, nil_Formals(), Int, no_expr()))),
        single_Features(method(concat, single_Formals(formal(arg, Str)), Str, no_expr()))),
        single_Features(method(substr, append_Formals(single_Formals(formal(arg, Int)), single_Formals(formal(arg2, Int))), Str, no_expr()))),
        filename), Basic,this));
}

void CgenClassTable::install_class(CgenNodeP nd) {
    Symbol name = nd->get_name();
    if (probe(name)) return;
    nds = new List<CgenNode>(nd,nds);
    addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs) {
    for(int i = cs->first(); cs->more(i); i = cs->next(i))
        install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

void CgenClassTable::build_inheritance_tree() {
    for(List<CgenNode> *l = nds; l; l = l->tl())
        set_relations(l->hd());
}

void CgenClassTable::set_relations(CgenNodeP nd) {
    CgenNode *parent = probe(nd->get_parent());
    nd->set_parentnd(parent);
    parent->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n) {
    children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p) {
    assert(parentnd == NULL);
    assert(p != NULL);
    parentnd = p;
}

void CgenClassTable::assign_tags(CgenNode* node, int& counter) {
    node->tag_id = counter++;
    
    // Cache specific basic tags
    if (node->get_name() == Str) stringclasstag = node->tag_id;
    else if (node->get_name() == Int) intclasstag = node->tag_id;
    else if (node->get_name() == Bool) boolclasstag = node->tag_id;

    m_tag_lookup[node->get_name()] = node->tag_id;

    List<CgenNode>* children = node->get_children();
    for (List<CgenNode>* l = children; l; l = l->tl()) {
        assign_tags(l->hd(), counter);
    }
    node->max_descendant_tag = counter - 1; 
}

std::vector<CgenNode*> CgenClassTable::get_all_nodes() {
    if (m_all_nodes.empty()) {
        for (List<CgenNode> *l = nds; l; l = l->tl()) {
            m_all_nodes.push_back(l->hd());
        }
        std::reverse(m_all_nodes.begin(), m_all_nodes.end());
    }
    return m_all_nodes;
}

CgenNodeP CgenClassTable::root() { return probe(Object); }

void CgenClassTable::emit_global_data() {
    Symbol main    = idtable.lookup_string(MAINNAME);
    Symbol string  = idtable.lookup_string(STRINGNAME);
    Symbol integer = idtable.lookup_string(INTNAME);

    str << "\t.data\n" << ALIGN;
    str << GLOBAL << CLASSNAMETAB << endl;
    str << GLOBAL; emit_protobj_ref(main,str); str << endl;
    str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
    str << GLOBAL; emit_protobj_ref(string,str); str << endl;
    str << GLOBAL; falsebool.code_ref(str); str << endl;
    str << GLOBAL; truebool.code_ref(str); str << endl;
    str << GLOBAL << INTTAG << endl;
    str << GLOBAL << BOOLTAG << endl;
    str << GLOBAL << STRINGTAG << endl;

    str << INTTAG << LABEL << WORD << intclasstag << endl;
    str << BOOLTAG << LABEL << WORD << boolclasstag << endl;
    str << STRINGTAG << LABEL << WORD << stringclasstag << endl;
}

void CgenClassTable::emit_gc_select() {
    str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
    str << "_MemMgr_INITIALIZER:" << endl;
    str << WORD << gc_init_names[cgen_Memmgr] << endl;
    str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
    str << "_MemMgr_COLLECTOR:" << endl;
    str << WORD << gc_collect_names[cgen_Memmgr] << endl;
    str << GLOBAL << "_MemMgr_TEST" << endl;
    str << "_MemMgr_TEST:" << endl;
    str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}

void CgenClassTable::emit_constants() {
    stringtable.add_string("");
    inttable.add_string("0");
    stringtable.code_string_table(str,stringclasstag);
    inttable.code_string_table(str,intclasstag);
    falsebool.code_def(str,boolclasstag);
    truebool.code_def(str,boolclasstag);
}

void CgenClassTable::emit_class_name_tab() {
    str << CLASSNAMETAB << LABEL;
    std::vector<CgenNode*> sorted_nodes(get_all_nodes().size());
    for (CgenNode* n : get_all_nodes()) {
        sorted_nodes[n->tag_id] = n;
    }
    for (CgenNode* node : sorted_nodes) {
        str << WORD;
        stringtable.lookup_string(node->get_name()->get_string())->code_ref(str);
        str << endl;
    }
}

void CgenClassTable::emit_class_obj_tab() {
    str << CLASSOBJTAB << LABEL;
    std::vector<CgenNode*> sorted_nodes(get_all_nodes().size());
    for (CgenNode* n : get_all_nodes()) {
        sorted_nodes[n->tag_id] = n;
    }
    for (CgenNode* node : sorted_nodes) {
        str << WORD; emit_protobj_ref(node->get_name(), str); str << endl;
        str << WORD; emit_init_ref(node->get_name(), str); str << endl;
    }
}

void CgenClassTable::emit_dispatch_tabs() {
    for (CgenNode* node : get_all_nodes()) {
        emit_disptable_ref(node->get_name(), str); str << LABEL;
        std::vector<method_class*> methods = node->get_full_methods();
        std::map<Symbol, Symbol> class_map = node->get_method_class_map();
        for (method_class* m : methods) {
            str << WORD;
            emit_method_ref(class_map[m->name], m->name, str);
            str << endl;
        }
    }
}

void CgenClassTable::emit_prototypes() {
    for (CgenNode* node : get_all_nodes()) {
        node->gen_prototype(str);
    }
}

void CgenClassTable::emit_global_text() {
    str << GLOBAL << HEAP_START << endl
        << HEAP_START << LABEL
        << WORD << 0 << endl
        << "\t.text" << endl
        << GLOBAL;
    emit_init_ref(idtable.add_string("Main"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Int"),str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("String"),str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Bool"),str);
    str << endl << GLOBAL;
    emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
    str << endl;
}

void CgenClassTable::emit_initializers() {
    for (CgenNode* node : get_all_nodes()) {
        node->gen_init(str);
    }
}

void CgenClassTable::emit_methods() {
    for (CgenNode* node : get_all_nodes()) {
        if (!node->basic()) {
            node->gen_methods(str);
        }
    }
}

// --- CgenNode Implementation ---

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
    class__class((const class__class &) *nd),
    parentnd(NULL),
    children(NULL),
    basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());
}

std::vector<CgenNode*> CgenNode::get_inheritance_path() {
    if (m_inheritance_path.empty()) {
        CgenNode* curr = this;
        while (curr->get_name() != No_class) {
            m_inheritance_path.push_back(curr);
            curr = curr->get_parentnd();
        }
        std::reverse(m_inheritance_path.begin(), m_inheritance_path.end());
    }
    return m_inheritance_path;
}

std::vector<attr_class*> CgenNode::get_full_attributes() {
    if (m_all_attributes.empty()) {
        std::vector<CgenNode*> path = get_inheritance_path();
        int offset = 0;
        for (CgenNode* node : path) {
            Features fs = node->features;
            for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
                Feature f = fs->nth(i);
                if (!f->is_method()) {
                    attr_class* a = (attr_class*)f;
                    m_all_attributes.push_back(a);
                    m_attr_idx[a->name] = offset++;
                }
            }
        }
    }
    return m_all_attributes;
}

std::vector<method_class*> CgenNode::get_full_methods() {
    if (m_all_methods.empty()) {
        std::vector<CgenNode*> path = get_inheritance_path();
        for (CgenNode* node : path) {
            Features fs = node->features;
            for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
                Feature f = fs->nth(i);
                if (f->is_method()) {
                    method_class* m = (method_class*)f;
                    if (m_method_idx.find(m->name) == m_method_idx.end()) {
                        // New method found
                        m_method_idx[m->name] = m_all_methods.size();
                        m_all_methods.push_back(m);
                        m_method_class[m->name] = node->name;
                    } else {
                        // Overridden method
                        int idx = m_method_idx[m->name];
                        m_all_methods[idx] = m;
                        m_method_class[m->name] = node->name;
                    }
                }
            }
        }
    }
    return m_all_methods;
}

std::map<Symbol, int>& CgenNode::get_method_idx_map() {
    get_full_methods();
    return m_method_idx;
}
std::map<Symbol, Symbol>& CgenNode::get_method_class_map() {
    get_full_methods();
    return m_method_class;
}
std::map<Symbol, int>& CgenNode::get_attr_idx_map() {
    get_full_attributes();
    return m_attr_idx;
}

void CgenNode::gen_prototype(ostream& s) {
    std::vector<attr_class*> attrs = get_full_attributes();
    s << WORD << "-1" << endl;
    emit_protobj_ref(name, s); s << LABEL;
    s << WORD << tag_id << "\t# class tag" << endl;
    s << WORD << (DEFAULT_OBJFIELDS + attrs.size()) << "\t# size" << endl;
    s << WORD; emit_disptable_ref(name, s); s << endl;
    for (attr_class* a : attrs) {
        s << WORD;
        if (a->type_decl == Int) inttable.lookup_string("0")->code_ref(s);
        else if (a->type_decl == Str) stringtable.lookup_string("")->code_ref(s);
        else if (a->type_decl == Bool) falsebool.code_ref(s);
        else s << "0";
        s << endl;
    }
}

void CgenNode::gen_init(ostream& s) {
    emit_init_ref(name, s); s << LABEL;
    emit_addi(SP, SP, -12, s);
    emit_store(FP, 3, SP, s);
    emit_store(SELF, 2, SP, s);
    emit_store(RA, 1, SP, s);
    emit_addi(FP, SP, 4, s);
    emit_move(SELF, ACC, s);

    if (parentnd && name != Object) {
        s << JAL; emit_init_ref(parentnd->name, s); s << endl;
    }

    Features fs = features;
    for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
        Feature f = fs->nth(i);
        if (!f->is_method()) {
            attr_class* a = (attr_class*)f;
            if (a->init->get_type() != NULL) {
                ScopeTracker tracker;
                tracker.current_class = this;
                a->init->code(s, tracker);
                int idx = m_attr_idx[a->name];
                emit_store(ACC, idx + DEFAULT_OBJFIELDS, SELF, s);
                if (cgen_Memmgr == 1) {
                    emit_addi(A1, SELF, 4 * (idx + DEFAULT_OBJFIELDS), s);
                    emit_gc_check(A1, s);
                }
            }
        }
    }
    emit_move(ACC, SELF, s);
    emit_load(FP, 3, SP, s);
    emit_load(SELF, 2, SP, s);
    emit_load(RA, 1, SP, s);
    emit_addi(SP, SP, 12, s);
    emit_ret(s);
}

void CgenNode::gen_methods(ostream& s) {
    Features fs = features;
    for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
        Feature f = fs->nth(i);
        if (f->is_method()) {
            method_class* m = (method_class*)f;
            emit_method_ref(name, m->name, s); s << LABEL;
            
            emit_addi(SP, SP, -12, s);
            emit_store(FP, 3, SP, s);
            emit_store(SELF, 2, SP, s);
            emit_store(RA, 1, SP, s);
            emit_addi(FP, SP, 4, s);
            emit_move(SELF, ACC, s);

            ScopeTracker tracker;
            tracker.current_class = this;
            int arg_count = m->num_args();
            for (int j = 0; j < arg_count; ++j) {
                formal_class* arg = (formal_class*)m->formals->nth(j);
                tracker.add_param(arg->get_name(), arg_count - 1 - j);
            }
            
            m->expr->code(s, tracker);

            emit_load(FP, 3, SP, s);
            emit_load(SELF, 2, SP, s);
            emit_load(RA, 1, SP, s);
            emit_addi(SP, SP, 12 + arg_count * 4, s); // Pop frame + args
            emit_ret(s);
        }
    }
}

// --- Expression Code Generation ---

int label_counter = 0;

void assign_class::code(ostream &s, ScopeTracker env) {
    expr->code(s, env);
    int idx = env.find_local(name);
    if (idx != -1) {
        // Dynamic access to stack variable
        int offset = env.get_current_depth() - idx;
        emit_store(ACC, offset, SP, s);
        if (cgen_Memmgr == 1) {
             emit_addi(A1, SP, 4 * offset, s);
             emit_gc_check(A1, s);
        }
    } else {
        idx = env.find_param(name);
        if (idx != -1) {
            emit_store(ACC, idx + 3, FP, s);
            if (cgen_Memmgr == 1) {
                emit_addi(A1, FP, 4 * (idx + 3), s);
                emit_gc_check(A1, s);
            }
        } else {
            idx = env.find_attribute(name);
            if (idx != -1) {
                emit_store(ACC, idx + DEFAULT_OBJFIELDS, SELF, s);
                if (cgen_Memmgr == 1) {
                    emit_addi(A1, SELF, 4 * (idx + DEFAULT_OBJFIELDS), s);
                    emit_gc_check(A1, s);
                }
            }
        }
    }
}

void static_dispatch_class::code(ostream &s, ScopeTracker env) {
    std::vector<Expression> actuals = get_actuals();
    for (Expression e : actuals) {
        e->code(s, env);
        emit_push(ACC, s);
        env.push_temporary();
    }
    
    expr->code(s, env);
    
    int not_void_label = label_counter++;
    emit_bne(ACC, ZERO, not_void_label, s);
    emit_load_string(ACC, stringtable.lookup_string(env.current_class->get_filename()->get_string()), s);
    emit_li(T1, 1, s);
    emit_jal("_dispatch_abort", s);
    emit_label_def(not_void_label, s);
    
    std::string method_tab = std::string(type_name->get_string()) + DISPTAB_SUFFIX;
    emit_la(T1, (char*)method_tab.c_str(), s);
    
    CgenNode* target = codegen_classtable->get_node(type_name);
    int idx = target->get_method_idx_map()[name];
    
    emit_load(T1, idx, T1, s);
    emit_jalr(T1, s);
    
    for (size_t i = 0; i < actuals.size(); ++i) env.pop_temporary();
}

void dispatch_class::code(ostream &s, ScopeTracker env) {
    std::vector<Expression> actuals = get_actuals();
    for (Expression e : actuals) {
        e->code(s, env);
        emit_push(ACC, s);
        env.push_temporary();
    }
    
    expr->code(s, env);
    
    int not_void_label = label_counter++;
    emit_bne(ACC, ZERO, not_void_label, s);
    emit_load_string(ACC, stringtable.lookup_string(env.current_class->get_filename()->get_string()), s);
    emit_li(T1, 1, s);
    emit_jal("_dispatch_abort", s);
    emit_label_def(not_void_label, s);
    
    Symbol type = expr->get_type();
    Symbol class_name = (type == SELF_TYPE) ? env.current_class->get_name() : type;
    CgenNode* node = codegen_classtable->get_node(class_name);
    int idx = node->get_method_idx_map()[name];
    
    emit_load(T1, 2, ACC, s); // T1 = dispatch table
    emit_load(T1, idx, T1, s); // T1 = method address
    emit_jalr(T1, s);
    
    for (size_t i = 0; i < actuals.size(); ++i) env.pop_temporary();
}

void cond_class::code(ostream &s, ScopeTracker env) {
    pred->code(s, env);
    emit_fetch_int(T1, ACC, s); // fetch val
    int false_lbl = label_counter++;
    int end_lbl = label_counter++;
    emit_beq(T1, ZERO, false_lbl, s);
    then_exp->code(s, env);
    emit_branch(end_lbl, s);
    emit_label_def(false_lbl, s);
    else_exp->code(s, env);
    emit_label_def(end_lbl, s);
}

void loop_class::code(ostream &s, ScopeTracker env) {
    int start_lbl = label_counter++;
    int end_lbl = label_counter++;
    emit_label_def(start_lbl, s);
    pred->code(s, env);
    emit_fetch_int(T1, ACC, s);
    emit_beq(T1, ZERO, end_lbl, s);
    body->code(s, env);
    emit_branch(start_lbl, s);
    emit_label_def(end_lbl, s);
    emit_move(ACC, ZERO, s);
}

void typcase_class::code(ostream &s, ScopeTracker env) {
    expr->code(s, env);
    
    int valid_label = label_counter++;
    emit_bne(ACC, ZERO, valid_label, s);
    emit_load_string(ACC, stringtable.lookup_string(env.current_class->get_filename()->get_string()), s);
    emit_li(T1, 1, s);
    emit_jal("_case_abort2", s);
    emit_label_def(valid_label, s);
    
    emit_load(T1, 0, ACC, s); // T1 = object tag
    
    std::vector<Case> cases = get_cases();
    // Sort by inheritance depth (Deepest first for correct matching)
    std::sort(cases.begin(), cases.end(), [&](Case a, Case b) {
        CgenNode* n1 = codegen_classtable->get_node(((branch_class*)a)->type_decl);
        CgenNode* n2 = codegen_classtable->get_node(((branch_class*)b)->type_decl);
        return n1->get_inheritance_path().size() > n2->get_inheritance_path().size();
    });
    
    int exit_label = label_counter++;
    
    for (Case c : cases) {
        branch_class* b = (branch_class*)c;
        int next_case_label = label_counter++;
        CgenNode* typeNode = codegen_classtable->get_node(b->type_decl);
        
        // Range check: min_tag <= tag <= max_tag
        emit_blti(T1, typeNode->tag_id, next_case_label, s);
        emit_bgti(T1, typeNode->max_descendant_tag, next_case_label, s);
        
        // Match found
        env.enter_scope();
        env.add_local(b->name);
        emit_push(ACC, s); 
        b->expr->code(s, env);
        emit_addi(SP, SP, 4, s); // Restore stack
        env.exit_scope();
        
        emit_branch(exit_label, s);
        emit_label_def(next_case_label, s);
    }
    
    emit_jal("_case_abort", s);
    emit_label_def(exit_label, s);
}

void block_class::code(ostream &s, ScopeTracker env) {
    for (int i = body->first(); body->more(i); i = body->next(i))
        body->nth(i)->code(s, env);
}

void let_class::code(ostream &s, ScopeTracker env) {
    if (init->get_type() == No_type) {
        if (type_decl == Int) emit_load_int(ACC, inttable.lookup_string("0"), s);
        else if (type_decl == Str) emit_load_string(ACC, stringtable.lookup_string(""), s);
        else if (type_decl == Bool) emit_load_bool(ACC, BoolConst(0), s);
        else emit_move(ACC, ZERO, s);
    } else {
        init->code(s, env);
    }
    
    env.enter_scope();
    env.add_local(identifier);
    emit_push(ACC, s);
    
    body->code(s, env);
    
    emit_addi(SP, SP, 4, s);
    env.exit_scope();
}

void plus_class::code(ostream &s, ScopeTracker env) {
    e1->code(s, env);
    emit_push(ACC, s);
    env.push_temporary();
    e2->code(s, env);
    emit_jal("Object.copy", s); // Copy result object
    emit_load(T1, 1, SP, s);    // T1 = e1
    emit_addi(SP, SP, 4, s);
    env.pop_temporary();
    emit_load(T1, DEFAULT_OBJFIELDS, T1, s); // T1 = val(e1)
    emit_load(T2, DEFAULT_OBJFIELDS, ACC, s); // T2 = val(e2)
    emit_add(T3, T1, T2, s);
    emit_store(T3, DEFAULT_OBJFIELDS, ACC, s);
}

void sub_class::code(ostream &s, ScopeTracker env) {
    e1->code(s, env);
    emit_push(ACC, s);
    env.push_temporary();
    e2->code(s, env);
    emit_jal("Object.copy", s);
    emit_load(T1, 1, SP, s);
    emit_addi(SP, SP, 4, s);
    env.pop_temporary();
    emit_load(T1, DEFAULT_OBJFIELDS, T1, s);
    emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);
    emit_sub(T3, T1, T2, s);
    emit_store(T3, DEFAULT_OBJFIELDS, ACC, s);
}

void mul_class::code(ostream &s, ScopeTracker env) {
    e1->code(s, env);
    emit_push(ACC, s);
    env.push_temporary();
    e2->code(s, env);
    emit_jal("Object.copy", s);
    emit_load(T1, 1, SP, s);
    emit_addi(SP, SP, 4, s);
    env.pop_temporary();
    emit_load(T1, DEFAULT_OBJFIELDS, T1, s);
    emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);
    emit_mul(T3, T1, T2, s);
    emit_store(T3, DEFAULT_OBJFIELDS, ACC, s);
}

void divide_class::code(ostream &s, ScopeTracker env) {
    e1->code(s, env);
    emit_push(ACC, s);
    env.push_temporary();
    e2->code(s, env);
    emit_jal("Object.copy", s);
    emit_load(T1, 1, SP, s);
    emit_addi(SP, SP, 4, s);
    env.pop_temporary();
    emit_load(T1, DEFAULT_OBJFIELDS, T1, s);
    emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);
    emit_div(T3, T1, T2, s);
    emit_store(T3, DEFAULT_OBJFIELDS, ACC, s);
}

void neg_class::code(ostream &s, ScopeTracker env) {
    e1->code(s, env);
    emit_jal("Object.copy", s);
    emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);
    emit_neg(T1, T1, s);
    emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
}

void lt_class::code(ostream &s, ScopeTracker env) {
    e1->code(s, env);
    emit_push(ACC, s);
    env.push_temporary();
    e2->code(s, env);
    emit_load(T1, 1, SP, s);
    emit_addi(SP, SP, 4, s);
    env.pop_temporary();
    emit_load(T1, DEFAULT_OBJFIELDS, T1, s);
    emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);
    emit_load_bool(ACC, BoolConst(1), s);
    int label = label_counter++;
    emit_blt(T1, T2, label, s);
    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(label, s);
}

void eq_class::code(ostream &s, ScopeTracker env) {
    e1->code(s, env);
    emit_push(ACC, s);
    env.push_temporary();
    e2->code(s, env);
    emit_move(T2, ACC, s);
    emit_load(T1, 1, SP, s);
    emit_addi(SP, SP, 4, s);
    env.pop_temporary();
    
    int true_lbl = label_counter++;
    int done_lbl = label_counter++;
    
    // Check pointer equality first
    emit_beq(T1, T2, true_lbl, s);
    
    // Call equality_test
    emit_load_bool(ACC, BoolConst(1), s);
    emit_load_bool(A1, BoolConst(0), s);
    emit_jal("equality_test", s);
    emit_branch(done_lbl, s);
    
    emit_label_def(true_lbl, s);
    emit_load_bool(ACC, BoolConst(1), s);
    emit_label_def(done_lbl, s);
}

void leq_class::code(ostream &s, ScopeTracker env) {
    e1->code(s, env);
    emit_push(ACC, s);
    env.push_temporary();
    e2->code(s, env);
    emit_load(T1, 1, SP, s);
    emit_addi(SP, SP, 4, s);
    env.pop_temporary();
    emit_load(T1, DEFAULT_OBJFIELDS, T1, s);
    emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);
    emit_load_bool(ACC, BoolConst(1), s);
    int label = label_counter++;
    emit_bleq(T1, T2, label, s);
    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(label, s);
}

void comp_class::code(ostream &s, ScopeTracker env) {
    e1->code(s, env);
    emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);
    int label = label_counter++;
    emit_load_bool(ACC, BoolConst(1), s);
    emit_beqz(T1, label, s);
    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(label, s);
}

void int_const_class::code(ostream& s, ScopeTracker env) {
    emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s, ScopeTracker env) {
    emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s, ScopeTracker env) {
    emit_load_bool(ACC, BoolConst(this->val), s);
}

void new__class::code(ostream &s, ScopeTracker env) {
    if (type_name == SELF_TYPE) {
        // Dynamic object creation
        emit_la(T1, "class_objTab", s);
        emit_load(T2, 0, SELF, s); // fetch tag
        emit_sll(T2, T2, 3, s);
        emit_addu(T1, T1, T2, s);
        emit_push(T1, s);
        emit_load(ACC, 0, T1, s); // load protObj
        emit_jal("Object.copy", s);
        emit_load(T1, 1, SP, s);
        emit_addi(SP, SP, 4, s);
        emit_load(T1, 1, T1, s); // load init address
        emit_jalr(T1, s);
    } else {
        // Static object creation
        std::string proto = std::string(type_name->get_string()) + PROTOBJ_SUFFIX;
        emit_la(ACC, (char*)proto.c_str(), s);
        emit_jal("Object.copy", s);
        std::string init = std::string(type_name->get_string()) + CLASSINIT_SUFFIX;
        emit_jal((char*)init.c_str(), s);
    }
}

void isvoid_class::code(ostream &s, ScopeTracker env) {
    e1->code(s, env);
    emit_move(T1, ACC, s);
    emit_load_bool(ACC, BoolConst(1), s);
    int label = label_counter++;
    emit_beqz(T1, label, s);
    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(label, s);
}

void no_expr_class::code(ostream &s, ScopeTracker env) {
    emit_move(ACC, ZERO, s);
}

void object_class::code(ostream &s, ScopeTracker env) {
    if (name == self) {
        emit_move(ACC, SELF, s);
        return;
    }
    
    // Check local variables first
    int idx = env.find_local(name);
    if (idx != -1) {
        // Correctly calculate offset from current SP
        int offset = env.get_current_depth() - idx;
        emit_load(ACC, offset, SP, s);
        return;
    }
    
    // Check parameters
    idx = env.find_param(name);
    if (idx != -1) {
        emit_load(ACC, idx + 3, FP, s);
        return;
    }
    
    // Check attributes
    idx = env.find_attribute(name);
    if (idx != -1) {
        emit_load(ACC, idx + DEFAULT_OBJFIELDS, SELF, s);
        return;
    }
}
