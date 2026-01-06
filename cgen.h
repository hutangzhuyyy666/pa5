#ifndef CGEN_H
#define CGEN_H

#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"
#include <vector>
#include <map>
#include <algorithm>

enum Basicness {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

// ScopeTracker is now defined in cool-tree.handcode.h to allow correct inclusion order

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
    List<CgenNode> *nds;
    ostream& str;
    int stringclasstag;
    int intclasstag;
    int boolclasstag;

    std::vector<CgenNode*> m_all_nodes; 
    std::map<Symbol, int> m_tag_lookup; 

    void assign_tags(CgenNode* node, int& counter);

    void emit_global_data();
    void emit_gc_select();
    void emit_constants();
    void emit_class_name_tab();
    void emit_class_obj_tab();
    void emit_dispatch_tabs();
    void emit_prototypes();
    void emit_global_text();
    void emit_initializers();
    void emit_methods();

    void install_basic_classes();
    void install_class(CgenNodeP nd);
    void install_classes(Classes cs);
    void build_inheritance_tree();
    void set_relations(CgenNodeP nd);

public:
    CgenClassTable(Classes, ostream& str);
    void code();
    CgenNodeP root();

    CgenNodeP get_node(Symbol name) { return probe(name); }
    std::vector<CgenNode*> get_all_nodes();
};

class CgenNode : public class__class {
private:
    CgenNodeP parentnd; 
    List<CgenNode> *children; 
    Basicness basic_status; 

    std::vector<CgenNode*> m_inheritance_path;
    std::vector<attr_class*> m_all_attributes;
    std::vector<method_class*> m_all_methods;
    
    std::map<Symbol, int> m_method_idx; 
    std::map<Symbol, Symbol> m_method_class; 
    std::map<Symbol, int> m_attr_idx; 

public:
    int tag_id; 
    int max_descendant_tag; 

    CgenNode(Class_ c, Basicness bstatus, CgenClassTableP class_table);

    void add_child(CgenNodeP child);
    List<CgenNode> *get_children() { return children; }
    void set_parentnd(CgenNodeP p);
    CgenNodeP get_parentnd() { return parentnd; }
    int basic() { return (basic_status == Basic); }

    void gen_prototype(ostream& s);
    void gen_init(ostream& s);
    void gen_methods(ostream& s); 

    std::vector<CgenNode*> get_inheritance_path(); 
    std::vector<attr_class*> get_full_attributes(); 
    std::vector<method_class*> get_full_methods(); 

    std::map<Symbol, int>& get_method_idx_map();
    std::map<Symbol, Symbol>& get_method_class_map();
    std::map<Symbol, int>& get_attr_idx_map();
};

class BoolConst {
private:
    int val;
public:
    BoolConst(int);
    void code_def(ostream&, int boolclasstag);
    void code_ref(ostream&) const;
};

#endif
