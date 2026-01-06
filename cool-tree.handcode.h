#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H

#include <vector>
#include <map>
#include <iostream>
#include "symtab.h"
#include "tree.h"

typedef int Boolean;

// ----------------------------------------------------------
// 辅助函数实现 (Inline Implementations)
// ----------------------------------------------------------

// 1. pad 函数实现
inline char *pad(int n) {
    if (n > 80) return (char*)"                                        ";
    static char s[81];
    for (int i = 0; i < n; i++) s[i] = ' ';
    s[n] = '\0';
    return s;
}

// 2. Boolean 辅助函数实现 (修复链接错误的关键)
inline Boolean copy_Boolean(Boolean b) {
    return b;
}

inline void assert_Boolean(Boolean) {}

inline void dump_Boolean(ostream& stream, int padding, Boolean b) {
    stream << pad(padding) << (int) b << "\n";
}

// 3. Symbol 辅助函数声明 (实现通常在库文件中)
Symbol copy_Symbol(Symbol);
void dump_Symbol(ostream&, int, Symbol);
void dump_lineno(ostream&, int);

// ----------------------------------------------------------

class CgenNode;

// ScopeTracker 定义
class ScopeTracker {
private:
    std::vector<std::pair<Symbol, int> > m_scope_stack;
    std::map<Symbol, int> m_param_map;
    int m_current_depth; 

public:
    CgenNode* current_class; 

    ScopeTracker() : m_current_depth(0), current_class(NULL) {}

    void enter_scope() {
        m_scope_stack.push_back(std::make_pair((Symbol)NULL, 0));
    }

    void exit_scope() {
        while (!m_scope_stack.empty()) {
            std::pair<Symbol, int> top = m_scope_stack.back();
            m_scope_stack.pop_back();
            if (top.first == NULL) break; 
            m_current_depth--; 
        }
    }

    void add_local(Symbol name) {
        m_scope_stack.push_back(std::make_pair(name, m_current_depth));
        m_current_depth++;
    }

    void add_param(Symbol name, int offset) {
        m_param_map[name] = offset;
    }

    void push_temporary() {
        m_current_depth++;
    }

    void pop_temporary() {
        m_current_depth--;
    }

    int find_local(Symbol name) {
        for (int i = m_scope_stack.size() - 1; i >= 0; --i) {
            if (m_scope_stack[i].first == name) {
                return m_scope_stack[i].second;
            }
        }
        return -1;
    }

    int find_param(Symbol name) {
        if (m_param_map.find(name) != m_param_map.end()) {
            return m_param_map[name];
        }
        return -1;
    }

    // 在 cgen.cc 中实现
    int find_attribute(Symbol name); 
    
    int get_current_depth() const { return m_current_depth; }
};

#define Program_EXTRAS \
virtual void cgen(ostream&) = 0; \
virtual void dump_with_types(ostream&, int) = 0;

#define program_EXTRAS \
void cgen(ostream&); \
void dump_with_types(ostream&, int);

#define Class__EXTRAS \
virtual Symbol get_name() = 0; \
virtual Symbol get_parent() = 0; \
virtual Symbol get_filename() = 0; \
virtual void dump_with_types(ostream&,int) = 0;

#define class__EXTRAS \
Symbol get_name() { return name; } \
Symbol get_parent() { return parent; } \
Symbol get_filename() { return filename; } \
void dump_with_types(ostream&,int);

#define Feature_EXTRAS \
virtual void dump_with_types(ostream&,int) = 0; \
virtual bool is_method() = 0;

#define Feature_SHARED_EXTRAS \
void dump_with_types(ostream&,int);

#define method_EXTRAS \
bool is_method() { return true; } \
int num_args() { return formals->len(); } \
Symbol get_name() { return name; }

#define attr_EXTRAS \
bool is_method() { return false; } \
Symbol get_name() { return name; }

#define Formal_EXTRAS \
virtual void dump_with_types(ostream&,int) = 0;

#define formal_EXTRAS \
void dump_with_types(ostream&,int); \
Symbol get_name() { return name; }

#define Case_EXTRAS \
virtual void dump_with_types(ostream& ,int) = 0;

#define branch_EXTRAS \
void dump_with_types(ostream& ,int);

#define Expression_EXTRAS \
Symbol type; \
Symbol get_type() { return type; } \
Expression set_type(Symbol s) { type = s; return this; } \
virtual void code(ostream&, ScopeTracker) = 0; \
virtual void dump_with_types(ostream&,int) = 0; \
void dump_type(ostream&, int); \
Expression_class() { type = (Symbol) NULL; }

#define Expression_SHARED_EXTRAS \
void code(ostream&, ScopeTracker); \
void dump_with_types(ostream&,int);

#define dispatch_EXTRAS \
std::vector<Expression> get_actuals() { \
    std::vector<Expression> v; \
    for(int i = actual->first(); actual->more(i); i = actual->next(i)) \
        v.push_back(actual->nth(i)); \
    return v; \
}

#define static_dispatch_EXTRAS \
std::vector<Expression> get_actuals() { \
    std::vector<Expression> v; \
    for(int i = actual->first(); actual->more(i); i = actual->next(i)) \
        v.push_back(actual->nth(i)); \
    return v; \
}

#define typcase_EXTRAS \
std::vector<Case> get_cases() { \
    std::vector<Case> v; \
    for(int i = cases->first(); cases->more(i); i = cases->next(i)) \
        v.push_back(cases->nth(i)); \
    return v; \
}

#endif
