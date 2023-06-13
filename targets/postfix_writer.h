#ifndef __MML_TARGETS_POSTFIX_WRITER_H__
#define __MML_TARGETS_POSTFIX_WRITER_H__

#include "targets/basic_ast_visitor.h"

#include <set>
#include <stack>
#include "targets/symbol.h"
#include <sstream>
#include <cdk/emitters/basic_postfix_emitter.h>

namespace mml {

  //!
  //! Traverse syntax tree and generate the corresponding assembly code.
  //!
  class postfix_writer: public basic_ast_visitor {
    
    cdk::symbol_table<mml::symbol> &_symtab;
    std::shared_ptr<mml::symbol> _curr_function; // Saves the function and arguments
    std::vector<std::shared_ptr<mml::symbol>> _function_symbols; // Saves the function symbols
    
    std::set<std::string> _external_functions;
    std::string _function_label;
    std::vector<std::string> _return_labels;
    std::string _extern_label;
    std::set<std::string> _not_declared_symbols;
    std::vector<int> _whileConditionLabel, _whileEndLabel;
    cdk::basic_postfix_emitter &_pf;
    
    int _lbl;
    int _offset;
    int _while_counter = 0;
    bool _in_function_body = false;
    bool _in_function_args = false;
    bool _return_seen;

  public:
    postfix_writer(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<mml::symbol> &symtab,
                   cdk::basic_postfix_emitter &pf) :
        basic_ast_visitor(compiler), _symtab(symtab), _pf(pf), _lbl(0), _offset(0) {
    }

    ~postfix_writer() {
      os().flush();
    }

  private:
    /** Method used to generate sequential labels. */
    inline std::string mklbl(int lbl) {
      std::ostringstream oss;
      if (lbl < 0)
        oss << ".L" << -lbl;
      else
        oss << "_L" << lbl;
      return oss.str();
    }

    // do not edit these lines
    #define __IN_VISITOR_HEADER__
    #include ".auto/visitor_decls.h"  // automatically generated
    #undef __IN_VISITOR_HEADER__
    // do not edit these lines: end
  };

} // mml

#endif
