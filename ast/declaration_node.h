#ifndef __MML_AST_DECLARATION_NODE_H__
#define __MML_AST_DECLARATION_NODE_H__

#include <cdk/ast/expression_node.h>
#include <cdk/ast/typed_node.h>
#include <cdk/types/basic_type.h>

namespace mml {

  /**
   * Class for describing declaration nodes.
   */
  class declaration_node: public cdk::typed_node {
    int _qualifier;
    std::string _identifier;
    cdk::expression_node* _rvalue;

  public:
    inline declaration_node(int lineno, int qualifier, std::string& identifier, cdk::expression_node* rvalue, std::shared_ptr<cdk::basic_type> var_type) :
        cdk::typed_node(lineno), _qualifier(qualifier), _identifier(identifier), _rvalue(rvalue) {
        type(var_type);
    }

    inline int qualifier() {
        return _qualifier;
    }

    inline std::string& identifier() {
        return _identifier;
    }

    inline cdk::expression_node* rvalue() {
        return _rvalue;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_declaration_node(this, level);
    }

  };

} // mml

#endif
