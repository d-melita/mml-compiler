#ifndef __MML_AST_SIZEOF_NODE_H__
#define __MML_AST_SIZEOF_NODE_H__

#include <cdk/ast/expression_node.h>

namespace mml {

  /**
   * Class for describing next nodes.
   */
  class sizeof_node: public cdk::expression_node {
    cdk::expression_node *_argument;

  public:
    inline sizeof_node(int lineno, cdk::expression_node *argument) :
        cdk::expression_node(lineno), _argument(argument) {
    }

    inline cdk::expression_node *argument() {
      return _argument;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_sizeof_node(this, level);
    }

  };

} // mml

#endif