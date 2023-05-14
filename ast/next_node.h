#ifndef __MML_AST_NEXT_NODE_H__
#define __MML_AST_NEXT_NODE_H__

#include <cdk/ast/basic_node.h>

namespace mml {

  /**
   * Class for describing next nodes.
   */
  class next_node: public cdk::basic_node {
    int _argument;

  public:
    inline next_node(int lineno, int argument) :
        cdk::basic_node(lineno), _argument(argument) {
    }

    inline int argument() {
      return _argument;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_next_node(this, level);
    }

  };

} // mml

#endif
