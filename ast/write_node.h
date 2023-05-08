#ifndef __MML_AST_WRITE_NODE_H__
#define __MML_AST_WRITE_NODE_H__

#include <cdk/ast/sequence_node.h>

namespace mml {

  /**
   * Class for describing write nodes.
   */
  class write_node: public cdk::basic_node {
    cdk::sequence_node* _expressions;
    bool _hasNewline;

  public:

    inline write_node(int lineno, cdk::sequence_node* expressions, bool hasNewline = false) :
        cdk::basic_node(lineno), _expressions(expressions), _hasNewline(hasNewline) {
    }

    inline cdk::sequence_node* expressions() {
      return _expressions;
    }

    inline bool hasNewline() {
      return _hasNewline;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_write_node(this, level);
    }

  };

} // mml

#endif
