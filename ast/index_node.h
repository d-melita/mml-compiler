#ifndef __MML_AST_INDEX_NODE_H__
#define __MML_AST_INDEX_NODE_H__

#include <cdk/ast/lvalue_node.h>
#include <cdk/ast/expression_node.h>

namespace mml {

  /**
   * Class for describing index nodes.
   */
  class index_node: public cdk::lvalue_node {

    cdk::expression_node* _ptr;
    cdk::expression_node* _idx;

  public:

    inline index_node(int lineno, cdk::expression_node* ptr, cdk::expression_node* idx) :
    cdk::lvalue_node(lineno), _ptr(ptr), _idx(idx) {
    }

    cdk::expression_node* ptr() {
      return _ptr;
    }

    cdk::expression_node* idx() {
      return _idx;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_index_node(this, level);
    }

  };

} // mml

#endif
