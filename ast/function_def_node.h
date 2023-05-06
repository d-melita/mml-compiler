#ifndef __MML_AST_FUNCTION_DEF_NODE_H__
#define __MML_AST_FUNCTION_DEF_NODE_H__

#include <cdk/ast/expression_node.h>

namespace mml {

  /**
   * Class for describing function definition nodes.
   */
  class function_def_node: public cdk::expression_node {
    cdk::sequence_node* _arguments;
    std::shared_ptr<cdk::basic_type>* _returnType;
    mml::block_node* _block;

  public:
    /* Function with arguments */
    inline function_def_node(int lineno, cdk::sequence_node* arguments, std::shared_ptr<cdk::basic_type>* returnType, mml::block_node* block) :
        cdk::expression_node(lineno), _arguments(arguments), _returnType(returnType), _block(block) {
    }

    inline cdk::sequence_node* arguments() {
        return _arguments;
    }

    inline std::shared_ptr<cdk::basic_type>* returnType() {
        return _returnType;
    }

    inline mml::block_node* block() {
        return _block;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_def_node(this, level);
    }

  };

} // mml

#endif
