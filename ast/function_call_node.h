#ifndef __MML_AST_FUNCTION_CALL_NODE_H__
#define __MML_AST_FUNCTION_CALL_NODE_H__

#include <string>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>

namespace mml {

  /**
   * Class for describing function call nodes.
   */
  class function_call_node: public cdk::expression_node {
    cdk::expression_node* _identifier;
    cdk::sequence_node* _arguments;

  public:

    /* Constructor for a function without arguments */
    inline function_call_node(int lineno, cdk::expression_node* identifier) :
            cdk::expression_node(lineno), _identifier(identifier), _arguments(nullptr) {
    }

    /* Constructor for a function with arguments */
    inline function_call_node(int lineno, cdk::expression_node* identifier, cdk::sequence_node* arguments) :
        cdk::expression_node(lineno), _identifier(identifier), _arguments(arguments) {
    }

    inline cdk::expression_node* identifier() {
        return _identifier;
    }

    inline cdk::sequence_node* arguments() {
        return _arguments;
    }

    inline cdk::expression_node* getArgument(size_t i) {
        return dynamic_cast<cdk::expression_node*>(_arguments->node(i));
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_call_node(this, level);
    }

  };

} // mml

#endif
