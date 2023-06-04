#ifndef __MML_AST_FUNCTION_DEF_NODE_H__
#define __MML_AST_FUNCTION_DEF_NODE_H__

#include <cdk/ast/typed_node.h>
#include <cdk/types/functional_type.h>

namespace mml {

  /**
   * Class for describing function definition nodes.
   */
  class function_def_node: public cdk::expression_node {
    cdk::sequence_node* _arguments;
    mml::block_node* _block;
    std::shared_ptr<cdk::basic_type> _return_type;
    bool _is_main;

  public:

    /* non-main function definition */
    inline function_def_node(int lineno, cdk::sequence_node* arguments, std::shared_ptr<cdk::basic_type> return_type, mml::block_node* block) :
            cdk::expression_node(lineno), _arguments(arguments), _block(block), _is_main(false), _return_type(return_type) {

      // Create a functional type
      std::vector<std::shared_ptr<cdk::basic_type>> input_types;
      for (auto basic_node : _arguments->nodes()) {
        auto typed_node = dynamic_cast<cdk::typed_node*>(basic_node);
        input_types.push_back(typed_node->type());
      }

      type(cdk::functional_type::create(input_types, return_type));
    }

    /* Main function definition */
    inline function_def_node(int lineno, mml::block_node* block) :
            cdk::expression_node(lineno), _arguments(nullptr), _block(block), _is_main(true), _return_type(cdk::primitive_type::create(4, cdk::TYPE_INT)) {

      type(cdk::functional_type::create(
              cdk::primitive_type::create(4, cdk::TYPE_INT) // Main function is of type int
      ));
    }

    inline cdk::sequence_node* arguments() {
        return _arguments;
    }

    inline cdk::typed_node* getArgument(size_t i) {
      return dynamic_cast<cdk::typed_node*>(_arguments->node(i));
    }

    inline mml::block_node* block() {
        return _block;
    }

    inline std::shared_ptr<cdk::basic_type> get_return_type() {
        return _return_type;
    }

    inline bool is_main() {
        return _is_main;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_def_node(this, level);
    }

  };

} // mml

#endif
