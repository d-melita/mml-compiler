#ifndef __MML_AST_FUNCTION_DEF_NODE_H__
#define __MML_AST_FUNCTION_DEF_NODE_H__

#include <cdk/ast/typed_node.h>
#include <cdk/types/functional_type.h>

namespace mml {

  /**
   * Class for describing function definition nodes.
   */
  class function_def_node: public cdk::typed_node {
    cdk::sequence_node* _arguments;
    std::shared_ptr<cdk::basic_type>* _return_type;
    mml::block_node* _block;
    bool _is_main;

  public:
    /* Function with arguments */
    inline function_def_node(int lineno, cdk::sequence_node* arguments, std::shared_ptr<cdk::basic_type>* return_type, mml::block_node* block, bool is_main = false) :
            cdk::typed_node(lineno), _arguments(arguments), _return_type(return_type), _block(block), _is_main(is_main) {

      // Create a functional type
      std::vector<std::shared_ptr<cdk::basic_type>> input_types;
      for (auto basic_node : _arguments->nodes()) {
        auto typed_node = dynamic_cast<cdk::typed_node*>(basic_node);
        input_types.push_back(typed_node->type());
      }

      type(cdk::functional_type::create(input_types, *_return_type));
    }

    inline cdk::sequence_node* arguments() {
        return _arguments;
    }

    inline std::shared_ptr<cdk::basic_type>* returnType() {
        return _return_type;
    }

    inline mml::block_node* block() {
        return _block;
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
