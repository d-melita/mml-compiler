#ifndef __MML_TARGETS_TYPE_CHECKER_H__
#define __MML_TARGETS_TYPE_CHECKER_H__

#define ERR_NOT_INT_TYPE "Error: the condition must be of type int"

#include "targets/basic_ast_visitor.h"

namespace mml {

  class type_checker: public basic_ast_visitor {
    cdk::symbol_table<mml::symbol>& _symtab;
    basic_ast_visitor* _parent;

    public:
      type_checker(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<mml::symbol>& symtab, basic_ast_visitor *parent) :
          basic_ast_visitor(compiler), _symtab(symtab), _parent(parent) {
      }

    public:
      ~type_checker() {
      os().flush();
    }

    protected:
      // Auxiliary methods for type checking
      void processUnaryExpression(cdk::unary_operation_node* const node, int lvl);
      void processBinaryExpression(cdk::binary_operation_node* const node, int lvl);

      void do_BooleanLogicalExpression(cdk::binary_operation_node* const node, int lvl);
      void do_PIDExpression(cdk::binary_operation_node* const node, int lvl);
      void do_IDExpression(cdk::binary_operation_node* const node, int lvl);
      void do_IntegerOnlyExpression(cdk::binary_operation_node* const node, int lvl);
      void do_ScalarLogicalExpression(cdk::binary_operation_node* const node, int lvl);
      void do_GeneralLogicalExpression(cdk::binary_operation_node* const node, int lvl);
      template<typename T>
      void process_literal(cdk::literal_node<T>* const node, int lvl) {
      }

    public:
      // do not edit these lines
      #define __IN_VISITOR_HEADER__
      #include ".auto/visitor_decls.h" // automatically generated
      #undef __IN_VISITOR_HEADER__
      // do not edit these lines: end

  };
  
  //---------------------------------------------------------------------------
  //     HELPER MACRO FOR TYPE CHECKING
  //---------------------------------------------------------------------------

  #define CHECK_TYPES(compiler, symtab, node) { \
    try { \
      mml::type_checker checker(compiler, symtab, this); \
      (node)->accept(&checker, 0); \
    } \
    catch (const std::string &problem) { \
      std::cerr << (node)->lineno() << ": " << problem << std::endl; \
      return; \
    } \
  }

  #define ASSERT_SAFE_EXPRESSIONS CHECK_TYPES(_compiler, _symtab, node)     

} // mml

#endif
