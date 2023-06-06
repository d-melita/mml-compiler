#include <string>
#include "targets/type_checker.h"
#include ".auto/all_nodes.h"  // automatically generated
#include <cdk/types/primitive_type.h>
#include <mml_parser.tab.h>

#define ASSERT_UNSPEC { if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC)) return; }

#define DEBUG 1
void debug(std::string str) {
  if (DEBUG) std::cout << "[*] (Debug) " << str << std::endl;
}


void mml::type_checker::do_BooleanLogicalExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if (!node->left()->is_typed(cdk::TYPE_INT)) {
    throw std::string("Integer expression expected in binary expression");
  }
  node->right()->accept(this, lvl + 2);
  if (!node->right()->is_typed(cdk::TYPE_INT)) {
    throw std::string("Integer expression expected in binary expression");
  }
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

//---------------------------------------------------------------------------

void mml::type_checker::do_sequence_node(cdk::sequence_node *const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl + 2);
  }
}

//---------------------------------------------------------------------------

void mml::type_checker::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}
void mml::type_checker::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}

void mml::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (!node->argument()->is_typed(cdk::TYPE_INT)) {
    throw std::string("wrong type in argument of unary expression");
  }
  node->type(node->argument()->type());
}
void mml::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
  do_BooleanLogicalExpression(node, lvl);
}
void mml::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
  do_BooleanLogicalExpression(node, lvl);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_integer_node(cdk::integer_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
}

void mml::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}

//---------------------------------------------------------------------------

void mml::type_checker::processUnaryExpression(cdk::unary_operation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
  if (!(node->argument()->is_typed(cdk::TYPE_INT) || node->argument()->is_typed(cdk::TYPE_DOUBLE))) {
    throw std::string("wrong type in argument of unary expression");
  }
  // in MML, expressions are always int
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_neg_node(cdk::neg_node *const node, int lvl) {
  processUnaryExpression(node, lvl);
}

//---------------------------------------------------------------------------

void mml::type_checker::processBinaryExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if (!node->left()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in left argument of binary expression");

  node->right()->accept(this, lvl + 2);
  if (!node->right()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in right argument of binary expression");

  // in MML, expressions are always int
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_PIDExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(node->left()->type());
  } else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_POINTER)) {
    node->type(node->right()->type());
  } else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (node->left()->is_typed(cdk::TYPE_UNSPEC) && node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else {
    throw std::string("wrong types in arguments of binary expression");
  }
}

void mml::type_checker::do_IDExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (node->left()->is_typed(cdk::TYPE_UNSPEC) && node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else {
    throw std::string("wrong types in arguments of binary expression");
  }
}

void mml::type_checker::do_IntegerOnlyExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if (!node->left()->is_typed(cdk::TYPE_INT)) {
    throw std::string("expected integer type in left argument of binary expression");
  }

  node->right()->accept(this, lvl + 2);
  if (!node->right()->is_typed(cdk::TYPE_INT)) {
    throw std::string("expected integer type in right argument of binary expression");
  }

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_ScalarLogicalExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);

  if (!(node->left()->is_typed(cdk::TYPE_INT) || node->left()->is_typed(cdk::TYPE_DOUBLE))) {
    throw std::string("expected integer or double type in left argument of binary expression");
  }

  node->right()->accept(this, lvl + 2);

  if (!(node->right()->is_typed(cdk::TYPE_INT) || node->right()->is_typed(cdk::TYPE_DOUBLE))) {
    throw std::string("expected integer or double type in right argument of binary expression");
  }

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_GeneralLogicalExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (node->left()->type() != node->right()->type()) {
    throw std::string("expected same type in both arguments of binary expression");
  }

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  do_PIDExpression(node, lvl);
}
void mml::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
  do_PIDExpression(node, lvl);
}
void mml::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
  do_IDExpression(node, lvl);
}
void mml::type_checker::do_div_node(cdk::div_node *const node, int lvl) {
  do_IDExpression(node, lvl);
}
void mml::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  do_IntegerOnlyExpression(node, lvl);
}
void mml::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  do_ScalarLogicalExpression(node, lvl);
}
void mml::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
  do_ScalarLogicalExpression(node, lvl);
}
void mml::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  do_ScalarLogicalExpression(node, lvl);
}
void mml::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  do_ScalarLogicalExpression(node, lvl);
}
void mml::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  do_GeneralLogicalExpression(node, lvl);
}
void mml::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  do_GeneralLogicalExpression(node, lvl);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_variable_node(cdk::variable_node *const node, int lvl) {
  ASSERT_UNSPEC;
  const std::string &id = node->name();
  std::shared_ptr<mml::symbol> symbol = _symtab.find(id);

  if (symbol != nullptr) {
    node->type(symbol->type());
  } else {
    throw id;
  }
}

void mml::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_UNSPEC;
  try {
    node->lvalue()->accept(this, lvl);
    node->type(node->lvalue()->type());
  } catch (const std::string &id) {
    throw "undeclared variable '" + id + "'";
  }
}


static bool check_pointed_types_compatibility(std::shared_ptr<cdk::basic_type> lvalue_type, std::shared_ptr<cdk::basic_type> rvalue_type) {
  
  while (rvalue_type != nullptr && lvalue_type->name() == cdk::TYPE_POINTER && rvalue_type->name() == cdk::TYPE_POINTER) {
    lvalue_type = cdk::reference_type::cast(lvalue_type)->referenced();
    rvalue_type = cdk::reference_type::cast(rvalue_type)->referenced();
  }
  
  return rvalue_type == nullptr || lvalue_type->name() != rvalue_type->name();
}


bool check_function_types_compatibility(std::shared_ptr<cdk::functional_type> lvalue_type, std::shared_ptr<cdk::functional_type> rvalue_type) {

  if (lvalue_type->output(0)->name() == cdk::TYPE_POINTER && 
      (rvalue_type->output(0)->name() != cdk::TYPE_POINTER || 
       !check_pointed_types_compatibility(lvalue_type->output(0), rvalue_type->output(0)))) {
    return false;
  }
  else if (lvalue_type->output(0)->name() == cdk::TYPE_DOUBLE && 
           (rvalue_type->output(0)->name() != cdk::TYPE_INT && 
            rvalue_type->output(0)->name() != cdk::TYPE_DOUBLE)) {
    return false;
  }
  else if (lvalue_type->output(0)->name() == cdk::TYPE_FUNCTIONAL && 
           (rvalue_type->output(0)->name() != cdk::TYPE_FUNCTIONAL || 
            !check_function_types_compatibility(cdk::functional_type::cast(lvalue_type->output(0)), 
                                                cdk::functional_type::cast(rvalue_type->output(0))))) {
    return false;
  }
  else if (lvalue_type->output(0)->name() != rvalue_type->output(0)->name()) {
    return false;
  }
  else if (lvalue_type->input_length() != rvalue_type->input_length()) {
    return false;
  }
  
  for (size_t i = 0; i < lvalue_type->input_length(); i++) {
    if (rvalue_type->input(i)->name() == cdk::TYPE_POINTER && 
        (rvalue_type->input(i)->name() != cdk::TYPE_POINTER || 
         !check_pointed_types_compatibility(lvalue_type->input(i), rvalue_type->input(i)))) {
        return false;
    } 
    else if (lvalue_type->input(i)->name() == cdk::TYPE_FUNCTIONAL && 
             (rvalue_type->input(i)->name() != cdk::TYPE_FUNCTIONAL || 
              check_function_types_compatibility(cdk::functional_type::cast(lvalue_type->input(i)), cdk::functional_type::cast(rvalue_type->input(i))))) {
        return false;
    } 
    else if (rvalue_type->input(i)->name() == cdk::TYPE_DOUBLE &&
             (lvalue_type->input(i)->name() != cdk::TYPE_INT && 
              lvalue_type->input(i)->name() != cdk::TYPE_DOUBLE)) {
        return false;
    } 
    else if ((lvalue_type->input(i)->name() != rvalue_type->input(i)->name())) {
      return false;
    }
  }
  
  return true;
}


void mml::type_checker::do_assignment_node(cdk::assignment_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->lvalue()->accept(this, lvl + 4);
  node->rvalue()->accept(this, lvl + 4);

  if (node->lvalue()->is_typed(cdk::TYPE_INT)){
    
    if (node->rvalue()->is_typed(cdk::TYPE_INT)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    }
    else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    }
    else {
      throw std::string("wrong assignment to integer");
    }
  }
  
  else if (node->lvalue()->is_typed(cdk::TYPE_DOUBLE)){
    
    if (node->rvalue()->is_typed(cdk::TYPE_DOUBLE) || node->rvalue()->is_typed(cdk::TYPE_INT)) {
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    } 
    else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
      node->rvalue()->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    } 
    else {
      throw std::string("wrong assignment to real");
    }
  }
  
  else if (node->lvalue()->is_typed(cdk::TYPE_STRING)){
    
    if (node->rvalue()->is_typed(cdk::TYPE_STRING)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
    } 
    else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
      node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
    } 
    else {
      throw std::string("wrong assignment to string");
    }
  } 
  
  else if (node->lvalue()->is_typed(cdk::TYPE_VOID)){
    // TODO should be done?
  }
  
  else if (node->lvalue()->is_typed(cdk::TYPE_POINTER)){
    
    if (node->rvalue()->is_typed(cdk::TYPE_POINTER)) {
      
      // Peels layers of `[[...]]` until it reaches the last layer
      auto lvalue_type = node->lvalue()->type();
      auto rvalue_type = node->rvalue()->type(); 
      if (!check_pointed_types_compatibility(lvalue_type, rvalue_type)) {
        throw std::string("wrong number of pointer depth assignment / type to pointer");
      }
      node->type(node->rvalue()->type()); 
    } 
    // TODO: not sure on this one
    else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {             
      node->type(cdk::primitive_type::create(4, cdk::TYPE_ERROR));
      node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_ERROR));
    } 
    else {
      throw std::string("wrong assignment to pointer");
    }
  }
  
  else if (node->lvalue()->is_typed(cdk::TYPE_FUNCTIONAL)){
    
    if (node->rvalue()->is_typed(cdk::TYPE_FUNCTIONAL)) {
        if (!(check_function_types_compatibility(cdk::functional_type::cast(node->lvalue()->type()), cdk::functional_type::cast(node->rvalue()->type()))
             || (node->rvalue()->is_typed(cdk::TYPE_POINTER) && 
                 cdk::reference_type::cast(node->rvalue()->type())->referenced() == nullptr))) {
          
          throw std::string("wrong type for function initializer");
        }
      node->type(node->rvalue()->type());
   
    } 
    // TODO: not sure on this one
    else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {              
      node->type(cdk::primitive_type::create(4, cdk::TYPE_ERROR));
      node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_ERROR));
    } 
    else {
      throw std::string("wrong assignment to function");
    }
  }
  
  else {
    throw std::string("wrong types in assignment");
  }
}

//---------------------------------------------------------------------------

void mml::type_checker::do_evaluation_node(mml::evaluation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_while_node(mml::while_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (!node->condition()->is_typed(cdk::TYPE_INT)) {
    throw std::string("expected integer condition");
  }
}

//---------------------------------------------------------------------------

void mml::type_checker::do_if_node(mml::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (!node->condition()->is_typed(cdk::TYPE_INT)) {
    throw std::string("expected integer condition");
  }
}

void mml::type_checker::do_if_else_node(mml::if_else_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (!node->condition()->is_typed(cdk::TYPE_INT)) {
    throw std::string("expected integer condition");
  }
}

//---------------------------------------------------------------------------

void mml::type_checker::do_address_of_node(mml::address_of_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->lvalue()->accept(this, lvl + 2);
  node->type(cdk::reference_type::create(4, node->lvalue()->type()));
}

void mml::type_checker::do_block_node(mml::block_node * const node, int lvl) {
  // EMPTY
}

void mml::type_checker::do_declaration_node(mml::declaration_node * const node, int lvl) {
  // TODO
}

void mml::type_checker::do_function_call_node(mml::function_call_node *const node, int lvl) {

  ASSERT_UNSPEC;
  
  std::vector<std::shared_ptr<cdk::basic_type>> in_types;
  std::shared_ptr<cdk::basic_type> out_type;
  
  if (node->identifier()){
    
    node->identifier()->accept(this, lvl + 2);
    
    if (!(node->identifier()->type()->name() == cdk::TYPE_FUNCTIONAL)) {
      throw std::string("ERROR: expected a pointer to call function");
    }
    
    in_types = cdk::functional_type::cast(node->identifier()->type())->input()->components();
    out_type = cdk::functional_type::cast(node->identifier()->type())->output(0);
  }
  // Recursive call (`@`)
  else {
    auto symbol = _symtab.find("@", 1);
    
    if (symbol == nullptr) {
      throw std::string("ERROR: Recursive symbol used outside a function");
    }
    else if (symbol->is_main()) {
      throw std::string("ERROR: Recursive symbol used in main function");
    }
    
    in_types = cdk::functional_type::cast(symbol->type())->input()->components();
    out_type = cdk::functional_type::cast(symbol->type())->output(0);
  }
  
  node->type(out_type);   

  if (node->arguments()->size() == in_types.size()) {
    
    node->arguments()->accept(this, lvl + 4);
    
    for (size_t i = 0; i < node->arguments()->size(); i++) {
      if (node->getArgument(i)->type()->name() == in_types[i]->name()) continue;
      if (in_types[i]->name() == cdk::TYPE_DOUBLE && node->getArgument(i)->is_typed(cdk::TYPE_INT)) continue;
      throw std::string("ERROR: type mismatch for argument " + std::to_string(i + 1) + ".");
    }
  } 
  else {
    throw std::string(
        "ERROR: number of arguments in call (" + std::to_string(node->arguments()->size()) + ") must match declaration ("
            + std::to_string(in_types.size()) + ").");
  }
}

void mml::type_checker::do_function_def_node(mml::function_def_node *const node, int lvl) {
  ASSERT_UNSPEC
  if (node->is_main()) {
    auto mainfunc = mml::create_symbol(cdk::functional_type::create(cdk::primitive_type::create(4, cdk::TYPE_INT)), "_main", 0, tPRIVATE);
    mainfunc->set_main(true);
    auto cdkInt = cdk::primitive_type::create(4, cdk::TYPE_INT);
    std::vector<std::shared_ptr<cdk::basic_type>> input_types;
    auto mainat = mml::create_symbol(cdk::reference_type::create(4, cdkInt), "@", 0, tPUBLIC);
    mainat->set_main(true);
    if (_symtab.find_local(mainat->name())) {
      _symtab.replace(mainat->name(), mainat);
    } else {
      if (!_symtab.insert(mainat->name(), mainat)) {
        std::cerr << "ERROR: error inserting main @" << std::endl;
        return;
      }
    }
    _parent->set_new_symbol(mainfunc);
  } else {
    auto function = mml::create_symbol(node->type(), "@", 0, tPRIVATE);

    if (_symtab.find_local(function->name())) {
      _symtab.replace(function->name(), function);
    } else {
      if (!_symtab.insert(function->name(), function)) {
        std::cerr << "ERROR: error inserting function" << std::endl;
        return;
      }
    }
    _parent->set_new_symbol(function);
  }
}

void mml::type_checker::do_identity_node(mml::identity_node *const node, int lvl) {
  ASSERT_UNSPEC;
  processUnaryExpression(node, lvl);
}

void mml::type_checker::do_index_node(mml::index_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->ptr()->accept(this, lvl + 2);
  std::shared_ptr<cdk::reference_type> ref = cdk::reference_type::cast(node->ptr()->type());

  if (!node->ptr()->is_typed(cdk::TYPE_INT)) {
    throw std::string("pointer expression expected in index left value");
  }

  node->idx()->accept(this, lvl + 2);
  if (!node->idx()->is_typed(cdk::TYPE_INT)) {
    throw std::string("integer expression expected");
  }

  node->type(ref->referenced());
}

void mml::type_checker::do_input_node(mml::input_node *const node, int lvl) {
  node->type(cdk::primitive_type::create(0, cdk::TYPE_UNSPEC));
}

void mml::type_checker::do_next_node(mml::next_node *const node, int lvl) {
  // EMPTY
}

void mml::type_checker::do_nullptr_node(mml::nullptr_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::reference_type::create(4, nullptr));
}

void mml::type_checker::do_return_node(mml::return_node *const node, int lvl) {
  // TODO
}

void mml::type_checker::do_sizeof_node(mml::sizeof_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_stack_alloc_node(mml::stack_alloc_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (!node->argument()->is_typed(cdk::TYPE_INT)) {
    throw std::string("expected integer argument in stack allocation expression");
  }
  auto mytype = cdk::reference_type::create(4, cdk::primitive_type::create(4, cdk::TYPE_INT));
  node->type(mytype);
}

void mml::type_checker::do_stop_node(mml::stop_node *const node, int lvl) {
  // EMPTY
}

void mml::type_checker::do_write_node(mml::write_node *const node, int lvl) {
  node->expressions()->accept(this, lvl + 2);
}
