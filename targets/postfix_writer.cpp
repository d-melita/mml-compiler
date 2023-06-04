#include <string>
#include <sstream>
#include "targets/type_checker.h"
#include "targets/postfix_writer.h"
#include "targets/frame_size_calculator.h"
#include ".auto/all_nodes.h"  // all_nodes.h is automatically generated

//---------------------------------------------------------------------------

void mml::postfix_writer::do_nil_node(cdk::nil_node * const node, int lvl) {
  // EMPTY
}
void mml::postfix_writer::do_data_node(cdk::data_node * const node, int lvl) {
  // EMPTY
}
void mml::postfix_writer::do_double_node(cdk::double_node * const node, int lvl) {
  // EMPTY
}
void mml::postfix_writer::do_not_node(cdk::not_node * const node, int lvl) {
  // EMPTY
}
void mml::postfix_writer::do_and_node(cdk::and_node * const node, int lvl) {
  // EMPTY
}
void mml::postfix_writer::do_or_node(cdk::or_node * const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_sequence_node(cdk::sequence_node * const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_integer_node(cdk::integer_node * const node, int lvl) {
  _pf.INT(node->value()); // push an integer
}

void mml::postfix_writer::do_string_node(cdk::string_node * const node, int lvl) {
  int lbl1;

  /* generate the string */
  _pf.RODATA(); // strings are DATA readonly
  _pf.ALIGN(); // make sure we are aligned
  _pf.LABEL(mklbl(lbl1 = ++_lbl)); // give the string a name
  _pf.SSTRING(node->value()); // output string characters

  /* leave the address on the stack */
  _pf.TEXT(); // return to the TEXT segment
  _pf.ADDR(mklbl(lbl1)); // the string to be printed
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_neg_node(cdk::neg_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
  _pf.NEG(); // 2-complement
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_add_node(cdk::add_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.ADD();
}
void mml::postfix_writer::do_sub_node(cdk::sub_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.SUB();
}
void mml::postfix_writer::do_mul_node(cdk::mul_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.MUL();
}
void mml::postfix_writer::do_div_node(cdk::div_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.DIV();
}
void mml::postfix_writer::do_mod_node(cdk::mod_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.MOD();
}
void mml::postfix_writer::do_lt_node(cdk::lt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.LT();
}
void mml::postfix_writer::do_le_node(cdk::le_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.LE();
}
void mml::postfix_writer::do_ge_node(cdk::ge_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.GE();
}
void mml::postfix_writer::do_gt_node(cdk::gt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.GT();
}
void mml::postfix_writer::do_ne_node(cdk::ne_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.NE();
}
void mml::postfix_writer::do_eq_node(cdk::eq_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.EQ();
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_variable_node(cdk::variable_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  // simplified generation: all variables are global
  _pf.ADDR(node->name());
}

void mml::postfix_writer::do_rvalue_node(cdk::rvalue_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl);
  _pf.LDINT(); // depends on type size
}

void mml::postfix_writer::do_assignment_node(cdk::assignment_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->rvalue()->accept(this, lvl); // determine the new value
  _pf.DUP32();
  if (new_symbol() == nullptr) {
    node->lvalue()->accept(this, lvl); // where to store the value
  } else {
    _pf.DATA(); // variables are all global and live in DATA
    _pf.ALIGN(); // make sure we are aligned
    _pf.LABEL(new_symbol()->name()); // name variable location
    reset_new_symbol();
    _pf.SINT(0); // initialize it to 0 (zero)
    _pf.TEXT(); // return to the TEXT segment
    node->lvalue()->accept(this, lvl);  //DAVID: bah!
  }
  _pf.STINT(); // store the value at address
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_evaluation_node(mml::evaluation_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
  if (node->argument()->is_typed(cdk::TYPE_INT)) {
    _pf.TRASH(4); // delete the evaluated value
  } else if (node->argument()->is_typed(cdk::TYPE_STRING)) {
    _pf.TRASH(4); // delete the evaluated value's address
  } else {
    std::cerr << "ERROR: CANNOT HAPPEN!" << std::endl;
    exit(1);
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_while_node(mml::while_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1, lbl2;
  _pf.LABEL(mklbl(lbl1 = ++_lbl));
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl2 = ++_lbl));
  node->block()->accept(this, lvl + 2);
  _pf.JMP(mklbl(lbl1));
  _pf.LABEL(mklbl(lbl2));
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_if_node(mml::if_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->block()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1));
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_if_else_node(mml::if_else_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1, lbl2;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->thenblock()->accept(this, lvl + 2);
  _pf.JMP(mklbl(lbl2 = ++_lbl));
  _pf.LABEL(mklbl(lbl1));
  node->elseblock()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1 = lbl2));
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_address_of_node(mml::address_of_node *const node, int lvl) {
}

void mml::postfix_writer::do_block_node(mml::block_node * const node, int lvl) {
}

void mml::postfix_writer::do_declaration_node(mml::declaration_node * const node, int lvl) {
}

void mml::postfix_writer::do_function_call_node(mml::function_call_node *const node, int lvl) {
}

void mml::postfix_writer::do_function_def_node(mml::function_def_node *const node, int lvl) {
  
  ASSERT_SAFE_EXPRESSIONS;
  
  _curr_function = new_symbol();
  
  std::string func_name = _curr_function->name();
  bool is_public = false;
  
  if (_curr_function) {
    _function_symbols.push_back(_curr_function);
    reset_new_symbol();
  }
  
  // prepare for arguments (old_fp & old_ret, 4 bytes each)
  _offset = 8;    
  _symtab.push(); 
  
  if (node->arguments()->size() > 0) {
    
    _in_function_args = true;
  
    for (size_t i = 0; i < node->arguments()->size(); i++) {
      cdk::basic_node *arg = node->arguments()->node(i);
      if (arg == nullptr) {
        break;
      }
      arg->accept(this, 0);
    }
    
    _in_function_args = false;
  }
  
  std::string _function_label = mklbl(++_lbl);
  _return_labels.push_back(_function_label);
  _pf.TEXT(_return_labels.back());
  _pf.ALIGN();
  if (is_public) {
    _pf.GLOBAL(func_name, _pf.FUNC());
  }
  _pf.LABEL(_function_label);
  
  frame_size_calculator lsc(_compiler, _symtab, _curr_function);
  
  _symtab.push();
  node->accept(&lsc, lvl);
  _symtab.pop();
  
  _pf.ENTER(lsc.localsize());
  
  // prepare for local vars
  _offset = 0;
  
  bool _prev = _in_function_body;
  _in_function_body = true;
  if (node->block()) {
    node->block()->accept(this, lvl + 4);
  }
  _in_function_body = _prev;
  
  if (_in_function_body) {
    _pf.TEXT(_return_labels.back());
    _pf.ADDR(_function_label);
  } 
}

void mml::postfix_writer::do_identity_node(mml::identity_node *const node, int lvl) {
}

void mml::postfix_writer::do_index_node(mml::index_node *const node, int lvl) {
}

void mml::postfix_writer::do_input_node(mml::input_node *const node, int lvl) {
}

void mml::postfix_writer::do_next_node(mml::next_node *const node, int lvl) {
}

void mml::postfix_writer::do_nullptr_node(mml::nullptr_node *const node, int lvl) {
}

void mml::postfix_writer::do_return_node(mml::return_node *const node, int lvl) {
}

void mml::postfix_writer::do_sizeof_node(mml::sizeof_node *const node, int lvl) {
}

void mml::postfix_writer::do_stack_alloc_node(mml::stack_alloc_node *const node, int lvl) {
}

void mml::postfix_writer::do_stop_node(mml::stop_node *const node, int lvl) {
}

void mml::postfix_writer::do_write_node(mml::write_node *const node, int lvl) {
  
  ASSERT_SAFE_EXPRESSIONS;

  for (size_t ix = 0; ix < node->expressions()->size(); ix++) {
    auto child = dynamic_cast<cdk::expression_node*>(node->expressions()->node(ix));

    std::shared_ptr<cdk::basic_type> type = child->type();
    child->accept(this, lvl);

    if (type->name() == cdk::TYPE_INT) {
      _external_functions.insert("printi");
      _pf.CALL("printi");
      _pf.TRASH(4);
    } else if (type->name() == cdk::TYPE_STRING) {
      _external_functions.insert("prints");
      _pf.CALL("prints");
      _pf.TRASH(4);
    } else if (type->name() == cdk::TYPE_DOUBLE) {
      _external_functions.insert("printd");
      _pf.CALL("printd");
      _pf.TRASH(8);
    } else {
      std::cerr << "ERROR: cannot print expression of unknown type" << std::endl;
      return;
    }
    if (node->has_newline()) {
      _external_functions.insert("println");
      _pf.CALL("println");
    }
  }
}
