#include <string>
#include <sstream>
#include "targets/type_checker.h"
#include "targets/postfix_writer.h"
#include "targets/frame_size_calculator.h"
#include ".auto/all_nodes.h"  // all_nodes.h is automatically generated
#include "targets/symbol.h"
#include <mml_parser.tab.h>

//---------------------------------------------------------------------------

void mml::postfix_writer::do_nil_node(cdk::nil_node * const node, int lvl) {
  // EMPTY
}
void mml::postfix_writer::do_data_node(cdk::data_node * const node, int lvl) {
  // EMPTY
}

void mml::postfix_writer::do_not_node(cdk::not_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
  _pf.INT(0);
  _pf.EQ();
}
void mml::postfix_writer::do_and_node(cdk::and_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  std::string lbl = mklbl(++_lbl);
  node->left()->accept(this, lvl);
  _pf.DUP32();
  _pf.JZ(lbl);
  node->right()->accept(this, lvl);
  _pf.AND();
  _pf.ALIGN();
  _pf.LABEL(lbl);
}
void mml::postfix_writer::do_or_node(cdk::or_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  std::string lbl = mklbl(++_lbl);
  node->left()->accept(this, lvl);
  _pf.DUP32();
  _pf.JNZ(lbl);
  node->right()->accept(this, lvl);
  _pf.OR();
  _pf.ALIGN();
  _pf.LABEL(lbl);
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_sequence_node(cdk::sequence_node * const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_integer_node(cdk::integer_node * const node, int lvl) {
  if (_in_function_body) {
    _pf.INT(node->value());
  } else {
    _pf.SINT(node->value());
  }
}

void mml::postfix_writer::do_double_node(cdk::double_node * const node, int lvl) {
  std::string lbl = mklbl(++_lbl);
  if (_in_function_body) {
    _pf.CALL(lbl);
    _pf.TEXT();
    _pf.ALIGN();
    _pf.LABEL(lbl);
    _pf.START();
    _pf.DOUBLE(node->value());
    _pf.STFVAL64();
    _pf.LEAVE();
    _pf.RET();
    _pf.TEXT(_return_labels.back());
    _pf.LDFVAL64();
  } else {
    _pf.SDOUBLE(node->value());
  }
}

void mml::postfix_writer::do_string_node(cdk::string_node * const node, int lvl) {
  std::string lbl = mklbl(++_lbl);

  /* generate the string */
  _pf.RODATA(); // strings are DATA readonly
  _pf.ALIGN(); // make sure we are aligned
  _pf.LABEL(lbl); // give the string a name
  _pf.SSTRING(node->value()); // output string characters

  if (_in_function_body) {
    _pf.TEXT(_return_labels.back());
    _pf.ADDR(lbl);
  } else {
    _pf.DATA();
    _pf.SADDR(lbl);
  }
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
  node->left()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->left()->is_typed(cdk::TYPE_INT)) {
    auto referenced = cdk::reference_type::cast(node->type())->referenced();
    _pf.INT(referenced->size());
    _pf.MUL();
  }

  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    auto referenced = cdk::reference_type::cast(node->type())->referenced();
    _pf.INT(referenced->size());
    _pf.MUL();
  }
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DADD();
  } else {
    _pf.ADD();
  }
}
void mml::postfix_writer::do_sub_node(cdk::sub_node * const node, int lvl) {
  node->left()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->left()->is_typed(cdk::TYPE_INT)) {
    auto referenced = cdk::reference_type::cast(node->type())->referenced();
    _pf.INT(referenced->size());
    _pf.MUL();
  }

  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    auto referenced = cdk::reference_type::cast(node->type())->referenced();
    _pf.INT(referenced->size());
    _pf.MUL();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DSUB();
  } else {
    _pf.SUB();
  }
}
void mml::postfix_writer::do_mul_node(cdk::mul_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DMUL();
  } else {
    _pf.MUL();
  }
}
void mml::postfix_writer::do_div_node(cdk::div_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DDIV();
  } else {
    _pf.DIV();
  }
}
void mml::postfix_writer::do_mod_node(cdk::mod_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);
  _pf.MOD();
}
void mml::postfix_writer::do_lt_node(cdk::lt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl + 2);
  if (node->right()->is_typed(cdk::TYPE_INT) && node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  _pf.LT();
}
void mml::postfix_writer::do_le_node(cdk::le_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl + 2);
  if (node->right()->is_typed(cdk::TYPE_INT) && node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  _pf.LE();
}
void mml::postfix_writer::do_ge_node(cdk::ge_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl + 2);
  if (node->right()->is_typed(cdk::TYPE_INT) && node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
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
  node->left()->accept(this, lvl + 2);
  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl + 2);
  if (node->right()->is_typed(cdk::TYPE_INT) && node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  _pf.NE();
}
void mml::postfix_writer::do_eq_node(cdk::eq_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl + 2);
  if (node->right()->is_typed(cdk::TYPE_INT) && node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
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

    ASSERT_SAFE_EXPRESSIONS;

    std::cout << "[* Debug] Entered do_block_node" << std::endl;
    _symtab.push();
    if (node->declarations()) {
      std::cout << "[* Debug] Accepting declarations" << std::endl;
      node->declarations()->accept(this, lvl + 2);
    }
    if (node->instructions()) {
      std::cout << "[* Debug] Accepting instructions" << std::endl;
      node->instructions()->accept(this, lvl + 2);
    }
    _symtab.pop();
    std::cout << "[* Debug] Leaving do_block_node" << std::endl;
}

void mml::postfix_writer::do_declaration_node(mml::declaration_node * const node, int lvl) {
}

void mml::postfix_writer::do_function_call_node(mml::function_call_node *const node, int lvl) {
}



void mml::postfix_writer::do_function_def_node(mml::function_def_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  std::cout << "[* Debug] Entered do_function_def_node" << std::endl;  
  if (node->is_main()) {

    for (std::string name : _not_declared_symbols) {
      auto symbol = _symtab.find(name, 0);
      if (symbol->is_foreign()) {
        _external_functions.insert(name);
      } else {
        _pf.BSS();
        _pf.ALIGN();
        _pf.LABEL(name);
        _pf.SALLOC(symbol->type()->size());    
      }
    }
    
    auto int_func_type = cdk::functional_type::create(cdk::primitive_type::create(4, cdk::TYPE_INT));
    auto main = create_symbol(int_func_type, "_main", 0, tPRIVATE);
    main->set_main(true);
    auto mainat = mml::create_symbol(int_func_type, "@", 0, tPRIVATE);
    mainat->set_main(true);

    _function_symbols.push_back(main);
    _return_labels.push_back("_main");
    
    _symtab.push(); // _level++; new context;
    _pf.TEXT(_return_labels.back());
    _pf.ALIGN();
    _pf.GLOBAL("_main", _pf.FUNC());
    _pf.LABEL("_main");

    frame_size_calculator lsc(_compiler, _symtab, main);

    // trick to render all inserted symbols during this visit useless
    _symtab.push();
    node->accept(&lsc, lvl);
    _symtab.pop();
  
    _pf.ENTER(lsc.localsize());

    bool _prev = _return_seen;
    _return_seen = false;
    _in_function_body = true;
    if (node->block()) {
      node->block()->accept(this, lvl + 2);
    }
    _in_function_body = false;

    _symtab.pop();
    
    _return_labels.pop_back();
    if (!_return_seen) {
      _pf.INT(0);
      _pf.STFVAL32();
      _pf.LEAVE();
      _pf.RET();
    }

    _function_symbols.pop_back();
    for (std::string ext : _external_functions) {
      _pf.EXTERN(ext);
    }
    _external_functions.clear();
    _return_seen = _prev;
    
    std::cout << "[* Debug] Finished `is_main`" << std::endl;
    return;
  } else {
  
    _curr_function = new_symbol();
    bool is_public = false;
    std::string func_name;

    if (_curr_function) {
      _function_symbols.push_back(_curr_function);
      reset_new_symbol();
    }

    // prepare for arguments (old_fp & old_ret, 4 bytes each)
    _offset = 8;    
    _symtab.push(); 

    if (node->arguments()) {

      _in_function_args = true;

      for (size_t i = 0; i < node->arguments()->size(); i++) {
        cdk::basic_node *arg = node->arguments()->node(i);
        if (!arg) {
          break;
        }
        arg->accept(this, 0);
      }

      _in_function_args = false;
    }

    _function_label = mklbl(++_lbl);
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

    _symtab.pop();

    if (!_return_seen) {
      _pf.LEAVE();
      _pf.RET();
    }

    _return_labels.pop_back();
    if (_curr_function) _function_symbols.pop_back();

    if (_in_function_body) {
      _pf.TEXT(_return_labels.back());
      _pf.ADDR(_function_label);
    }
    std::cout << "[* Debug] Finished not-main definition node" << std::endl;
  }
}

void mml::postfix_writer::do_identity_node(mml::identity_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl + 2);
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
  ASSERT_SAFE_EXPRESSIONS;
  std::cout << "[* Debug] Started do_return_node" << std::endl;
  _return_seen = true;
  auto current_function = _function_symbols.back();
  std::shared_ptr<cdk::basic_type> outputType = cdk::functional_type::cast(current_function->type())->output(0);
  if (outputType->name() != cdk::TYPE_VOID) {
    node->retval()->accept(this, lvl + 2);
    if (outputType->name() == cdk::TYPE_INT || outputType->name() == cdk::TYPE_STRING 
    || outputType->name() == cdk::TYPE_POINTER || outputType->name() == cdk::TYPE_FUNCTIONAL) {
      _pf.STFVAL32();
    } else if (outputType->name() == cdk::TYPE_DOUBLE) {
      if (node->retval()->type()->name() == cdk::TYPE_INT) _pf.I2D();
        _pf.STFVAL64();
    } else {
      std::cerr << node->lineno() << " ERROR: Unknown return type" << std::endl;
    }
  }
  _pf.LEAVE();
  _pf.RET();
  std::cout << "[* Debug] Finished do_return_node" << std::endl;
}

void mml::postfix_writer::do_sizeof_node(mml::sizeof_node *const node, int lvl) {
}

void mml::postfix_writer::do_stack_alloc_node(mml::stack_alloc_node *const node, int lvl) {
}

void mml::postfix_writer::do_stop_node(mml::stop_node *const node, int lvl) {
}

void mml::postfix_writer::do_write_node(mml::write_node *const node, int lvl) {
  
  std::cout << "[* Debug] Started do_write_node" << std::endl;
  
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
  
  std::cout << "[* Debug] Finished do_write_node" << std::endl;
}
