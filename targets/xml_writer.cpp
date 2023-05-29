#include <string>
#include "targets/xml_writer.h"
#include "targets/type_checker.h"
#include ".auto/all_nodes.h"  // automatically generated

#include "mml_parser.tab.h"

static std::string qualifier_name(int qualifier) {
  switch (qualifier) {
    case tPUBLIC: return "public";
    case tFOREIGN: return "foreign";
    case tFORWARD: return "forward";
    case tAUTO_TYPE: return "auto";
    default: return "unknown qualifier";
  }
}

static std::string type(std::string type) {
  // replace all < with &lt;
  size_t pos = 0;
  while ((pos = type.find("<", pos)) != std::string::npos) {
    type.replace(pos, 1, "&lt;");
    pos += 4;
  }
  return type;
}

//---------------------------------------------------------------------------

void mml::xml_writer::do_nil_node(cdk::nil_node * const node, int lvl) {
  openTag(node, lvl);
  closeTag(node, lvl);
}
void mml::xml_writer::do_data_node(cdk::data_node * const node, int lvl) {
  // EMPTY
}
void mml::xml_writer::do_double_node(cdk::double_node * const node, int lvl) {
  process_literal(node, lvl);
}
void mml::xml_writer::do_not_node(cdk::not_node * const node, int lvl) {
  do_unary_operation(node, lvl);
}
void mml::xml_writer::do_and_node(cdk::and_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void mml::xml_writer::do_or_node(cdk::or_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}

//---------------------------------------------------------------------------

void mml::xml_writer::do_sequence_node(cdk::sequence_node * const node, int lvl) {
  os() << std::string(lvl, ' ') << "<sequence_node size='" << node->size() << "'>" << std::endl;
  for (size_t i = 0; i < node->size(); i++)
    node->node(i)->accept(this, lvl + 2);
  closeTag(node, lvl);
}

//---------------------------------------------------------------------------

void mml::xml_writer::do_integer_node(cdk::integer_node * const node, int lvl) {
  process_literal(node, lvl);
}

void mml::xml_writer::do_string_node(cdk::string_node * const node, int lvl) {
  process_literal(node, lvl);
}

//---------------------------------------------------------------------------

void mml::xml_writer::do_unary_operation(cdk::unary_operation_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  node->argument()->accept(this, lvl + 2);
  closeTag(node, lvl);
}

void mml::xml_writer::do_neg_node(cdk::neg_node * const node, int lvl) {
  do_unary_operation(node, lvl);
}

//---------------------------------------------------------------------------

void mml::xml_writer::do_binary_operation(cdk::binary_operation_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);
  closeTag(node, lvl);
}

void mml::xml_writer::do_add_node(cdk::add_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void mml::xml_writer::do_sub_node(cdk::sub_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void mml::xml_writer::do_mul_node(cdk::mul_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void mml::xml_writer::do_div_node(cdk::div_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void mml::xml_writer::do_mod_node(cdk::mod_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void mml::xml_writer::do_lt_node(cdk::lt_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void mml::xml_writer::do_le_node(cdk::le_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void mml::xml_writer::do_ge_node(cdk::ge_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void mml::xml_writer::do_gt_node(cdk::gt_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void mml::xml_writer::do_ne_node(cdk::ne_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}
void mml::xml_writer::do_eq_node(cdk::eq_node * const node, int lvl) {
  do_binary_operation(node, lvl);
}

//---------------------------------------------------------------------------

void mml::xml_writer::do_variable_node(cdk::variable_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  os() << std::string(lvl, ' ') << "<" << node->label() << ">" << node->name() << "</" << node->label() << ">" << std::endl;
}

void mml::xml_writer::do_rvalue_node(cdk::rvalue_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  node->lvalue()->accept(this, lvl + 4);
  closeTag(node, lvl);
}

void mml::xml_writer::do_assignment_node(cdk::assignment_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);

  node->lvalue()->accept(this, lvl);
  reset_new_symbol();

  node->rvalue()->accept(this, lvl + 4);
  closeTag(node, lvl);
}

//---------------------------------------------------------------------------

void mml::xml_writer::do_evaluation_node(mml::evaluation_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  node->argument()->accept(this, lvl + 2);
  closeTag(node, lvl);
}

//---------------------------------------------------------------------------

void mml::xml_writer::do_while_node(mml::while_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  openTag("condition", lvl + 2);
  node->condition()->accept(this, lvl + 4);
  closeTag("condition", lvl + 2);
  openTag("block", lvl + 2);
  node->block()->accept(this, lvl + 4);
  closeTag("block", lvl + 2);
  closeTag(node, lvl);
}

//---------------------------------------------------------------------------

void mml::xml_writer::do_if_node(mml::if_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  openTag("condition", lvl + 2);
  node->condition()->accept(this, lvl + 4);
  closeTag("condition", lvl + 2);
  openTag("then", lvl + 2);
  node->block()->accept(this, lvl + 4);
  closeTag("then", lvl + 2);
  closeTag(node, lvl);
}

void mml::xml_writer::do_if_else_node(mml::if_else_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  openTag("condition", lvl + 2);
  node->condition()->accept(this, lvl + 4);
  closeTag("condition", lvl + 2);
  openTag("then", lvl + 2);
  node->thenblock()->accept(this, lvl + 4);
  closeTag("then", lvl + 2);
  openTag("else", lvl + 2);
  node->elseblock()->accept(this, lvl + 4);
  closeTag("else", lvl + 2);
  closeTag(node, lvl);
}

//---------------------------------------------------------------------------

void mml::xml_writer::do_address_of_node(mml::address_of_node *const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  node->lvalue()->accept(this, lvl + 2);
  closeTag(node, lvl);
}

void mml::xml_writer::do_block_node(mml::block_node *const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  openTag("declarations", lvl + 2);
  if (node->declarations()) {
    node->declarations()->accept(this, lvl + 4);
  }
  closeTag("declarations", lvl + 2);
  openTag("instructions", lvl + 2);
  if (node->instructions()) {
    node->instructions()->accept(this, lvl + 4);
  }
  closeTag("instructions", lvl + 2);
  closeTag(node, lvl);
}

void mml::xml_writer::do_declaration_node(mml::declaration_node *const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  os() << std::string(lvl, ' ') << "<" << node->label() 
       << " qualifier='" << qualifier_name(node->qualifier()) 
       << "' identifier='"<< node->identifier() 
       << "' type='" << (node->type() ? type(cdk::to_string(node->type())) : "variable")
       << "'>" << std::endl;
  
  if (node->rvalue()) {
    openTag("rvalue", lvl + 2);
    node->rvalue()->accept(this, lvl + 4);
    closeTag("rvalue", lvl + 2);
  }
  closeTag(node, lvl);
}

void mml::xml_writer::do_function_call_node(mml::function_call_node *const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  openTag("identifier", lvl + 2);
  if (node->identifier()) {
    node->identifier()->accept(this, lvl + 4);
  }
  closeTag("identifier", lvl + 2);
  openTag("arguments", lvl + 2);
  if (node->arguments()) {
    node->arguments()->accept(this, lvl + 4);
  }
  closeTag("arguments", lvl + 2);
  closeTag(node, lvl);
}

void mml::xml_writer::do_function_def_node(mml::function_def_node *const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  os() << std::string(lvl, ' ') << "<" << node->label() 
       << " is_main='" << (node->is_main()? "true" : "false") 
        << "' type='" << type(cdk::to_string(node->type()))
       << "'>" << std::endl;
  openTag("arguments", lvl + 2);
  if (node->arguments()) {
    node->arguments()->accept(this, lvl + 4);
  }
  closeTag("arguments", lvl + 2);
  openTag("block", lvl + 2);
  node->block()->accept(this, lvl + 4);
  closeTag("block", lvl + 2);
  closeTag(node, lvl);
}

void mml::xml_writer::do_identity_node(mml::identity_node *const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  node->argument()->accept(this, lvl + 2);
  closeTag(node, lvl);
}

void mml::xml_writer::do_index_node(mml::index_node *const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  openTag("ptr", lvl + 2);
  node->ptr()->accept(this, lvl + 4);
  closeTag("ptr", lvl + 2);
  openTag("idx", lvl + 2);
  node->idx()->accept(this, lvl + 4);
  closeTag("idx", lvl + 2);
  closeTag(node, lvl);
}

void mml::xml_writer::do_input_node(mml::input_node *const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  closeTag(node, lvl);
}

void mml::xml_writer::do_next_node(mml::next_node *const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  os () << std::string(lvl, ' ') << "<" << node->label() << " level='" 
        << node->level() << "'>" << std::endl;
  closeTag(node, lvl);
}

void mml::xml_writer::do_nullptr_node(mml::nullptr_node *const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  closeTag(node, lvl);
}

void mml::xml_writer::do_return_node(mml::return_node *const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  openTag("retval", lvl + 2);
  if (node->retval()) {
    node->retval()->accept(this, lvl + 4);
  }
  closeTag("retval", lvl + 2);
  closeTag(node, lvl);
}

void mml::xml_writer::do_sizeof_node(mml::sizeof_node *const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  openTag("argument", lvl + 2);
  node->argument()->accept(this, lvl + 4);
  closeTag("argument", lvl + 2);
  closeTag(node, lvl);
}

void mml::xml_writer::do_stack_alloc_node(mml::stack_alloc_node *const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  openTag(node, lvl);
  node->argument()->accept(this, lvl + 2);
  closeTag(node, lvl);
}

void mml::xml_writer::do_stop_node(mml::stop_node *const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  os () << std::string(lvl, ' ') << "<" << node->label() << " level='" 
        << node->level() << "'>" << std::endl;
  closeTag(node, lvl);
}

void mml::xml_writer::do_write_node(mml::write_node *const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  os() << std::string(lvl, ' ') << "<" << node->label() << " has_newline='" 
       << (node->has_newline() ? "true" : "false") << "'>" << std::endl;
  node->expressions()->accept(this, lvl + 2);
  closeTag(node, lvl);
}
