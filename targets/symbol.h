#ifndef __MML_TARGETS_SYMBOL_H__
#define __MML_TARGETS_SYMBOL_H__

#include <string>
#include <memory>
#include <cdk/types/basic_type.h>

namespace mml {

  class symbol {
    std::shared_ptr<cdk::basic_type> _type;
    std::string _name;
    int _qualifier; // qualifiers: public, forward, "private" (i.e., none)
    int _offset; // 0 (zero) means global variable/function
    bool _is_foreign; // check if it's a foreign function
    bool _is_main; // check if it's the main function (since a program_node isn't used)

  public:
    symbol(std::shared_ptr<cdk::basic_type> type, const std::string &name, long value, int qualifier) :
        _type(type), _name(name), _qualifier(qualifier), _is_foreign(false), _is_main(false) {
    }

    virtual ~symbol() {
      // EMPTY
    }

    std::shared_ptr<cdk::basic_type> type() const {
      return _type;
    }
    bool is_typed(cdk::typename_type name) const {
      return _type->name() == name;
    }
    
    const std::string &name() const {
      return _name;
    }
    
    bool is_main() {
      return _is_main;
    }
    void set_main(bool val) {
      _is_main = val;
    }
    
    bool is_foreign() {
      return _is_foreign;
    }
    void set_foreign(bool val) {
      _is_foreign = val;
    }
    
    int offset() {
      return _offset;
    }
    void set_offset(int offset) {
      _offset = offset;
    }
    
    int qualifier() {
      return _qualifier;
    }
    void set_qualifier(int qualifier) {
      _qualifier = qualifier;
    }
  };
  
  // Creates a new symbol, using a shared pointer
  inline auto create_symbol(std::shared_ptr<cdk::basic_type> type, const std::string &name, long value, int qualifier) {
      return std::make_shared<symbol>(type, name, value, qualifier);
  }

} // mml

#endif
