# MML compiler

Project developed in collaboration with [Pedro Chaparro](https://github.com/PedroChaps) for our Compilers class at Técnico Lisboa, 2022/2023.

The steps to develop a full MML compiler imply the adaptation of:
* the scanner (`mml_scanner.l`)
* the parser (`mml_parser.y`)
* the symbol (`targets/symbol.h`)
* the type checker (`targets/type_checker.cpp`)
* the XML writer (for the middle delivery: `targets/xml_writer.cpp`)
* the Postfix writer (for the final delivery: `targets/postfix_writer.cpp`)
