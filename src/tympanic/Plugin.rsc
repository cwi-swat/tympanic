module tympanic::Plugin

import util::IDE;
import tympanic::Syntax;
import ParseTree;


void main() {
  registerLanguage("Tympanic", "tymp", start[ASTMapping](str src, loc org) {
    return parse(#start[ASTMapping], src, org);
  });
}
