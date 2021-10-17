 #without importing, gauche doesn't recognize cond-expand in test
gosh\
    -I /test/srfi-225\
    -I /dependencies/srfi-126\
    -I /dependencies/r6rs-enums-0.0.1\
    -I /dependencies/r6rs-lists-0.0.1\
    -I /dependencies/r6rs-sorting-0.0.1\
    -I /dependencies/srfi-146\
    -e '(import (srfi 125) (srfi 126) (srfi 146) (srfi 146 hash))'\
    /test/srfi-225/srfi-225-test.scm
