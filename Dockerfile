FROM alpine
RUN apk add --no-cache git
RUN mkdir /dependencies
WORKDIR /dependencies

RUN mkdir /dependencies/srfi-27/srfi/ -p
RUN echo "\
(define-library (srfi 27)\
                (import (scheme base))\
                (export random-integer)\
                (begin\
                  (define (random-integer arg) arg)))\
" > /dependencies/srfi-27/srfi/27.sld

RUN ["git", "clone", "https://github.com/scheme-requests-for-implementation/srfi-69/"]
RUN ["git", "clone", "https://github.com/scheme-requests-for-implementation/srfi-125/"]
RUN ["git", "clone", "https://github.com/scheme-requests-for-implementation/srfi-126/"]
RUN ["git", "clone", "https://github.com/scheme-requests-for-implementation/srfi-128/"]

RUN ["git", "clone", "https://github.com/scheme-requests-for-implementation/srfi-143/"]
RUN sed -i 's/(srfi-143)/(srfi 143)/g' /dependencies/srfi-143/srfi-143/srfi-143.sld

RUN ["git", "clone", "https://github.com/scheme-requests-for-implementation/srfi-146/"]

RUN mkdir /dependencies/srfi-145/srfi/ -p
RUN echo "\
(define-library (srfi 145)\
                (import (scheme base))\
                (export assume)\
                (begin\
                (define-syntax assume\
                    (syntax-rules ()\
                        ((assume expression message ...)\
                         (or expression\
                             (error \"invalid assumption\" (quote expression) (list message ...))))\
                        ((assume . _)\
                         (syntax-error \"invalid assume syntax\"))))))\
" > /dependencies/srfi-145/srfi/145.sld

RUN ["git", "clone", "https://github.com/scheme-requests-for-implementation/srfi-151/"]
RUN sed -i 's/(srfi-151)/(srfi 151)/g' /dependencies/srfi-151/srfi-151/srfi-151.sld

RUN ["git", "clone", "https://github.com/scheme-requests-for-implementation/srfi-158/"]
RUN sed -i 's/(srfi-158)/(srfi 158)/g' /dependencies/srfi-158/srfi-158.sld

RUN ["wget", "https://snow-fort.org/s/ccs.neu.edu/will/r6rs/enums/0.0.1/r6rs-enums-0.0.1.tgz"]
RUN ["tar", "-xf", "r6rs-enums-0.0.1.tgz"]
RUN ["wget", "https://snow-fort.org/s/ccs.neu.edu/will/r6rs/lists/0.0.1/r6rs-lists-0.0.1.tgz"]
RUN ["tar", "-xf", "r6rs-lists-0.0.1.tgz"]
RUN ["wget", "https://snow-fort.org/s/ccs.neu.edu/will/r6rs/sorting/0.0.1/r6rs-sorting-0.0.1.tgz"]
RUN ["tar", "-xf", "r6rs-sorting-0.0.1.tgz"]
