FROM alpine
RUN apk add --no-cache git
RUN mkdir /dependencies
WORKDIR /dependencies
RUN ["git", "clone", "https://github.com/scheme-requests-for-implementation/srfi-69/"]
RUN ["git", "clone", "https://github.com/scheme-requests-for-implementation/srfi-125/"]
RUN ["git", "clone", "https://github.com/scheme-requests-for-implementation/srfi-126/"]
RUN ["git", "clone", "https://github.com/scheme-requests-for-implementation/srfi-146/"]
RUN ["wget", "https://snow-fort.org/s/ccs.neu.edu/will/r6rs/enums/0.0.1/r6rs-enums-0.0.1.tgz"]
RUN ["tar", "-xf", "r6rs-enums-0.0.1.tgz"]
RUN ["wget", "https://snow-fort.org/s/ccs.neu.edu/will/r6rs/lists/0.0.1/r6rs-lists-0.0.1.tgz"]
RUN ["tar", "-xf", "r6rs-lists-0.0.1.tgz"]
RUN ["wget", "https://snow-fort.org/s/ccs.neu.edu/will/r6rs/sorting/0.0.1/r6rs-sorting-0.0.1.tgz"]
RUN ["tar", "-xf", "r6rs-sorting-0.0.1.tgz"]
