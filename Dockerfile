FROM alpine
RUN apk add --no-cache git
RUN mkdir /test
WORKDIR /test
ADD . srfi-225
RUN ["git", "clone", "https://github.com/scheme-requests-for-implementation/srfi-69/"]
RUN ["git", "clone", "https://github.com/scheme-requests-for-implementation/srfi-125/"]
RUN ["git", "clone", "https://github.com/scheme-requests-for-implementation/srfi-126/"]
RUN ["git", "clone", "https://github.com/scheme-requests-for-implementation/srfi-146/"]
