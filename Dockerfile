## This Dockerfile can be used to compile a `cider` binary that will
## work on Linux. It should be run with, e.g.,
##
##     docker run -it -v $(pwd)/.build:/root/.local mipadi/cider

FROM ubuntu:latest AS build

VOLUME /root/.local

ENV PATH=/root/.local/bin:${PATH}

RUN apt-get update \
 && apt-get install -y curl \
 && curl -sSL https://get.haskellstack.org/ | sh

WORKDIR /root
COPY . .
RUN stack setup \
 && stack build \
 && stack test

CMD stack install && ldd /root/.local/bin/cider
