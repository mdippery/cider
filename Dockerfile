## This Dockerfile can be used to compile a `cider` binary that will
## work on Linux. It should be built and run with:
##
##     export CIDER_VERSION=<cider version>
##     docker build --build-arg CIDER_VERSION -t mipadi/cider -t mipadi/cider:$CIDER_VERSION .
##     mkdir .build
##     docker run --rm -v $(pwd)/.build:/root/build mipadi/cider

FROM ubuntu:latest AS source

ARG CIDER_VERSION

RUN apt-get update \
 && apt-get install -y curl \
 && apt-get install -y git

WORKDIR /root
RUN git clone https://github.com/mdippery/cider.git

WORKDIR /root/cider
RUN git checkout v${CIDER_VERSION}

###############################################################################

FROM ubuntu:latest AS build

ENV PATH=/root/.local/bin:${PATH}

RUN apt-get update \
 && apt-get install -y curl \
 && curl -sSL https://get.haskellstack.org/ | sh

COPY --from=source /root/cider/ /root/cider/
WORKDIR /root/cider
RUN stack setup \
 && stack build \
 && stack test \
 && stack install

###############################################################################

FROM ubuntu:latest

VOLUME /root/build

COPY --from=build /root/.local /root/.local
CMD cp /root/.local/bin/cider /root/build/cider \
 && ldd /root/build/cider
