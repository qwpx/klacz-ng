FROM ubuntu:14.04

MAINTAINER Klacz-NG developrs, see CONTRIBUTORS in main source distribution

RUN locale-gen en_US.UTF-8

ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

RUN useradd -rm klaczng

RUN mkdir /srv/klaczng
RUN chown -R klaczng:klaczng /srv/klaczng

RUN mkdir /opt/klaczng
RUN chown -R klaczng:klaczng /opt/klaczng

RUN apt-get -y update
RUN apt-get -y install supervisor git libtool build-essential autoconf automake pkg-config wget

# Setup zeromq
WORKDIR /tmp
RUN wget http://download.zeromq.org/zeromq-4.0.5.tar.gz \
  && tar -zxf zeromq-4.0.5.tar.gz \
  && cd zeromq-4.0.5 && ./configure && make && make install
# Setup haskell
RUN apt-get -y install software-properties-common

RUN apt-add-repository "deb http://ppa.launchpad.net/hvr/ghc/ubuntu precise main" \
    && apt-get -y update

RUN apt-get install -y --force-yes \
    ghc-7.8.3 \
    cabal-install-1.20 \
    && rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/*

env PATH=/opt/ghc/7.8.3/bin:/opt/cabal/1.20/bin:${PATH}
RUN cabal-1.20 update
RUN cabal-1.20 install protocol-buffers protocol-buffers-descriptor \
  uuid lens zeromq4-haskell lifted-base monad-control monads-tf transformers-base \
  attoparsec irc network optparse-applicative

WORKDIR /tmp
RUN git clone https://github.com/xyzzyz/protopap.git
WORKDIR /tmp/protopap
RUN cabal-1.20 configure && cabal-1.20 install

# Set up lib/

ADD lib/haskell/ /tmp/haskell/
WORKDIR /tmp/haskell
RUN cabal configure && cabal install --global

# Set up ephemeral/

ADD ephemeral/ /srv/klaczng/ephemeral/
WORKDIR /tmp/ephemeral
RUN cabal configure && cabal install --global

# Set up gateway

ADD gateway/ /srv/klaczng/gateway
WORKDIR /srv/klaczng/gateway

RUN mkdir /tmp/gateway-build
RUN cabal-1.20 configure --builddir=/tmp/gateway-build \
  && cabal-1.20 install --builddir=/tmp/gateway-build --prefix /opt/klaczng/gateway

# Set up stateless

ADD stateless/ /srv/klaczng/stateless
WORKDIR /srv/klaczng/stateless

RUN mkdir /tmp/stateless-build
RUN cabal-1.20 configure --builddir=/tmp/stateless-build \
  && cabal-1.20 install --builddir=/tmp/stateless-build --prefix /opt/klaczng/stateless

# init image
USER klaczng

WORKDIR /tmp





