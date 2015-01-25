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
RUN apt-get -y install supervisor

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
RUN cabal-1.20 install protocol-buffers protocol-buffers-descriptor uuid

ADD lib/haskell/ /tmp/haskell/
WORKDIR /tmp/haskell
RUN cabal configure && cabal install --global

ADD modules/pick /srv/klaczng/pick
WORKDIR /srv/klaczng/pick

# RUN mkdir /tmp/pick-build
# RUN cabal-1.20 configure --builddir=/tmp/pick-build \
#     && cabal-1.20 install --builddir=/tmp/pick-build --prefix /opt/klaczng/pick

# init image
USER klaczng

WORKDIR /tmp





