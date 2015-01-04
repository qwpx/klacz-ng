FROM ubuntu:14.04

MAINTAINER Klacz-NG developrs, see CONTRIBUTORS in main source distribution

RUN locale-gen en_US.UTF-8

ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

RUN apt-get -y update  \
    && apt-get -y install python-pip python-dev software-properties-common supervisor

RUN apt-add-repository "deb http://ppa.launchpad.net/hvr/ghc/ubuntu precise main" \
    && apt-get -y update

RUN apt-get install -y --force-yes \
    ghc-7.8.3 \
    cabal-install-1.20 \
    && rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/*

RUN useradd -rm klaczng

ADD lib /srv/klaczng/lib
ADD modules /srv/klaczng/modules
ADD vendor /srv/klaczng/vendor

WORKDIR /srv/klaczng
RUN pip install -r vendor/requirements.txt
RUN /bin/sh ./vendor/haskell-modules

RUN chown -R klaczng:klaczng /srv/klaczng
USER klaczng
