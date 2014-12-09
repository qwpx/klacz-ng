FROM ubuntu:14.04

MAINTAINER Klacz-NG developrs, see CONTRIBUTORS in main source distribution

RUN apt-get -y update \
    && apt-get -y install python-pip python-dev \
    && pip install twisted \
    && pip install pika \
    && rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/*

RUN useradd -rm klaczng

ADD . /srv/klaczng

RUN chown -R klaczng:klaczng /srv/klaczng
WORKDIR /srv/klaczng
USER klaczng
