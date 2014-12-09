FROM ubuntu:14.04

MAINTAINER Klacz-NG developrs, see CONTRIBUTORS in main source distribution

RUN apt-get -y update \
    && apt-get -y install python-pip python-dev \
    && rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/*

RUN useradd -rm klaczng

ADD . /srv/klaczng
WORKDIR /srv/klaczng

RUN pip install -r vendor/requirements.txt

RUN chown -R klaczng:klaczng /srv/klaczng
USER klaczng
