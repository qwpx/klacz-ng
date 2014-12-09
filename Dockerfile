FROM ubuntu:14.04

MAINTAINER Klacz-NG developrs, see CONTRIBUTORS in main source distribution

RUN apt-get -y update \
    && apt-get -y install python-pip python-dev \
    && rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/*

RUN useradd -rm klaczng

ADD lib /srv/klaczng/lib
ADD modules /srv/klaczng/modules
ADD vendor /srv/klaczng/vendor


WORKDIR /srv/klaczng
RUN pip install -r vendor/requirements.txt

RUN chown -R klaczng:klaczng /srv/klaczng
USER klaczng
