FROM ubuntu:bionic

ENV DEBIAN_FRONTEND=noninteractive \
    DOCKER_BUILD=1

RUN apt-get update && \
    apt-get install -y apt-transport-https libcurl3-gnutls libarchive13 wget desktop-file-utils aria2 fuse gnupg2 build-essential file libglib2.0-bin git && \
    install -m 0777 -d /workspace && \
    mkdir /debs

RUN adduser --system --uid 1000 test

COPY *.deb /debs
RUN chown -R test /workspace
USER test

WORKDIR /workspace