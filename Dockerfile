from ubuntu:18.10

RUN apt-get update && apt-get install -y mlton git vim make build-essential autoconf

COPY . /root/mpl
WORKDIR /root/mpl

RUN autoreconf -vfi && ./configure && make all-no-docs BOOTSTRAP_OTHER=false
