FROM ubuntu:20.04

# Install the dependencies. We'll use the ubuntu provided mlton to bootstrap our local build.
RUN apt-get update -qq \
 && apt-get install -qq git build-essential libgmp-dev mlton mlton-tools vim \
 && git clone https://github.com/mlton/mlton.git /root/mlton \
 && cd /root/mlton \
 && git checkout on-20241230-release \
 && make

ENV PATH=/root/mlton/build/bin:$PATH

# Copy the current directory (MPL source root) to a location within the container & move there
COPY . /root/mpl
WORKDIR /root/mpl

# Build from source, install, and make examples
RUN make && make install && cd examples && make

ENV PATH=/root/mpl/build/bin:$PATH
