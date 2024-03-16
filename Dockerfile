FROM rust

copy ./setup.sh ./
RUN ./setup.sh
RUN apt-get update && apt-get install -y \
    libtcl \
    && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /bluerdma-test
WORKDIR /bluerdma-test

copy ./test/ ./test
copy ./run_system_test.sh Makefile.base Makefile.test ./