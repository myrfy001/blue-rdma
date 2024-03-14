FROM rust

copy ./setup.sh ./
RUN ./setup.sh