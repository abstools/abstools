FROM frolvlad/alpine-oraclejdk8:slim
MAINTAINER Rudi Schlatte <rudi@ifi.uio.no>

# A dockerfile for running the `absc' command-line compiler

# To build the image:
#     docker build -t absc .
# To compile an abs model `file.abs' in the current directory:
#     docker run --rm -v "$PWD":/usr/src -w /usr/src absc -erlang file.abs
# To get a command-line inside the container for the current directory
# (e.g., to run a model via `gen/erl/run' inside the container):
#     docker run -it --rm -v "$PWD":/usr/src -w /usr/src --entrypoint /bin/sh absc


RUN apk --update add erlang && rm -rf /var/cache/apk/*
COPY src/bash/absc /usr/local/bin/absc
COPY dist/absfrontend.jar /usr/local/lib/absfrontend.jar
CMD ["-help"]
ENTRYPOINT ["/usr/local/bin/absc"]
