FROM erlang:21-alpine AS jdk-erlang
RUN apk --update add \
        bash \
        nss \
        openjdk8 \
        && rm -rf /var/cache/apk/*

FROM jdk-erlang AS builder
COPY ./ /appSrc/
WORKDIR /appSrc
RUN chmod +x gradlew \
    && ./gradlew --no-daemon frontend:plainJar

# A dockerfile for running the `absc' command-line compiler
#
# To build the image, cd to the abstools root directory and run:
#
#     docker build -t absc -f frontend/Dockerfile .
#
# To compile an abs model `file.abs' in the current directory:
#
#     docker run --rm -v "$PWD":/usr/src -w /usr/src absc -erlang file.abs
#
# To get a command-line inside the container for the current directory
# (e.g., to run a model via `gen/erl/run' inside the container):
#
#     docker run -it --rm -v "$PWD":/usr/src -w /usr/src --entrypoint /bin/sh absc
#
# Note that if you have installed erlang on the host, you do not need
# to run the model inside the container, since the compiled files will
# be in your current directory.
FROM jdk-erlang
LABEL maintainer="Rudolf Schlatte <rudi@ifi.uio.no>"
COPY --from=builder /appSrc/frontend/bin/bash/absc /usr/local/bin/absc
COPY --from=builder /appSrc/frontend/dist/absfrontend.jar /usr/local/lib/absfrontend.jar
CMD ["-help"]
ENTRYPOINT ["/usr/local/bin/absc"]
