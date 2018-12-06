FROM erlang:21-alpine AS jdk-erlang
RUN apk --update add openjdk8 bash && rm -rf /var/cache/apk/*

FROM jdk-erlang AS builder
COPY ./.git /appSrc/.git
COPY ./frontend /appSrc/frontend
WORKDIR /appSrc/frontend
RUN chmod +x gradlew \
    && ./gradlew build -x test -x check

# A dockerfile for running the `absc' command-line compiler

# To build the image:
#     docker build -t absc -f Dockerfile ..
# To compile an abs model `file.abs' in the current directory:
#     docker run --rm -v "$PWD":/usr/src -w /usr/src absc -erlang file.abs
# To get a command-line inside the container for the current directory
# (e.g., to run a model via `gen/erl/run' inside the container):
#     docker run -it --rm -v "$PWD":/usr/src -w /usr/src --entrypoint /bin/sh absc
FROM jdk-erlang
LABEL maintainer="Rudolf Schlatte <rudi@ifi.uio.no>"
COPY --from=builder /appSrc/frontend/bin/bash/absc /usr/local/bin/absc
COPY --from=builder /appSrc/frontend/dist/absfrontend.jar /usr/local/lib/absfrontend.jar
CMD ["-help"]
ENTRYPOINT ["/usr/local/bin/absc"]
