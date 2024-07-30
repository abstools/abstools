# syntax=docker/dockerfile:1.3-labs
FROM erlang:26-alpine AS jdk-erlang
RUN <<EOF
apk --update add bash nss openjdk21-jdk gcc libc-dev git
rm -rf /var/cache/apk/*
EOF

FROM jdk-erlang AS builder
COPY ./ /appSrc/
WORKDIR /appSrc
RUN <<EOF
chmod +x gradlew
./gradlew --no-daemon frontend:assemble
EOF

# A dockerfile for running the `absc` command-line compiler and the analysis
# tools `apet`, `cofloco`, `costabs`, `maypar`, `pubs` and `syco`.
#
# To build the image, cd to the abstools root directory and run:
#
# : docker build -t abslang/absc -f docker/absc.Dockerfile .
#
# To compile an abs model `file.abs' in the current directory:
#
# : docker run --rm -v "$PWD":/usr/src -w /usr/src abslang/absc:latest --erlang file.abs
#
# To run a compiled abs model:
#
# : docker run --rm -v "$PWD":/usr/src -w /usr/src --entrypoint /usr/src/gen/erl/run abslang/absc
#
# To run a different tool than `absc`, for example `costabs':
#
# : docker run --rm -v "$PWD":/usr/src -w /usr/src --entrypoint /usr/local/bin/costabs abslang/absc:latest file.abs
#
# To get a command-line inside the container for the current directory
# (e.g., to run a model via `gen/erl/run' inside the container):
#
# : docker run -it --rm -v "$PWD":/usr/src -w /usr/src --entrypoint /bin/sh abslang/absc
#
# Note that if you have installed erlang on the host, you do not need
# to run the model inside the container, since the compiled files will
# be in your current directory.
FROM jdk-erlang
LABEL maintainer="Rudolf Schlatte <rudi@ifi.uio.no>"
COPY --from=builder /appSrc/binaries /binaries
COPY --from=builder /appSrc/frontend/bin/bash/absc /usr/local/bin/absc
COPY --from=builder /appSrc/frontend/dist/absfrontend.jar /usr/local/lib/absfrontend.jar
RUN <<EOF
unzip /binaries/saco.colab.zip -d /usr/local/lib
ln -s ../lib/saco/bin/costabs /usr/local/bin/
ln -s ../lib/saco/bin/deco /usr/local/bin/
ln -s ../lib/saco/bin/generateProlog /usr/local/bin/
ln -s ../lib/saco/bin/maypar /usr/local/bin/
ln -s ../lib/saco/bin/pubs /usr/local/bin/
unzip /binaries/cofloco.colab.zip -d /usr/local/lib
ln -s ../lib/cofloco/bin/cofloco /usr/local/bin/
unzip /binaries/sra.colab.zip -d /usr/local/lib
unzip /binaries/apet.colab.zip -d /usr/local/lib
ln -s ../lib/apet/bin/apet /usr/local/bin/
ln -s ../lib/apet/bin/syco /usr/local/bin/
sed -i 's|java -cp $ABSFRONTEND abs.backend.prolog.PrologBackend|java -jar /usr/local/lib/absfrontend.jar --prolog|g' /usr/local/lib/saco/bin/generateProlog
sed -i 's|java -cp $ABSFRONTEND abs.backend.prolog.PrologBackend|java -jar /usr/local/lib/absfrontend.jar --prolog|g' /usr/local/lib/apet/bin/generateProlog
EOF
CMD ["--help"]
ENTRYPOINT ["/usr/local/bin/absc"]
