FROM erlang:24-alpine AS jdk-erlang
RUN apk --update add \
    bash \
    nss \
    openjdk11-jdk \
    gcc libc-dev \
    git \
 && rm -rf /var/cache/apk/*

FROM jdk-erlang AS builder
COPY ./ /appSrc/
WORKDIR /appSrc
RUN chmod +x gradlew \
    && ./gradlew --no-daemon frontend:assemble

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
RUN wget http://costa.fdi.ucm.es/download/saco.colab.zip -O saco.colab.zip \
        && unzip saco.colab.zip -d /usr/local/lib \
        && rm saco.colab.zip \
        && ln -s ../lib/saco/bin/costabs /usr/local/bin/ \
        && ln -s ../lib/saco/bin/deco /usr/local/bin/ \
        && ln -s ../lib/saco/bin/generateProlog /usr/local/bin/ \
        && ln -s ../lib/saco/bin/maypar /usr/local/bin/ \
        && ln -s ../lib/saco/bin/pubs /usr/local/bin/ \
        && wget http://costa.fdi.ucm.es/download/cofloco.colab.zip -O cofloco.colab.zip \
        && unzip cofloco.colab.zip -d /usr/local/lib \
        && rm cofloco.colab.zip \
        && ln -s ../lib/cofloco/bin/cofloco /usr/local/bin/ \
        && wget http://costa.fdi.ucm.es/download/sra.colab.zip -O sra.colab.zip \
        && unzip sra.colab.zip -d /usr/local/lib \
        && rm sra.colab.zip \
        && wget http://costa.fdi.ucm.es/download/apet.colab.zip -O apet.colab.zip \
        && unzip apet.colab.zip -d /usr/local/lib \
        && rm apet.colab.zip \
        && ln -s ../lib/apet/bin/apet /usr/local/bin/ \
        && ln -s ../lib/apet/bin/syco /usr/local/bin/
COPY --from=builder /appSrc/frontend/bin/bash/absc /usr/local/bin/absc
COPY --from=builder /appSrc/frontend/dist/absfrontend.jar /usr/local/lib/absfrontend.jar
RUN sed -i 's|java -cp $ABSFRONTEND abs.backend.prolog.PrologBackend|java -jar /usr/local/lib/absfrontend.jar --prolog|g' /usr/local/lib/saco/bin/generateProlog
RUN sed -i 's|java -cp $ABSFRONTEND abs.backend.prolog.PrologBackend|java -jar /usr/local/lib/absfrontend.jar --prolog|g' /usr/local/lib/apet/bin/generateProlog
CMD ["--help"]
ENTRYPOINT ["/usr/local/bin/absc"]
