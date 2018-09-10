# Usage:
# $ docker build -t absmodel .
# $ docker run absmodel
FROM erlang:18
COPY . /gen/erl
WORKDIR /gen/erl
ENTRYPOINT ["/gen/erl/run"]
