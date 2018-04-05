FROM jenkins/jenkins:lts
# if we want to install via apt
USER root


# dependencies for abstools and abs_optimizer
RUN apt-get update && apt-get install -y \
        wget && \
        wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && \
        dpkg -i erlang-solutions_1.0_all.deb && \
        apt-get update && apt-get install -y \
        make \
        git \
        default-jdk \
        python-dev \
        erlang-base \
        ant \
        screen \
        nano

ENV PATH /abstools/frontend/bin/bash:$PATH


# dependencies for smart_deploy

RUN apt-get update && apt-get install -y \
        python2.7 \
        python-pip

# download and install zephyurs2
RUN cd / && \
    mkdir solvers_exec && \
    cd /solvers_exec && \
    git clone --recursive --depth=1 https://jacopomauro@bitbucket.org/jacopomauro/zephyrus2.git && \
    cd zephyrus2 && \
    pip install -e /solvers_exec/zephyrus2 && \
    pip install antlr4-python2-runtime toposort psutil click && \
    apt-get update && \
    apt-get install -y \
        time \
        flex \
        bison \
        libqt5core5a \
        libqt5gui5 \
        libqt5network5 \
        libqt5webkit5 \
        libqt5widgets5 && \
    rm -rf /var/lib/apt/lists/*


# Install minizinc suite
RUN cd / && \
    wget https://github.com/MiniZinc/MiniZincIDE/releases/download/2.1.7/MiniZincIDE-2.1.7-bundle-linux-x86_64.tgz && \
    tar -zxvf MiniZincIDE-2.1.7-bundle-linux-x86_64.tgz && \
    rm -rf MiniZincIDE-2.1.7-bundle-linux-x86_64.tgz && \
        mv MiniZincIDE-2.1.7-bundle-linux-x86_64 minizinc
ENV PATH /minizinc:$PATH


  ###############
  # download fzn2smt
  ###############

#COPY ./docker_scripts/antlr-runtime-3.2.jar /antlr-runtime-3.2.jar

RUN apt-get update && \
    # openjdk and unzip for the fzn2smt
    apt-get install -y \
        unzip \
        openjdk-8-jdk && \
  rm -rf /var/lib/apt/lists/* && \
  cd /solvers_exec && \
  wget http://ima.udg.edu/recerca/lap/fzn2smt/version/fzn2smt-2-0-02.zip && \
  unzip fzn2smt-2-0-02.zip && \
  rm -rf fzn2smt-2-0-02.zip

ENV CLASSPATH=/solvers_exec/fzn2smt-2-0-02:/antlr-runtime-3.2.jar:$CLASSPATH

###############
# download and compile z3
###############
RUN cd /solvers_exec && \
    ###############
    # install z3
    ###############
    git clone --depth 1 https://github.com/Z3Prover/z3.git && \
    cd z3 && \
    python scripts/mk_make.py --python && \
    cd build && \
    make && \
    make install


# abs file for the smart deployer test run
COPY FRHErlang.abs /solvers_exec/FRHErlang.abs 
RUN chmod a=rx /solvers_exec/FRHErlang.abs

# drop back to the regular jenkins user - good practice
USER jenkins
