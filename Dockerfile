FROM haskell:latest

RUN apt-get update && apt-get install -y build-essential

RUN stack upgrade
RUN stack setup

ADD . /app

RUN cd /app && stack build