FROM haskell:9.4.3

ADD server/bomberman.cabal /bomberman.cabal
ADD server/stack.yaml /stack.yaml
RUN stack setup
RUN stack build --dependencies-only

ENV DEBIAN_FRONTEND=noninteractive
RUN apt update && apt install -y nodejs=10.24.0~dfsg-1~deb10u2 npm=5.8.0+ds6-4+deb10u2 && rm -rf /var/lib/apt/lists/*

RUN npm install -g --unsafe-perm elm@latest-0.18.0
RUN npm install -g --unsafe-perm elm-test@latest-0.18.0
RUN npm install -g --unsafe-perm yarn@1.22.19
