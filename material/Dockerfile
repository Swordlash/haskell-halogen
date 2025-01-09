FROM ubuntu:22.04
ENV DEBIAN_FRONTEND=noninteractive
SHELL ["/bin/bash", "-c"]

RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    libffi-dev \
    libffi8 \
    libgmp-dev \
    libgmp10 \
    libncurses-dev \
    libncurses5 \
    libtinfo5 \
    python3 \
    cmake \
    git \
    unzip \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN git clone https://github.com/emscripten-core/emsdk.git && \
    cd emsdk && \
    ./emsdk install 3.1.74 && \
    ./emsdk activate 3.1.74 && \
    echo "source /emsdk/emsdk_env.sh" >> /root/.bashrc

ENV PATH="/emsdk/upstream/emscripten:/emsdk:/emsdk/upstream/bin:$PATH"

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_GHC_VERSION=9.10.1 BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_CABAL_VERSION=3.14.1.1 bash && \
        echo "source /root/.ghcup/env" >> /root/.bashrc

ENV PATH="/root/.ghcup/bin:/root/.cabal/bin:$PATH"

RUN ghcup config add-release-channel cross
RUN emconfigure ghcup install ghc --set javascript-unknown-ghcjs-9.12.1

RUN emcc --version && \
    ghcup --version && \
    javascript-unknown-ghcjs-ghc --version