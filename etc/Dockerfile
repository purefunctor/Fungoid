FROM utdemir/ghc-musl:v20-ghc884

RUN cd mnt && \
    cabal update && \
    cabal build --enable-executable-static --installdir=build
