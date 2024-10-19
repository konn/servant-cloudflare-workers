VERSION 0.8
ARG --global GHC_VER=9.10.1
ARG --global GLOBAL_CACHE_IMAGE=ghcr.io/konn/shortener/build-cache
FROM --platform=linux/amd64 ghcr.io/konn/ghc-wasm-earthly:${GHC_VER}
WORKDIR /workdir

ENV GHC=wasm32-wasi-ghc
ENV CABAL=wasm32-wasi-cabal --project-file=cabal-wasm.project \
--with-compiler=wasm32-wasi-ghc-${GHC_VER} \
--with-ghc=wasm32-wasi-ghc-${GHC_VER} \
--with-ghc-pkg=wasm32-wasi-ghc-pkg-${GHC_VER} \
--with-hc-pkg=wasm32-wasi-ghc-pkg-${GHC_VER} \
--with-hsc2hs=wasm32-wasi-hsc2hs-${GHC_VER}

build-all:
  COPY --keep-ts ./*.project ./
  COPY --keep-ts ./*.freeze ./
  COPY --keep-ts ./servant-cloudflare-workers ./servant-cloudflare-workers
  CACHE --sharing shared --chmod 0777 --id=all#ghc-${GHC_VER}#global-store --persist /root/.ghc-wasm/.cabal/store
  CACHE --sharing=shared --chmod=0777 --id=all#ghc${GHC_VER}#dist-newstyle --persist dist-newstyle
  RUN ${CABAL} update --index-state=2024-10-17T07:25:36Z
  RUN ${CABAL} build --only-dependencies all
  RUN ${CABAL} build servant-cloudflare-workers:exe:greet

build:
  FROM +build-all
  BUILD  --platform=linux/amd64 +build-all
  ARG target
  ARG outdir=$(echo ${target} | cut -d: -f3)
  ARG wasm=${outdir}.wasm
  # From frontend/build.sh in tweag/ghc-wasm-miso-examples
  LET HS_WASM_PATH=$(${CABAL} list-bin -v0 ${target})
  LET WASM_LIB=$(wasm32-wasi-ghc --print-libdir)
  LET DEST=dist/${wasm}
  RUN mkdir -p dist
  RUN cp ${HS_WASM_PATH} ./dist/${wasm}
  RUN ${WASM_LIB}/post-link.mjs --input ${HS_WASM_PATH} --output ./dist/ghc_wasm_jsffi.js
  SAVE ARTIFACT dist

optimised-wasm:
  ARG target
  ARG outdir=$(echo ${target} | cut -d: -f3)
  ARG wasm=${outdir}.wasm
  RUN mkdir -p dist/
  BUILD --platform=linux/amd64 +build --target=${target} --outdir=${outdir} --wasm=${wasm}.orig
  COPY (+build/dist/${wasm}.orig --target=${target} --outdir=${outdir} --wasm=${wasm}.orig) ./dist/
  RUN wizer --allow-wasi --wasm-bulk-memory true --init-func _initialize -o dist/${wasm} dist/${wasm}.orig
  RUN wasm-opt -Oz dist/${wasm} -o dist/${wasm}
  RUN wasm-tools strip -o dist/${wasm} dist/${wasm}
  COPY (+build/dist/ghc_wasm_jsffi.js --target=${target} --outdir=${outdir} --wasm=${wasm}.orig) ./dist/
  RUN rm ./dist/${wasm}.orig
  SAVE ARTIFACT dist

patch-jsffi-for-cf:
  ARG target
  ARG outdir=$(echo ${target} | cut -d: -f3)
  ARG wasm=${outdir}.wasm
  BUILD --platform=linux/amd64 +optimised-wasm --target=${target} --outdir=${outdir} --wasm=${wasm}
  COPY  (+optimised-wasm/dist --target=${target} --outdir=${outdir} --wasm=${wasm}) ./dist
  LET PATCHER=./js-ffi-patcher.mjs
  COPY ./build-scripts/jsffi-patcher.mjs ${PATCHER}
  RUN node ${PATCHER} ./dist/ghc_wasm_jsffi.js
  SAVE ARTIFACT ./dist

greet:
  COPY servant-cloudflare-workers/data/worker-template/ ./dist/
  BUILD --platform=linux/amd64  +patch-jsffi-for-cf --target=servant-cloudflare-workers:exe:greet --wasm=worker.wasm
  COPY (+patch-jsffi-for-cf/dist --target=servant-cloudflare-workers:exe:greet --wasm=worker.wasm) ./dist/src
  RUN cd ./dist && npm i
  SAVE ARTIFACT ./dist AS LOCAL _build/worker
