#FROM fpco/stack-build:lts-14 as build
FROM fpco/stack-build:test as build
RUN mkdir -p /opt/src /opt/bin
RUN stack build --resolver lts-14.1 conduit warp wai-extra optparse-simple rio html-conduit mime-types
COPY . /opt/src
RUN cd /opt/src && stack install --system-ghc --local-bin-path /opt/bin

FROM fpco/pid1:18.04
COPY --from=build /opt/bin/qapla /usr/local/bin/qapla
