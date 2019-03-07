FROM fpco/stack-build:lts-13.8 as build

WORKDIR /build
COPY . /build

RUN apt update && apt install -y libpq-dev

RUN stack test && stack build --copy-bins

FROM ubuntu:16.04

COPY --from=build /root/.local/bin/haskell-customer-service-exe /usr/local/bin
RUN chmod +x /usr/local/bin/haskell-customer-service-exe

CMD haskell-customer-service-exe
