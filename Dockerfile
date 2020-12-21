FROM clojure:lein-2.9.5 AS BASE

RUN apt-get update
RUN apt-get install --no-install-recommends -yy curl unzip build-essential zlib1g-dev
WORKDIR "/opt"
RUN curl -sLO https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-20.3.0/graalvm-ce-java11-linux-amd64-20.3.0.tar.gz
RUN tar -xzf graalvm-ce-java11-linux-amd64-20.3.0.tar.gz
ENV GRAALVM_HOME="/opt/graalvm-ce-java11-20.3.0"
ENV JAVA_HOME="/opt/graalvm-ce-java11-20.3.0/bin"
ENV PATH="$JAVA_HOME:$PATH"

# This is a hack to avoid redownload dependencies
COPY script/deps project.clj deps.edn ./
RUN ./deps && rm -f ./deps

COPY . .
RUN ./script/uberjar
RUN ./script/compile

FROM ubuntu:latest
RUN mkdir -p /usr/local/bin
COPY --from=BASE /opt/clojure-lsp /usr/local/bin/clojure-lsp
CMD ["clojure-lsp"]
