# Build the Haskell project
FROM haskell:8.8 as build
WORKDIR /build

# Copy source directory
COPY . .
RUN cabal new-update
RUN cabal new-build zkp:exe:zkp
RUN cp -f $(find dist-newstyle/build/x86_64-linux -name zkp -type f -perm -u=x) /usr/local/bin/

# Create a small image containing only the compiled zkp program
FROM debian:stretch-slim

RUN apt-get update && \
  apt-get install -y \
    libgmp10 \
    libc6 \
    && \
  apt-get autoremove -y && \
  apt-get clean -y && \
  rm -rf /var/lib/apt/lists/*

COPY --from=build /usr/local/bin/zkp /usr/local/bin/

ENTRYPOINT ["/usr/local/bin/zkp"]
