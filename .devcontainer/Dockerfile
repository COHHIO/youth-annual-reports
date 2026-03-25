FROM ghcr.io/browserless/chrome:latest

# install system dependencies for pak
USER root

RUN apt-get update && apt-get install -y \
  # {Matrix}
  gfortran \
  libblas-dev \
  liblapack-dev \
  libpq-dev \
  libssl-dev \
  && rm -rf /var/lib/apt/lists/*