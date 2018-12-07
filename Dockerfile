FROM rocker/r-ver:latest

# Set the timezone in TZ to avoid issues with Sys.timezone().
ENV TZ Etc/UTC

# Default configuration variables.
ENV CRANIUM_REPO_NAME=Cranium \
  CRANIUM_USE_ARCHIVE=true \
  CRANIUM_USE_HARDLINKS=false \
  CRANIUM_REPO_FIELDS="Package,Version,Priority,Depends,Imports,LinkingTo,\
Suggests,Enhances,License,License_is_FOSS,License_restricts_use,OS_type,Archs,\
MD5sum,NeedsCompilation" \
  CRANIUM_LATEST_ONLY=true


# Install dependencies.
ARG R_PKGS="data.table httpuv webutils mime"
ARG R_PKG_BUILD_DEPS="libcurl4-openssl-dev"
RUN apt-get update -qq && \
  apt-get install -y --no-install-recommends nginx ca-certificates $R_PKG_BUILD_DEPS && \
  install2.r --repos "https://cloud.r-project.org/" --error --deps NA $R_PKGS && \
  # Clean up build dependencies.
  apt-get remove --purge -y $R_PKG_BUILD_DEPS && \
  apt-get autoremove -y && \
  apt-get autoclean -y

COPY . /cranium
RUN cd /cranium && R CMD INSTALL .

VOLUME ["/var/lib/cranium"]
EXPOSE 8000

ENTRYPOINT ["Rscript", "/cranium/entrypoint.R"]
STOPSIGNAL 9
