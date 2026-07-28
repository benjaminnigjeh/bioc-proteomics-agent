# Dockerfile
#
# Bioconductor Proteomics Agent -- localhost-only Shiny application.
# Base image ships R + Bioconductor already configured for the correct
# release, which keeps package resolution fast and reproducible.

FROM bioconductor/bioconductor_docker:RELEASE_3_19

LABEL org.opencontainers.image.title="Bioconductor Proteomics Agent" \
      org.opencontainers.image.description="Multi-agent, Claude-powered R Shiny app for MS proteomics (localhost only)."

ENV RUNNING_IN_DOCKER=true \
    SHINY_PORT=3838 \
    DEBIAN_FRONTEND=noninteractive

# System dependencies: mzR/Spectra need netCDF + XML + SSL build headers;
# curl is used by the container HEALTHCHECK.
RUN apt-get update && apt-get install -y --no-install-recommends \
      libnetcdf-dev \
      libxml2-dev \
      libssl-dev \
      libcurl4-openssl-dev \
      zlib1g-dev \
      curl \
      pandoc \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Install R dependencies first so this layer is cached across app-code
# changes that don't touch the dependency list.
COPY scripts/install_packages.R scripts/install_packages.R
RUN Rscript scripts/install_packages.R

# Now copy the rest of the application.
COPY . .

# Generate the small bundled demo dataset (mzML + CSV tables) using the
# same Spectra/mzR libraries the app itself uses to read them back.
RUN Rscript scripts/generate_demo_data.R

# Non-root runtime user; writable upload/output directories.
RUN useradd --create-home --shell /bin/bash appuser \
    && mkdir -p /app/local-data /app/outputs \
    && chown -R appuser:appuser /app

USER appuser

EXPOSE 3838

HEALTHCHECK --interval=30s --timeout=5s --start-period=60s --retries=5 \
  CMD curl -fsS http://127.0.0.1:3838/ || exit 1

CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]
