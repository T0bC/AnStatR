# ---------- Base image: matches your renv.lock R version ----------
FROM rocker/r-ver:4.5.2

# ---------- System dependencies ----------
# Core build tools (C, C++, Fortran compilers + make)
# + igraph:    libglpk-dev, libxml2-dev, gfortran
# + curl:      libcurl4-openssl-dev
# + openssl:   libssl-dev
# + gdtools/ggiraph/svglite: libcairo2-dev, libfreetype6-dev, libfontconfig1-dev
# + ggiraph/png: libpng-dev
# + magick:    libmagick++-dev
# + textshaping: libharfbuzz-dev, libfribidi-dev
# + httpuv:    zlib1g-dev
# + stringi:   libicu-dev
# + DataExplorer/rmarkdown: pandoc
# + summarytools: tcl8.6, tk8.6 (+ dev headers for tcltk.so)
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    gfortran \
    libglpk-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libcairo2-dev \
    libfreetype6-dev \
    libfontconfig1-dev \
    libpng-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libmagick++-dev \
    zlib1g-dev \
    libicu-dev \
    pandoc \
    tcl8.6 \
    tk8.6 \
    tcl8.6-dev \
    tk8.6-dev \
    && rm -rf /var/lib/apt/lists/*

# ---------- Set up app directory ----------
WORKDIR /app

# ---------- Restore R packages via renv (with BuildKit cache) ----------
# Copy only renv infrastructure first so Docker can cache this expensive layer.
# The layer only rebuilds when renv.lock or activate.R change.
COPY renv.lock renv.lock
COPY renv/activate.R renv/activate.R
COPY .Rprofile .Rprofile

# Configure renv to install into a project-local library
RUN mkdir -p renv/library

# Restore packages. The --mount=type=cache keeps a persistent renv cache across
# builds so that only NEW or CHANGED packages are compiled — not everything.
# Requires: DOCKER_BUILDKIT=1 (default on modern Docker)
ENV RENV_PATHS_CACHE=/renv_cache
RUN --mount=type=cache,target=/renv_cache \
    R -e "source('renv/activate.R'); renv::restore(prompt = FALSE)"

# ---------- Copy application code ----------
COPY app.R app.R
COPY rhino.yml rhino.yml
COPY config.yml config.yml
COPY dependencies.R dependencies.R
COPY app/ app/
COPY www/ www/

# ---------- Runtime configuration ----------
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
