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
    cmake \
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

# KEY FIX: Disable the global cache so packages install directly
# into the project library (renv/library/) instead of symlinks to a cache.
# This ensures packages survive into the runtime container.
ENV RENV_CONFIG_CACHE_ENABLED=FALSE

# Restore all packages - they now live in /app/renv/library/
RUN R -e "source('renv/activate.R'); renv::restore(prompt = FALSE)"

# ---------- Verify packages installed correctly ----------
RUN R -e "library(shiny); cat('shiny version:', as.character(packageVersion('shiny')), '\n')"

# ---------- Skip slow renv checks at RUNTIME only ----------
# These only affect container startup speed
# They do NOT affect package installation during build
ENV RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE
ENV RENV_CONFIG_SANDBOX_ENABLED=FALSE

# ---------- Copy application code ----------
COPY app.R app.R
COPY rhino.yml rhino.yml
COPY config.yml config.yml
COPY dependencies.R dependencies.R
COPY CHANGELOG.md CHANGELOG.md
COPY app/ app/
COPY www/ www/

# ---------- Runtime configuration ----------
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
