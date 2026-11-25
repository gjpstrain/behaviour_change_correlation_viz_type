FROM rocker/verse:4.5.3

WORKDIR /home/rstudio

# Copy renv.lock
COPY --chown=rstudio:rstudio renv.lock ./renv.lock

# Install renv and restore packages
RUN R -e "install.packages('renv')"
RUN R -e "renv::restore(prompt = FALSE)"

COPY _extensions/ ./_extensions/
COPY additional_scripts/ ./additional_scripts/
COPY graphics/ ./graphics/
COPY stimuli/ ./stimuli/
COPY behaviour_change_correlation_viz_type_cache/ ./behaviour_change_correlation_viz_type_cache/
COPY data/ ./data/
COPY donation_behaviour.bib ./
COPY behaviour_change_correlation_viz_type.qmd ./
COPY elsarticle-harv.bst ./
COPY elsarticle.cls ./