FROM gcsxgsestimate_base
COPY renv.lock.prod renv.lock
RUN R -e 'renv::restore()'
COPY gcsxgsestimate_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz",upgrade="never")'
RUN rm /app.tar.gz
CMD R -e "options('shiny.port'=$PORT,shiny.host='0.0.0.0');gcsxgsestimate::run_app()"
