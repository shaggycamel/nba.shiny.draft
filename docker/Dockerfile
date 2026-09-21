FROM shaggycamel/nba.shiny.draft_base
COPY renv.lock renv.lock
RUN R -e 'options(renv.config.pak.enabled = FALSE);renv::restore(prompt=FALSE)'
COPY nba.shiny.draft_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz",upgrade="never")'
# RUN rm /app.tar.gz
ENV PORT=3838
EXPOSE 3838
USER rstudio
CMD ["Rscript", "-e", "options(shiny.host='0.0.0.0', shiny.port=as.integer(Sys.getenv('PORT','3838'))); library(nba.shiny.draft); nba.shiny.draft::run_app()"]