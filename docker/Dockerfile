FROM scs.nba.fty.league_draft_base:latest

# Re-run restore against the same lockfile. The base layer already populated the
# site library, so this is a cheap/cached safety check that keeps the app image
# self-consistent if the lockfile changes without rebuilding the base.
COPY renv.lock renv.lock
RUN R -e 'options(renv.config.pak.enabled = FALSE); renv::restore()'

# Install app source from the freshly built tarball
COPY draft_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz", upgrade = "never")' && rm /app.tar.gz

# Use the production golem config (inst/golem-config.yml)
ENV R_CONFIG_ACTIVE=production

# Health check
HEALTHCHECK --interval=30s --timeout=5s --start-period=10s \
    CMD curl -f http://localhost:3838/ || exit 1

EXPOSE 3838
USER rstudio
CMD R -e "options(shiny.port=3838, shiny.host='0.0.0.0'); draft::run_app()"
