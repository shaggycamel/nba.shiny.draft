#!/bin/bash

# Remember to chmod +x cron.sh on nuc after pulling latest file
# Dry run (build only, no login/push/HF restart): DRY_RUN=1 ./cron.sh

# ── Config ────────────────────────────────────────────────────────────────────

# If executing from cron source .profile (containing tokens)
if [ ! -t 1 ]; then
    source ./.profile
fi

# Always prefer rig's R (4.5.2) over the apt-installed one in /usr/bin
export PATH="/usr/local/bin:$PATH"

# Strict mode goes after the profile, which wasn't written to survive -e/-u
set -euo pipefail

# Directory
# on dev (mac) this is ./github/nba.shiny.draft/nba.shiny.draft
cd ./github/nba.shiny.draft || exit 1

# R version guard
step "Checking R version..."
R_VER="$(Rscript -e 'cat(as.character(getRversion()))')"
LOCK_VER="$(sed -n 's/.*"Version": "\(4\.[0-9.]*\)".*/\1/p' renv.lock | head -1)"
[ "$R_VER" = "$LOCK_VER" ] || { echo "R $R_VER != lockfile $LOCK_VER"; exit 1; }

# Variables
DRY_RUN="${DRY_RUN:-0}"
DOCKERHUB_USER="${DOCKERHUB_USER:-shaggycamel}"
IMAGE_NAME="nba.shiny.draft"
TAG="${TAG:-latest}"
FULL_IMAGE="$DOCKERHUB_USER/$IMAGE_NAME:$TAG"
VERSION_TAG="$(date +%Y%m%d)-$(git rev-parse --short HEAD)"
VERSION_IMAGE="$DOCKERHUB_USER/$IMAGE_NAME:$VERSION_TAG"

# Fail now, not after a 10 minute build
if [ "$DRY_RUN" != 1 ]; then
    : "${DOCKERHUB_TOKEN:?DOCKERHUB_TOKEN not set}"
    : "${HUGGINGFACE_TOKEN:?HUGGINGFACE_TOKEN not set}"
fi

# Custom function for messages
step() { printf "\n▶ %s\n\n" "$*"; }

# ── Clean & Build ───────────────────────────────────────────────────────────
step "Cleaning previous build artifacts..."
rm -f ./*.tar.gz

step "Regenerating data..."
Rscript -e "renv::exec(source('./data-raw/_generate_all.R'))"

step "Building R package tarball..."
R CMD build --no-build-vignettes .

step "Building Docker image: $FULL_IMAGE ($VERSION_TAG)..."
docker build --pull -f ./docker/Dockerfile -t "$FULL_IMAGE" -t "$VERSION_IMAGE" .

if [ "$DRY_RUN" = 1 ]; then
    printf "\n✔ Dry run complete, nothing pushed\n"
    exit 0
fi

# ── Publish ─────────────────────────────────────────────────────────────────
step "Logging in to Docker Hub..."
echo "$DOCKERHUB_TOKEN" | docker login -u "$DOCKERHUB_USER" --password-stdin

step "Pushing $FULL_IMAGE and $VERSION_TAG to Docker Hub..."
docker push "$FULL_IMAGE"
docker push "$VERSION_IMAGE"

step "Triggering Huggingface rebuild..."
curl -sf -X POST \
  "https://huggingface.co/api/spaces/shaggycamel/nba-shiny-draft/restart?factory=true" \
  -H "Authorization: Bearer $HUGGINGFACE_TOKEN"

docker image prune -f > /dev/null

printf "\n✔ Deployment complete\n"