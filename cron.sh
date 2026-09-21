#!/bin/bash

# Remember to chmod +x cron.sh on nuc after pulling latest file

# ── Config ────────────────────────────────────────────────────────────────────

# If executing from cron source .profile (containing tokens)
if [ ! -t 1 ]; then
    source ./.profile
fi

# Directory
# on dev (mac) this is ./github/nba.shiny.draft/nba.shiny.draft
cd ./github/nba.shiny.draft

# Variables
DOCKERHUB_USER="${DOCKERHUB_USER:-shaggycamel}"
IMAGE_NAME="nba.shiny.draft"
TAG="${TAG:-latest}"
FULL_IMAGE="$DOCKERHUB_USER/$IMAGE_NAME:$TAG"
HUGGINGFACE_TOKEN="$HUGGINGFACE_TOKEN"

# Custom function for messages
step() { printf "\n▶ %s\n\n" "$*"; }

set -e # Exit immediately on error

# ── Log in to Docker Hub ────────────────────────────────────────────────────
step "Logging in to Docker Hub..."
echo "$DOCKERHUB_TOKEN" | docker login -u "$DOCKERHUB_USER" --password-stdin

# ── Clean & Build ───────────────────────────────────────────────────────────
step "Cleaning previous build artifacts..."
rm -f ./data-raw/*.rda ./*.tar.gz

step "Regenerating data..."
Rscript ./data-raw/_generate_all.R

step "Building R package tarball..."
R CMD build .

step "Building Docker image: $FULL_IMAGE..."
docker build -f ./docker/Dockerfile -t "$FULL_IMAGE" .

step "Pushing $FULL_IMAGE to Docker Hub..."
docker push "$FULL_IMAGE"

step "Triggering Huggingface rebuild..."
curl -sf -X POST \
  "https://huggingface.co/api/spaces/shaggycamel/nba-shiny-draft/restart?factory=true" \
  -H "Authorization: Bearer $HUGGINGFACE_TOKEN"

printf "\n✔ Deployment complete\n"