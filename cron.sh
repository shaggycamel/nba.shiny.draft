#!/bin/bash

# Remember to chmod +x cron.sh on nuc after pulling latest file

# ── Config ────────────────────────────────────────────────────────────────────

set -euo pipefail

# Work from the repo root regardless of the invoking cwd
cd "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Deploy secrets. Resolved after the cd so it is the repo's own file: the previous
# version sourced ./.profile before cd-ing, so under cron (cwd=$HOME) it read
# ~/.profile, and with no [ -f ] guard it died outright when that was missing.
# A terminal means a human is driving, so values they already exported win.
if [ ! -t 1 ]; then
    for f in ./.deploy.env ./.profile "$HOME/.config/scs_deploy.env"; do
        if [ -f "$f" ]; then
            . "$f"
            break
        fi
    done
fi

# Variables
DOCKERHUB_USER="${DOCKERHUB_USER:-shaggycamel}"
IMAGE_NAME="scs.nba.fty.league_draft"
TAG="${TAG:-latest}"

# Single-image mode: one image, one HuggingFace space.
HF_SPACE="${HF_SPACE:-shaggycamel/scs-nba-fty-league-draft}"

DOCKERHUB_TOKEN="${DOCKERHUB_TOKEN:?DOCKERHUB_TOKEN not set}"
HUGGINGFACE_TOKEN="${HUGGINGFACE_TOKEN:?HUGGINGFACE_TOKEN not set}"

# Custom function for messages
step() { printf "\n▶ %s\n\n" "$*"; }

# ── Log in to Docker Hub (once) ─────────────────────────────────────────────
step "Logging in to Docker Hub..."
echo "$DOCKERHUB_TOKEN" | docker login -u "$DOCKERHUB_USER" --password-stdin

# ── Base image check (built externally, cron does not build it) ─────────────
step "Checking base image..."
if ! docker image inspect scs.nba.fty.league_draft_base:latest >/dev/null 2>&1; then
    printf "✘ scs.nba.fty.league_draft_base:latest not found — build it before running cron\n" >&2
    exit 1
fi

# ── Single-image build/deploy ────────────────────────────────────────────────
FULL_IMAGE="$DOCKERHUB_USER/$IMAGE_NAME:$TAG"

step "Cleaning previous build artifacts..."
rm -f ./data-raw/*.rda ./*.tar.gz docker/*.tar.gz

step "Regenerating data..."
Rscript ./data-raw/_generate_all.R

step "Building R package tarball..."
R CMD build .

step "Building Docker image: $FULL_IMAGE..."
docker build -f ./docker/Dockerfile -t "$FULL_IMAGE" .

step "Pushing $FULL_IMAGE to Docker Hub..."
docker push "$FULL_IMAGE"

step "Triggering HuggingFace rebuild for $HF_SPACE..."
HTTP_STATUS=$(curl -s -o /dev/null -w '%{http_code}' -X POST \
  "https://huggingface.co/api/spaces/$HF_SPACE/restart?factory=true" \
  -H "Authorization: Bearer $HUGGINGFACE_TOKEN")
if [ "$HTTP_STATUS" -lt 200 ] || [ "$HTTP_STATUS" -ge 300 ]; then
    printf "✘ HuggingFace restart failed for %s (HTTP %s)\n" "$HF_SPACE" "$HTTP_STATUS" >&2
    exit 1
fi
printf "✔ HuggingFace rebuild triggered for %s (HTTP %s)\n" "$HF_SPACE" "$HTTP_STATUS"

printf "\n✔ Single image processed\n"
