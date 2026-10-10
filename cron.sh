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
HF_SPACE="${HF_SPACE:-shaggycamel/draft}"

DOCKERHUB_TOKEN="${DOCKERHUB_TOKEN:?DOCKERHUB_TOKEN not set}"
HUGGINGFACE_TOKEN="${HUGGINGFACE_TOKEN:?HUGGINGFACE_TOKEN not set}"

# R work runs inside the base image, which already carries every renv package.
# The host has no R library for this project (and never has), so Rscript here dies
# on the first library() call.
REPO_DIR="$PWD"
R_IMAGE="${R_IMAGE:-scs.nba.fty.league_draft_base:latest}"
CREDS_FILE="${CREDS_FILE:-${SCS_HUB_CREDENTIALS:-$HOME/.config/scs_hub_credentials.ini}}"

# Custom function for messages
step() { printf "\n▶ %s\n\n" "$*"; }

run_r() {
  # docker silently creates a *directory* at a missing bind path, which then shows
  # up as a baffling R error, so check first.
  if [ ! -f "$CREDS_FILE" ]; then
    printf '✘ credentials file not found: %s\n' "$CREDS_FILE" >&2
    return 1
  fi
  docker run --rm \
    -e HOME=/root \
    -e RENV_CONFIG_AUTOLOADER_ENABLED=FALSE \
    -e NBA_DB_SOURCE="${NBA_DB_SOURCE:-cockroach-read}" \
    -v "$REPO_DIR:/work" \
    -v "$CREDS_FILE:/root/.config/scs_hub_credentials.ini:ro" \
    -w /work \
    "$R_IMAGE" "$@"
}

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

# The container writes as root, so the cleanup has to happen in there too: the host
# user cannot remove root-owned files.
step "Cleaning previous build artifacts..."
run_r sh -c 'rm -f ./data-raw/*.rda ./*.tar.gz docker/*.tar.gz'

step "Regenerating data..."
run_r Rscript ./data-raw/_generate_all.R

step "Building R package tarball..."
# R CMD build copies the tree *before* it applies .Rbuildignore, and .profile is a
# symlink to a file outside the mount, so in here it dangles and the copy dies
# ("cannot stat work/.profile"). Build from a throwaway copy with the deploy
# dotfiles removed - they must never reach the tarball anyway - then bring the
# tarball back into the mounted repo.
run_r sh -c 'rm -rf /tmp/pkgbuild && mkdir -p /tmp/pkgbuild && cp -a /work/. /tmp/pkgbuild/ \
  && rm -f /tmp/pkgbuild/.profile /tmp/pkgbuild/.deploy.env /tmp/pkgbuild/.Renviron \
  && cd /tmp/pkgbuild && R CMD build . && cp /tmp/pkgbuild/*.tar.gz /work/'

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

# Hand the generated artifacts back to the invoking host user, so the working tree
# is not left holding files the host cannot manage.
step "Restoring ownership to the host user..."
run_r sh -c "chown -R $(id -u):$(id -g) /work"

printf "\n✔ Single image processed\n"
