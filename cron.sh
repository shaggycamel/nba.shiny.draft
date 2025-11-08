# Navigate to nba.shiny.draft directory
cd ~/git/nba.shiny.draft

# Update data files
Rscript ./data-raw/_generate_all.R

# Feed app into a tar.gz file and generate dockerfiles
R -e 'golem::add_dockerfile_with_renv(lockfile = "renv.lock", output_dir = "docker", open = FALSE)'

# Navigate to docker directory
cd ./docker

# Build docker image
docker build --platform linux/amd64 -t shaggycamel/nba.shiny.draft:latest .

# push latest image to docker hub
docker push shaggycamel/nba.shiny.draft:latest

# deploy to render
curl -X POST "https://api.render.com/v1/services/srv-d478fqe3jp1c73brg450/deploys" \
     -H "Authorization: Bearer $RENDER_API_KEY" \
     -H "Content-Type: application/json" \
     -d '{"image":"docker.io/shaggycamel/nba.shiny.draft:latest"}'
