Write script that executes

R golem::add_dockerfile_with_renv(
  lockfile = "renv.lock",
  output_dir = "docker"
)

Runs docker build command

pushes latest image to docker hub

deploys to render