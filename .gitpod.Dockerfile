FROM gitpod/workspace-full
# Install SBT and prep Scala env
USER gitpod
RUN brew install scala sbt
