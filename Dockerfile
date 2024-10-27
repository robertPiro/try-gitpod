FROM gitpod/workspace-full
# Install SBT and prep Scala env
USER root
RUN brew install scala sbt