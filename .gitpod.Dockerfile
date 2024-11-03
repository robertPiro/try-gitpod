FROM gitpod/workspace-full
# Install SBT and prep Scala env
USER gitpod
RUN brew install sbt ammonite-repl
ENV JAVA_TOOL_OPTIONS=
