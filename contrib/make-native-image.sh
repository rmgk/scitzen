#!/bin/sh
sbt --client stageJars
# we have this dependency due to some JS compat, just remove it here
rm target/jars/graal-sdk-*.jar
rm target/jars/truffle-api-*.jar
rm target/jars/js-*.jar
native-image --class-path "target/jars/*" --gc=G1 scitzen.cli.ScitzenCommandline
