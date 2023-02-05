#!/bin/sh
sbt stageJars
native-image --class-path "target/jars/*" scitzen.cli.ScitzenCommandline
