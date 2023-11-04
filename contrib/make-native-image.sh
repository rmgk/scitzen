#!/bin/sh
sbt --client stageJars
native-image --class-path "target/jars/*" scitzen.cli.ScitzenCommandline
