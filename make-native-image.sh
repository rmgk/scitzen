#!/bin/sh
sbt writeClasspath
native-image @target/classpath.txt scitzen.cli.ScitzenCommandline
