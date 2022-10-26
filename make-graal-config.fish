#!/usr/bin/env fish

rm -rf Examples/scitzen.cache
rm -rf Examples/scitzen.out
rm -rf cli/src/main/resources/META-INF
set -l GH (cs java-home --jvm graalvm-java17:22.3.0)
$GH/bin/gu install native-image
$GH/bin/gu install js
cs launch sbt --jvm graalvm-java17:22.3.0 -- 'set javaOptions += "-agentlib:native-image-agent=config-output-dir=sr/main/resources/META-INF/native-image"' "run Examples --image-file-map imagemap.json"
rm imagemap.json
