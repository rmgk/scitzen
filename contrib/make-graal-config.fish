#!/usr/bin/env fish

rm -rf Examples/scitzen.cache
rm -rf Examples/scitzen.out
rm -rf src/main/resources/META-INF/native-image/generated
set JAVA_HOME (cs java-home --jvm graalvm-java19:22.3.1)
$JAVA_HOME/bin/gu install native-image
$JAVA_HOME/bin/gu install js
cs launch sbt -- 'set fork := true' 'set javaOptions += "-agentlib:native-image-agent=config-output-dir=src/main/resources/META-INF/native-image/generated"' "run Examples --image-file-map imagemap.json"
rm imagemap.json
