#!/usr/bin/env fish

rm -rf Examples/scitzen.cache
rm -rf Examples/scitzen.out
rm -rf src/main/resources/META-INF/native-image/generated
cs launch sbt -- 'set fork := true' 'set javaOptions += "-agentlib:native-image-agent=config-output-dir=src/main/resources/META-INF/native-image/generated"' "run Examples --image-file-map imagemap.json"
rm imagemap.json
