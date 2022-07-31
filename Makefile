graal_config:
	rm -rf Examples/scitzen.cache
	rm -rf Examples/scitzen.out
	rm -rf cli/src/main/resources/META-INF
	cs launch sbt --jvm graalvm-java17:22.2.0 -- 'set javaOptions += "-agentlib:native-image-agent=config-output-dir=src/main/resources/META-INF/native-image"' "run Examples --image-file-map imagemap.json"
	rm imagemap.json
