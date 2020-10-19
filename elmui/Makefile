all: main.js bosatsu.js bosatsu-jsapi-opt.js.map

main.js: src/Main.elm
	elm make src/Main.elm --optimize --output out/main.js

../jsapi/.js/target/scala-2.12/bosatsu-jsapi-opt.js:
	cd .. && sbt jsapiJS/fullOptJS && cd elmui

bosatsu.js: ../jsapi/.js/target/scala-2.12/bosatsu-jsapi-opt.js
	cp ../jsapi/.js/target/scala-2.12/bosatsu-jsapi-opt.js out/bosatsu.js

bosatsu-jsapi-opt.js.map: ../jsapi/.js/target/scala-2.12/bosatsu-jsapi-opt.js.map
	cp ../jsapi/.js/target/scala-2.12/bosatsu-jsapi-opt.js.map out/

format: src/*
	elm-format --yes src/
