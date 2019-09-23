all: main.js bosatsu.js

main.js: src/Main.elm
	elm make src/Main.elm --optimize --output out/main.js

bosatsu.js: ../jsapi/.js/target/scala-2.12/bosatsu-jsapi-fastopt.js
	cp ../jsapi/.js/target/scala-2.12/bosatsu-jsapi-fastopt.js out/bosatsu.js
