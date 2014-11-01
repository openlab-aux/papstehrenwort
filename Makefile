TEMP = tmp
BIN = $(TEMP)/bin
STATIC = static

BOWER = $(TEMP)/bower
NPM_INSTALL = PREFIX=$(TEMP) npm install -g
GEM_HOME = GEM_HOME=$(TEMP)

build: deps plumbing watch

clean:
	-rm $(STATIC)/bootstrap.min.css
	-rm $(STATIC)/jquery.min.js
	-rm watch
	-rm -r $(TEMP)
	-rm -r .sass-cache # cache location of command isn’t working right

watch:
	cp watch-template watch
	echo "$(BIN)/coffee --watch -o $(STATIC) js/*.coffee 2>&1 \
		| prepend \"coffee\" >> \$$TMP &" >> watch
	echo "$(GEM_HOME) $(BIN)/sass --scss \
		--cache-location=$(TEMP) \
		-I$(BOWER)/bootstrap-sass-official/assets/stylesheets/ \
		--watch sass:$(STATIC) 2>&1 \
		| prepend \"sass  \" >> \$$TMP &" >> watch
	echo "tail -f \$$TMP" >> watch
	chmod +x watch

plumbing:
	mkdir -p $(STATIC)
	ln -rsf $(BOWER)/jquery/dist/jquery.min.js $(STATIC)

deps: npm gem
	bower_directory=$(BOWER) $(BIN)/bower install

npm:
	$(NPM_INSTALL) bower
	$(NPM_INSTALL) coffee-script

gem:
	$(GEM_HOME) gem install -N -n $(TEMP)/bin sass
