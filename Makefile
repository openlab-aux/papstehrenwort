TEMP = temp
STATIC = app/static
BOWER = $(TEMP)/bower

build: deps
	mkdir -p $(STATIC)
	ln -rsf $(BOWER)/bootstrap/dist/css/bootstrap.min.css $(STATIC)
	ln -rsf $(BOWER)/jquery/dist/jquery.min.js $(STATIC)

deps: npm
	bower_directory=$(BOWER) $(TEMP)/bin/bower install

npm:
	PREFIX=$(TEMP) npm install -g bower

clean:
	-rm $(STATIC)/bootstrap.min.css
	-rm $(STATIC)/jquery.min.js
	-rm -r $(TEMP)
