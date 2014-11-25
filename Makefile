# © 2014 OpenLab Augsburg e.V. and contributors (see CONTRIBUTORS).
# 
# This file is part of Papstehrenwort.
# 
# Papstehrenwort is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# Papstehrenwort is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
# 
# You should have received a copy of the GNU Affero General Public License
# along with Papstehrenwort.  If not, see <http://www.gnu.org/licenses/>.

TEMP = tmp
BIN = $(TEMP)/bin
STATIC = static

BOWER = $(TEMP)/bower
NPM_INSTALL = PREFIX=$(TEMP) npm install -g
GEM_HOME = GEM_HOME=$(TEMP)

build: deps plumbing watch

clean:
	-rm -r $(STATIC)
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
