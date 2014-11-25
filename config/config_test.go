// © 2014 OpenLab Augsburg e.V. and contributors (see CONTRIBUTORS).
//
// This file is part of Papstehrenwort.
//
// Papstehrenwort is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Papstehrenwort is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with Papstehrenwort.  If not, see <http://www.gnu.org/licenses/>.

package config

import (
	"fmt"
	"testing"
)

var minimalTomlMailConfig = `
[mail]
fromaddress = "dummy@example.com"
username = "dummy"
password = "mypass"
host = "smtp.example.com"
`

func TestToml(t *testing.T) {
	//default fields
	c := minimalConfig()
	if c.Mail.Port != "smtp" {
		t.Errorf("default port != smtp (is: %s)", c.Mail.Port)
	}
	if c.Mail.Identity != "" {
		t.Error("default identity not empty string")
	}
}

func minimalConfig() *C {
	c, _ := LoadConfig(minimalTomlMailConfig)
	return c
}

func ExampleMailConfigTest() {
	mc := minimalConfig().Mail
	mc.FromAddress = "BADF00D"
	fmt.Println(mc.TestMailConfig())
	//Output: config[mail]: malformed fromaddress (BADF00D)
}
