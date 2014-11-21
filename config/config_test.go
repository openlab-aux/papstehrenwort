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
