package main

import (
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
	config, _ := loadConfig(minimalTomlMailConfig)
	//default fields
	if config.Mail.Port != "smtp" {
		t.Errorf("default port != smtp (is: %s)", config.Mail.Port)
	}
	if config.Mail.Identity != "" {
		t.Error("default identity not empty string")
	}
}

func TestSendMail(t *testing.T) {
}
