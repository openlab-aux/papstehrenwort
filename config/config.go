package config

import (
	"github.com/BurntSushi/toml"
	"github.com/openlab-aux/papstehrenwort/reminders"
)

type C struct {
	Mail reminders.MailConfig
}

// LoadConfig puts the given toml string into a config struct
func LoadConfig(f string) (c *C, err error) {
	_, err = toml.Decode(f, &c)

	// defaults
	if c.Mail.Port == "" {
		c.Mail.Port = "smtp"
	}
	return
}
