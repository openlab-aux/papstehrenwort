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
