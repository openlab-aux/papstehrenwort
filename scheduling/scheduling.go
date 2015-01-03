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

// Package scheduling implements scheduling of tasks and reacting to
// scheduling-related changes.
// No IO is done in this module.
package scheduling

import (
	"github.com/openlab-aux/papstehrenwort/server"
	"time"
)

type Reminder struct {
	//TODO what information is needed?
}

// Schedule tracks a task, accepts changes to it and returns reminders
// on a channel.
// TODO: changes to users (userAdded, userDeleted)
func Schedule(task *server.Task) (<-chan Reminder, chan<- server.TaskChange) {
	rem := make(chan Reminder)
	chg := make(chan server.TaskChange)
	go func() {
		for {
			select {
			case _, open := <-chg:
				if !open {
					close(rem)
					return
				}
			case <-time.After(task.Frequency):
				rem <- Reminder{}
			}
		}
	}()
	return rem, chg
}
