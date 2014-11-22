// Package scheduling implements scheduling of tasks and reacting to
// scheduling-related changes.
// No IO is done in this module.
package scheduling

import (
	"github.com/openlab-aux/papstehrenwort/server"
	"time"
)

type Reminder int

const (
	due = iota
)

// Schedule tracks a task, accepts changes to it and returns reminders
// on a channel.
// TODO: changes to users (userAdded, userDeleted)
func Schedule(task *server.Task) (<-chan Reminder, chan<- server.TaskChange) {
	rem := make(chan Reminder)
	go func() {
		for {
			<-time.After(task.Frequency)
			rem <- due
		}
	}()
	return rem, nil

	// FIXME needs to be inside another module
}
