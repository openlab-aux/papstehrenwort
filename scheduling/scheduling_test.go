package scheduling

import (
	"github.com/openlab-aux/papstehrenwort/server"
	"testing"
	"time"
)

func TestScheduling(t *testing.T) {
	tick := 5 * time.Millisecond //5ms should be enough
	task := &server.Task{
		Name:        "something",
		Description: "",
		Frequency:   tick,
		Users:       []*server.User{},
	}
	rem, _ := Schedule(task)

	// test scheduling multiple times
	for i := 0; i < 5; i++ {
		timer := time.After(tick + tick/10)
		select {
		case <-timer:
			t.Errorf("task wasn’t scheduled after %s", tick)
		case <-rem:
			continue
		}
	}

}
