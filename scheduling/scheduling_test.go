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

package scheduling

import (
	"github.com/openlab-aux/papstehrenwort/server"
	"testing"
	"time"
)

func task(freq time.Duration) *server.Task {
	if freq == 0 {
		freq = time.Hour
	}
	return &server.Task{
		Name:        "something",
		Description: "",
		Frequency:   freq,
		Users:       []*server.User{},
	}
}

func TestSchedulingStop(t *testing.T) {
	ts := task(time.Second)
	rem, chg := Schedule(ts)
	close(chg)
	select {
	case _, open := <-rem:
		if open {
			t.Error("chg was not closed")
		}
	case <-time.After(5 * time.Millisecond):
		t.Error("should be stopped by now")
	}
}

func TestSchedulingMultipleTimes(t *testing.T) {
	//5ms should be enough
	tick := 5 * time.Millisecond
	rem, _ := Schedule(task(tick))
	n := 5
	for i := 0; i < n; i++ {
		timer := time.After(tick + time.Duration(int(tick)/(n+1)))
		select {
		case <-timer:
			t.Errorf("task wasn’t scheduled after %s", tick)
		case <-rem:
			continue
		}
	}
}

// func TestUserAdding(t *testing.T) {
// 	user := server.User{Name: "spurdo", Address: "spurdo@example.com"}
// 	cfg <- server.TaskChange{Kind: server.UserAdded, Val: user}
// }

// func TestUserDeleting(t *testing.T) {
// 	// TODO construct user here?
// 	rem, chg := Schedule(task(0))
// 	userdel := TaskChange{Kind: server.UserAdded, Val: }
// }

// func TestLastUserDeleted(t *testing.T) {
// }
