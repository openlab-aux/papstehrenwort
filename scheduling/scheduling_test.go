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
