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

package server

import (
	"fmt"
	"html/template"
	"io/ioutil"
	"log"
	"net/http"
	"net/mail"
	"time"
)

//FIXME make configurable
const (
	tasklist_template = "templates/tasks.html"
	error_template    = "templates/error.html"
)

type Task struct {
	Name        string
	Description string
	Frequency   time.Duration
	Users       []*User
}
type User mail.Address
type TaskChange int
type TaskList map[string]*Task

// UI serves the GUI-frontend in which popes can sign up for tasks.
func UI(port int, tasks TaskList) {
	http.Handle("/", tasks)
	http.HandleFunc("/static/", func(w http.ResponseWriter, r *http.Request) {
		http.ServeFile(w, r, r.URL.Path[1:])
	})
	http.ListenAndServe(fmt.Sprintf(":%d", port), nil)

}

// ServeHTTP displays the tasks as a table and offers a form to submit the
// selection.
func (tasks TaskList) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	switch req.Method {
	case "GET":
		switch req.URL.Path {
		case "/":
			w.Header()["Content-Type"] = []string{"text/html"}
			// FIXME only load once
			ts, err := ioutil.ReadFile(tasklist_template)
			if err != nil {
				// FIXME: no fatalities in this module!
				log.Fatal(err)
			}

			t := template.Must(template.New("tasklist").Parse(string(ts)))

			t.Execute(w, tasks)

		default:
			http.Error(w, "File not found", http.StatusNotFound)
		}

	case "POST":
		err := req.ParseForm()
		if err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		switch req.URL.Path {
		case "/commit":
			/* What we get via POST:
			E-Mail: email
			Name: name
			each checked task is transmitted as one key
			(see for-loop below)
			req.Form looks like this:
			map[name:[sternenseemann] Foobar:[do] submit:[Commit] email:[foo@foo.de]]
			*/
			if req.Form["name"][0] != "" && req.Form["email"][0] != "" {
				for taskname, task := range tasks {
					if req.Form[taskname] != nil {
						var newPope *User
						//FIXME(lukasepple) check email sanity!
						//has to be done on server-side, too
						//(because user input)
						newPope.Address = req.Form["email"][0]
						newPope.Name = req.Form["name"][0]
						task.Users = append(task.Users, newPope)
					}
				}
				http.Redirect(w, req, "/", http.StatusFound)
			} else {
				fmt.Println("The user did not fill out all the needed fields")
				w.Header()["Content-Type"] = []string{"text/html"}
				ts, err := ioutil.ReadFile(error_template)
				if err != nil {
					log.Fatal(err)
				}
				t := template.Must(template.New("error").Parse(string(ts)))
				t.Execute(w, "You did not fill out all needed fields!")
			}
		default:
			http.Error(w, "File not found", http.StatusNotFound)
		}
	}
}
