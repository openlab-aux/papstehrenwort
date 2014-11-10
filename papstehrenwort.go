package main

import (
	"encoding/json"
	"fmt"
	"html/template"
	"io/ioutil"
	"log"
	"net/http"
	"net/mail"
	"os"
	"os/signal"
	"time"
)

type Task struct {
	Description string
	Frequency   time.Duration
	Users       []User // already a list (future feature)
}
type User mail.Address
type TaskList map[string]*Task

const (
	tasklist_template = "templates/tasks.html"
	error_template    = "templates/error.html"
)

func main() {

	file := "tasks.json"
	tasks := loadFromJson(file)
	defer saveToJson(file, tasks)

	go uiServer(8080, tasks)
	// go handleGlobalContext(&ctx)

	sig := make(chan os.Signal)
	signal.Notify(sig, os.Interrupt)
	select {
	case <-sig:
		saveToJson(file, tasks)
		fmt.Println("\nExiting …")
	}
}

func uiServer(port int, tasks TaskList) {
	http.Handle("/", tasks)
	http.HandleFunc("/static/", func(w http.ResponseWriter, r *http.Request) {
		http.ServeFile(w, r, r.URL.Path[1:])
	})
	http.ListenAndServe(fmt.Sprintf(":%d", port), nil)

}

func (tasks TaskList) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	switch req.Method {
	case "GET":
		switch req.URL.Path {
		case "/":
			w.Header()["Content-Type"] = []string{"text/html"}
			ts, err := ioutil.ReadFile(tasklist_template)
			if err != nil {
				log.Fatal(err)
			}
			t := template.Must(template.New("tasklist").Parse(string(ts)))

			t.Execute(w, tasks)

		default:
			http.Error(w, "File not found", 404)
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
						var newPope User
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
			http.Error(w, "File not found", 404)
		}
	}
}

func loadFromJson(file string) TaskList {
	b, err := ioutil.ReadFile(file)
	if err != nil {
		l := make(TaskList)
		return l
	}
	var tasks TaskList
	err = json.Unmarshal(b, &tasks)
	logFatal(err)
	return tasks
}

func saveToJson(file string, tasks TaskList) {
	b, err := json.Marshal(tasks)

	logFatal(err)
	err = ioutil.WriteFile(file, b, 0644)
	logFatal(err)
}

func logFatal(err error) {
	if err != nil {
		log.Fatal(err)
	}
}
