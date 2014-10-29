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

const (
	tasklist_template = "ui_template.html"
)

type Task struct {
	Description string
	Frequency   time.Duration
	Users       []User // already a list (future feature)
}
type User mail.Address
type TaskList map[string]Task

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

func uiServer(port int, tasks *TaskList) {
	http.Handle("/", tasks)
	http.HandleFunc("/static/", func(w http.ResponseWriter, r *http.Request) {
		http.ServeFile(w, r, r.URL.Path[1:])
	})
	http.ListenAndServe(fmt.Sprintf(":%d", port), nil)

}

func (tasks *TaskList) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	switch req.Method {
	case "GET":
		if req.URL.Path == "/" {
			w.Header()["Content-Type"] = []string{"text/html"}
			ts, err := ioutil.ReadFile(tasklist_template)
			logFatal(err)
			t := template.Must(template.New("tasklist").Parse(string(ts)))

			t.Execute(w, tasks)
		}

	case "POST":
		err := req.ParseForm()
		if err == nil {
			if req.URL.Path == "/commit" {
				/* What we get via POST:
				E-Mail: email
				Name: name
				each checked task is transmitted as one key
				(see for-loop below)
				req.Form looks like this:
				map[name:[sternenseemann] Foobar:[do] submit:[Commit] email:[foo@foo.de]]
				*/
				for taskname, _ := range *tasks {
					if req.Form[taskname] != nil {
						// TODO: check for existance of the fields
						fmt.Println("The User", req.Form["name"][0], "with email", req.Form["email"][0],
							"commited theirselves the task", taskname)
					}
				}
				http.Redirect(w, req, "/", http.StatusFound)
			}
		} else {
			http.Error(w, err.Error(), http.StatusBadRequest)
		}
	}

}

func loadFromJson(file string) *TaskList {
	b, err := ioutil.ReadFile(file)
	if err != nil {
		l := make(TaskList)
		return &l
	}
	var tasks TaskList
	err = json.Unmarshal(b, &tasks)
	logFatal(err)
	return &tasks
}

func saveToJson(file string, tasks *TaskList) {
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
