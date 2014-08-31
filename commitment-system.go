package main

import (
	"fmt"
	"time"
	"net/mail"
	"encoding/json"
	"os"
	"os/signal"
	"log"
	"io/ioutil"
	"net/http"
)


type Task struct {
	Description string
	Frequency   time.Duration
	Users       []User // already a list (future feature)
}
type User mail.Address
type TaskList map[string]Task

type request int
const (
	allTasks = iota
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
		    fmt.Println("\nExiting …")
	    }
}

func uiServer(port int, tasks *TaskList) {
	http.Handle("/", tasks)

	http.ListenAndServe(fmt.Sprintf(":%d", port), nil)

}


func (tasks *TaskList) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	switch req.Method {
	case "GET":
		b, err := json.Marshal(tasks)
		if err != nil {
			log.Fatal(err)
			http.Error(w, "Error encoding JSON.", 500)
			return
		}
		w.Header()["Content-Type"] = []string{"text/json"}
		w.Write(b)
	case "POST":
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
