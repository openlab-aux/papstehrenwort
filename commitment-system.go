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
	"html/template"
)


type Task struct {
	Description string
	Frequency   time.Duration
	Users       []User // already a list (future feature)
}
type User mail.Address

type globalContext struct {
	request  chan request
	response chan map[string]Task
}
type request int
const (
	allTasks = iota
)

func main() {

	ctx := globalContext{
		request: make(chan request),
		response: make(chan map[string]Task),
	}
	go uiServer(8080, &ctx)
	go handleGlobalContext(&ctx)

	sig := make(chan os.Signal)
	signal.Notify(sig, os.Interrupt)
	    select {
	    case <-sig:
		    fmt.Println("\nExiting …")
	    }
}

func handleGlobalContext(ctx *globalContext) {
	file := "tasks.json"
	tasks := loadFromJson(file)
	defer saveToJson(file, tasks)

	for {
		// TODO
		if req := <-ctx.request; req == allTasks {
			ctx.response <- map[string]Task{
				"benis": Task{"huehue", 1337, []User{User{"Spurdo", "spurdo@spärde.de"}}},
			}
		}
	}
}

func uiServer(port int, ctx *globalContext) {
	http.HandleFunc("/", ctx.handleHome)

	http.ListenAndServe(fmt.Sprintf(":%d", port), nil)

}


func (ctx globalContext) handleHome(w http.ResponseWriter, req *http.Request) {
	// TODO: global generation of the template, once?
	switch req.Method {
	case "GET":
		t, err := template.ParseFiles("ui_template.html")
		logFatal(err)
		ctx.request <- allTasks
		err = t.Execute(w, <-ctx.response)
	case "POST":
	}
	
	
}


func loadFromJson(file string) *map[string]Task {
	b, err := ioutil.ReadFile(file)
	if err != nil {
		l := make(map[string]Task)
		return &l
	}
	var tasks map[string]Task
	err = json.Unmarshal(b, &tasks)
	logFatal(err)
	return &tasks
}

func saveToJson(file string, tasks *map[string]Task) {
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
