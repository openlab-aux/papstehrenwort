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
)



type Task struct {
	Title       string // id
	Description string
	Frequency   time.Duration
	Users       []User // already a list (future feature)
}

type User mail.Address

func main() {
	file := "tasks.json"
	tasks := loadFromJson(file)
	defer saveToJson(file, tasks)

	go uiServer(8080)

	sig := make(chan os.Signal)
	signal.Notify(sig, os.Interrupt)
	    select {
	    case <-sig:
		    fmt.Println("\nExiting …")
	    }
}

func loadFromJson(file string) *[]Task {
	b, err := ioutil.ReadFile(file)
	if err != nil {
		l := append(make([]Task, 0), Task{"Hello", "This is Olaf.", 100, nil})
		return &l
	}
	var tasks []Task
	err = json.Unmarshal(b, &tasks)
	logFatal(err)
	return &tasks
}

func saveToJson(file string, tasks *[]Task) {
	b, err := json.Marshal(tasks)
	logFatal(err)
	err = ioutil.WriteFile(file, b, 0644)
	logFatal(err)
}

func uiServer(port int) {
	return
}

func logFatal(err error) {
	if err != nil {
		log.Fatal(err)
	}
}
