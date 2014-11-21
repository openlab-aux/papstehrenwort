package main

import (
	"encoding/json"
	"fmt"
	"github.com/BurntSushi/toml"
	"github.com/openlab-aux/papstehrenwort/reminders"
	"github.com/openlab-aux/papstehrenwort/scheduling"
	"github.com/openlab-aux/papstehrenwort/server"
	"io/ioutil"
	"log"
	"os"
	"os/signal"
)

const (
	userAdded = iota
	userDeleted
)

type config struct {
	Mail reminders.MailConfig
}

var conf config

func main() {
	configString, err := ioutil.ReadFile("config.toml")
	logFatal(err)
	conf, err := loadConfig(string(configString))
	logFatal(err)
	file := "tasks.json"
	tasks := loadFromJson(file)
	defer saveToJson(file, tasks)

	go server.UI(8080, tasks)
	// TODO name in task and tasklist?!
	// What about duplication?
	// What about the children‽ Think about the children!
	// TODO test (mail) config at startup (so errors won’t be thrown after
	// some time but instantly)
	for _, t := range tasks {
		go scheduling.Schedule(t, &conf.Mail)
	}

	sig := make(chan os.Signal)
	signal.Notify(sig, os.Interrupt)
	select {
	case <-sig:
		saveToJson(file, tasks)
		fmt.Println("\nExiting …")
	}
}

// loadConfig puts the given toml string into a config struct
func loadConfig(f string) (c *config, err error) {
	_, err = toml.Decode(f, &c)

	// defaults
	if c.Mail.Port == "" {
		c.Mail.Port = "smtp"
	}
	return
}

func loadFromJson(file string) server.TaskList {
	b, err := ioutil.ReadFile(file)
	if err != nil {
		l := make(server.TaskList)
		return l
	}
	var tasks server.TaskList
	err = json.Unmarshal(b, &tasks)
	logFatal(err)
	return tasks
}

func saveToJson(file string, tasks server.TaskList) {
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
