package main

import (
	"encoding/json"
	"fmt"
	"github.com/openlab-aux/papstehrenwort/config"
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

var conf *config.C

func main() {
	configString, err := ioutil.ReadFile("config.toml")
	logFatal(err)
	conf, err := config.LoadConfig(string(configString))
	logFatal(err)
	err = conf.Mail.TestMailConfig()
	logFatal(err)
	file := "tasks.json"
	tasks := loadFromJson(file)
	defer saveToJson(file, tasks)

	go server.UI(8080, tasks)

	for _, t := range tasks {
		rem, _ := scheduling.Schedule(t)
		go func(t *server.Task) {
			fmt.Println("foo")
			<-rem
			sendReminder(t, conf.Mail)
		}(t)
	}

	sig := make(chan os.Signal)
	signal.Notify(sig, os.Interrupt)
	select {
	case <-sig:
		saveToJson(file, tasks)
		fmt.Println("\nExiting …")
	}
}

func sendReminder(t *server.Task, mc reminders.MailConfig) {
	for _, u := range t.Users {
		fromAddress := mc.FromAddress
		mail, err := reminders.CreateMail(t, u, fromAddress)
		log.Printf("Mail created for user %s", u.Name)
		if err != nil {
			//should not throw any error, since
			// the config gets checked
			log.Fatalf("%s\nThis should NEVER trigger!", err)
		}
		log.Printf("Sending mail to %s …", u.Address)

		err = mc.SendMail(mail)
		if err != nil {
			//TODO retry?
			//FIXME
			log.Fatal(err)
		}
		log.Printf("Sent mail!")
	}
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
