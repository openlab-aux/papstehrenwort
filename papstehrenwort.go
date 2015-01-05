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

package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"github.com/openlab-aux/papstehrenwort/config"
	"github.com/openlab-aux/papstehrenwort/reminders"
	"github.com/openlab-aux/papstehrenwort/scheduling"
	"github.com/openlab-aux/papstehrenwort/server"
	"io/ioutil"
	"log"
	"os"
	"os/signal"
	"reflect"
)

var conf *config.C

func main() {
	// load config
	configString, err := ioutil.ReadFile("config.toml")
	logFatal(err)
	conf, err := config.LoadConfig(string(configString))
	logFatal(err)
	err = conf.Mail.TestMailConfig()
	logFatal(err)

	// load data
	file := "tasks.json"
	tasks := loadFromJson(file)
	defer saveToJson(file, tasks)

	// setup UI
	inputc := make(chan server.UserInput)
	uiInfo := server.UIInformation{Tasks: tasks, Input: inputc}
	go server.UI(8080, uiInfo)
	// react on user input
	// only place the user list is modified
	go applyUserInput(&tasks, inputc)

	for _, t := range tasks {
		rem, _ := scheduling.Schedule(t)
		go func(t *server.Task) {
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
		mail, err := reminders.CreateMail(*t, u, fromAddress)
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

// applyUserInput applies the changes made by user input to the TaskList.
// The user is added or removed from the tasks he specified
func applyUserInput(tasks *server.TaskList, inpc chan server.UserInput) {
	modTask := func(inpTask *server.Task, inpUser server.User, active bool) error {
		taskExists := false
		for _, task := range *tasks {
			if inpTask == task {
				taskExists = true
				userExists := false
				newUsers := task.Users
				for i, user := range newUsers {

					if reflect.DeepEqual(inpUser, user) {
						userExists = true
						if !active {
							u := task.Users
							// delete from slice
							u = u[:i+copy(u[i:], u[i+1:])]
						}
						break
					}
				}
				task.Users = newUsers
				if active && !userExists {
					u := task.Users
					u = append(u, inpUser)
				}
				break
			}
		}
		if !taskExists {
			return errors.New("task does not exist")
		}
		return nil
	}
	for {
		select {
		case inp := <-inpc:
			for task, active := range inp.Tasks {
				modTask(task, inp.User, active)
			}
		}
	}

}

func loadFromJson(file string) server.TaskList {
	b, err := ioutil.ReadFile(file)
	if err != nil {
		//TODO which length?
		length := 10
		l := make(server.TaskList, length)
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
