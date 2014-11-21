package scheduling

import (
	"github.com/openlab-aux/papstehrenwort/reminders"
	"github.com/openlab-aux/papstehrenwort/server"
	"log"
	"time"
)

// Schedule tracks a task and sends mails the its popes.
// TODO: test reminders
// TODO: changes to users (userAdded, userDeleted)
func Schedule(task *server.Task, mailConf *reminders.MailConfig) {
	t := time.NewTicker(task.Frequency)
	for {
		<-t.C
		for _, u := range task.Users {
			fromAddress := mailConf.FromAddress
			mail, err := reminders.CreateMail(task, u, fromAddress)
			log.Printf("Mail created for user %s", u.Name)
			if err != nil {
				//should not throw any error, since
				// the config gets checked
				log.Fatalf("%s\nThis should NEVER trigger!", err)
			}
			log.Printf("Sending mail to %s …", u.Address)

			err = mailConf.SendMail(mail)
			if err != nil {
				//TODO
				//FIXME
				log.Fatal(err)
			}
			log.Printf("Sent mail!")
		}
	}
}
