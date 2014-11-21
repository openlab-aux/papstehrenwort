package reminders

import (
	"fmt"
	gmail "github.com/jpoehls/gophermail"
	"github.com/openlab-aux/papstehrenwort/server"
	"net/smtp"
)

type MailConfig struct {
	Identity    string
	Username    string
	Password    string
	Host        string
	Port        string
	FromAddress string
}

// reminderMail constructs a message to remind the user of a task due task.
func CreateMail(t *server.Task, u *server.User, fromAddress string) (*gmail.Message, error) {
	m := new(gmail.Message)
	if err := m.SetFrom(fromAddress); err != nil {
		return nil, err
	}
	if err := m.AddTo(u.Address); err != nil {
		return nil, err
	}
	//TODO list email headers (http://www.jamesshuggins.com/h/web1/list-email-headers.htm)
	//TODO Prefix config option
	m.Subject = fmt.Sprintf("Task Due: %s", t.Name)
	//TODO Generate mail text from a text/template
	m.Body = fmt.Sprintf(`Hya, %s!
The following task needs to be done as soon as possible:

%s
%s

Get to it, ninja!
`, u.Name, t.Name, t.Description)
	return m, nil
}

// sendMail connects to the SMTP server supplied in mc and sends an email.
func (mc *MailConfig) SendMail(msg *gmail.Message) error {
	auth := smtp.PlainAuth(mc.Identity, mc.Username, mc.Password, mc.Host)
	return gmail.SendMail(mc.Host+":"+mc.Port, auth, msg)
}
