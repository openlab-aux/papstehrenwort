package main

import (
	"encoding/json"
	"fmt"
	"github.com/BurntSushi/toml"
	gmail "github.com/jpoehls/gophermail"
	"html/template"
	"io/ioutil"
	"log"
	"net/http"
	"net/mail"
	"net/smtp"
	"os"
	"os/signal"
	"time"
)

type Task struct {
	Name        string
	Description string
	Frequency   time.Duration
	Users       []*User
	Changes chan<- TaskChange `json:"-"`
}
type User mail.Address
type TaskList map[string]*Task

const (
	tasklist_template = "ui_template.html"
)

type TaskChange int

const (
	userAdded = iota
	userDeleted
)

type config struct {
	Mail mailConfig
}
type mailConfig struct {
	Identity    string
	Username    string
	Password    string
	Host        string
	FromAddress string
}

var conf config

func main() {
	// FIXME: Add config file
	configFile := "config.toml"
	_, err := toml.DecodeFile(configFile, &conf)
	logFatal(err)

	file := "tasks.json"
	tasks := loadFromJson(file)
	defer saveToJson(file, tasks)

	go uiServer(8080, tasks)
	// TODO name in task and tasklist?!
	// What about duplication?
	// What about the children‽ Think about the children!
	// TODO test (mail) config at startup (so errors won’t be thrown after
	// some time but instantly)
	for _, t := range tasks {
		go schedule(t, &conf.Mail)
	}

	sig := make(chan os.Signal)
	signal.Notify(sig, os.Interrupt)
	select {
	case <-sig:
		saveToJson(file, tasks)
		fmt.Println("\nExiting …")
	}
}

// uiServer serves the GUI-frontend in which popes can sign up for tasks.
func uiServer(port int, tasks TaskList) {
	http.Handle("/", tasks)
	http.HandleFunc("/static/", func(w http.ResponseWriter, r *http.Request) {
		http.ServeFile(w, r, r.URL.Path[1:])
	})
	http.ListenAndServe(fmt.Sprintf(":%d", port), nil)

}

// ServeHTTP displays the tasks as a table and offers a form to submit the
// selection.
func (tasks TaskList) ServeHTTP(w http.ResponseWriter, req *http.Request) {
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
			for taskname, _ := range tasks {
				if req.Form[taskname] != nil {
					if req.Form["name"] != nil && req.Form["email"] != nil {
						// adding foo, stay tuned
					}
				}
			}
			http.Redirect(w, req, "/", http.StatusFound)
		}
	}
}

// Schedule tracks a task and sends mails the its popes.
// TODO: test reminders
// TODO: changes to users (userAdded, userDeleted)
func schedule(task *Task, mailConf *mailConfig) {
	t := time.NewTicker(task.Frequency)
	for {
		<-t.C
		log.Printf("%s fired", task.Name)
		for _, u := range task.Users {
			fromAddress := mailConf.FromAddress
			mail, err := reminderMail(task, u, fromAddress)
			log.Printf("Mail created for user %s", u.Name)
			if err != nil {
				//TODO: check mail sanity sooner?!
				logFatal(err)
			}
			log.Printf("Sending mail to %s …", u.Address)

			tmpstr, err := mail.Bytes()
			if err != nil {
				logFatal(err)
			}
			log.Printf("This mail:\n%s", tmpstr)
			err = mailConf.sendMail(mail)
			log.Printf("Sent mail!")
			if err != nil {
				//TODO
				logFatal(err)
			}
		}
	}
}

// sendMail connects to the SMTP server supplied in mc and sends an email.
func (mc *mailConfig) sendMail(msg *gmail.Message) error {
	auth := smtp.PlainAuth(mc.Identity, mc.Username, mc.Password, mc.Host)
	return gmail.SendMail(mc.Host, auth, msg)
}

// reminderMail constructs a message to remind the user of a task due task.
func reminderMail(t *Task, u *User, fromAddress string) (*gmail.Message, error) {
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
