package main

import (
	"fmt"
	"time"
	"net/mail"
	// "encoding/json"
	"os"
	"os/signal"
)



type Task struct {
	title       string // id
	description string
	frequency   time.Duration
	users       []User // already a list (future feature)
}

type User mail.Address

func main() {
	// tasks := loadFromJson()

	go uiServer(8080)

	sig := make(chan os.Signal)
	signal.Notify(sig, os.Interrupt)
	    select {
	    case <-sig:
		    fmt.Println("\nExiting …")
	    }
}

func loadFromJson() []Task {
	return nil
}

func uiServer(port int) {
	return
}
