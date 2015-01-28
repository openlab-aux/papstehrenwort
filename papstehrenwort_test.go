package main

import (
	"github.com/openlab-aux/papstehrenwort/server"
	"testing"
)

const (
	address = "test@suplto.de"
)

func TestApplyUserInput(t *testing.T) {
	user := server.User{Address: address}
	task := &server.Task{Users: make(map[string]server.User)}

	task.Users[address] = user
	tasks := []*server.Task{task}
	inpc := make(chan server.UserInput)

	go applyUserInput(tasks, inpc)
	inpc <- server.UserInput{User: user, Tasks: map[*server.Task]bool{task: true}}
	users := tasks[0].Users

	if len(users) < 1 || !(users[address] == user) {
		t.Errorf("User %s not in tasklist", user)
	}
}

func TestApplyUserInputNoNewUser(t *testing.T) {
	user := server.User{Address: address}
	task := &server.Task{Users: make(map[string]server.User)}
	tasks := []*server.Task{task}
	inpc := make(chan server.UserInput)

	go applyUserInput(tasks, inpc)

	inpc <- server.UserInput{User: user, Tasks: map[*server.Task]bool{task: false}}
	tmp := tasks[0].Users
	if len(tmp) > 0 {
		t.Error("User should not be in tasklist")
	}
}
