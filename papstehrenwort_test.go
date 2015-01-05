package main

import (
	"github.com/openlab-aux/papstehrenwort/server"
	"reflect"
	"testing"
)

func TestApplyUserInput(t *testing.T) {
	task := &server.Task{
		Users: make([]server.User, 1),
	}
	user := server.User{}
	tasks := server.TaskList{task}
	inpc := make(chan server.UserInput)
	go applyUserInput(&tasks, inpc)
	inpc <- server.UserInput{User: user, Tasks: map[*server.Task]bool{task: true}}
	tmp := tasks[0].Users
	if len(tmp) < 1 || !reflect.DeepEqual(tmp[0], user) {
		t.Error("User not in tasklist")
	}
}

func TestApplyUserInputNoNewUser(t *testing.T) {
	task := new(server.Task)
	user := server.User{}
	tasks := server.TaskList{task}
	inpc := make(chan server.UserInput)
	go applyUserInput(&tasks, inpc)
	inpc <- server.UserInput{User: user, Tasks: map[*server.Task]bool{task: false}}
	tmp := tasks[0].Users
	if len(tmp) > 0 {
		t.Error("User should not be in tasklist")
	}
}
