package goober

import "io/ioutil"
import "fmt"
import "os"

// universal constructs and initialization

type World struct {
	defaultNs Ns
}

var world *World

func init() {
	world = &World{defaultNs: NewNs("user")}

	path := os.Getenv("LISP_PATH")

	data, err := ioutil.ReadFile(path + "/core.el")
	if err != nil {
		panic(fmt.Sprintf("error reading file: core.el", err))
	}

	for _, val := range Read(string(data)) {
		Eval(&world.defaultNs, val)
	}
}

func DefaultNs() *Ns {
	return &world.defaultNs
}
