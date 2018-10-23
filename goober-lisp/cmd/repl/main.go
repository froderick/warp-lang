package main

import "fmt"
import "os"
import "bufio"
import "strings"
import "goober-lisp/goober"
import "runtime/debug"
import "io/ioutil"

func isEmpty(s string) bool {
	t := strings.TrimSpace(s)
	return len(t) == 0
}

func handle(ns *goober.Ns, input string) {

	// not supposed to panic across packages, but too bad
	defer func() {
		if e := recover(); e != nil {
			fmt.Printf("%s: %s", e, debug.Stack())
		}
	}()

	if !isEmpty(input) {
		for _, val := range goober.Read(input) {
			fmt.Printf("%v\n", goober.Eval(ns, val))
		}
	}
}

func main() {
	ns := goober.DefaultNs()

	stat, _ := os.Stdin.Stat()

	if len(os.Args) > 1 { // read from file

		data, err := ioutil.ReadFile(os.Args[1])
		if err != nil {
			panic(fmt.Sprintf("error reading input", err))
		}

		lines := strings.Split(string(data), "\n")
		filtered := make([]string, 0)
		for _, l := range lines {
			if !strings.HasPrefix(l, "#!") {
				filtered = append(filtered, l)
			}
		}
		done := strings.Join(filtered, "\n")

		for _, val := range goober.Read(done) {
			goober.Eval(ns, val)
		}

	} else if (stat.Mode() & os.ModeCharDevice) == 0 { // handle piped lisp script

		reader := bufio.NewReader(os.Stdin)
		data, err := ioutil.ReadAll(reader)
		if err != nil {
			panic(fmt.Sprintf("error reading input", err))
		}

		for _, val := range goober.Read(string(data)) {
			goober.Eval(ns, val)
		}
	} else { // fall back to repl
		for {
			reader := bufio.NewReader(os.Stdin)
			fmt.Print(ns.Name + "> ")

			input, err := reader.ReadString('\n')
			if err != nil {
				break
			}

			handle(ns, input)
		}
	}
}
