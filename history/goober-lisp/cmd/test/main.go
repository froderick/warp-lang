package main

import "fmt"

type T struct {
}

func main() {

	var container interface{} = T{}
	switch typed := container.(type) {
	case T:
		fmt.Printf("is a T: %v\n", typed)
	default:
		fmt.Printf("defaulted: %v\n", typed)
	}

	fmt.Printf("later: %v\n", typed)
}
