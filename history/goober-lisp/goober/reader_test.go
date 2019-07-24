package goober

import "testing"
import "fmt"
import "reflect"

func assertEqual(t *testing.T, a interface{}, b interface{}) {
	if !reflect.DeepEqual(a, b) {
		t.Log(fmt.Sprintf("%v and %v are not equal", a, b))
		t.Fail()
	}
}

func sexpr(v ...Value) Sexpr {
	return Sexpr(v)
}

func sym(s string) Symbol {
	return Symbol(s)
}

func readOne(s string) Value {
	return Read(s)[0]
}

func TestReadBoolean(t *testing.T) {
	assertEqual(t, readOne("true"), Boolean(true))
	assertEqual(t, readOne("false"), Boolean(false))
}

func TestReadSymbol(t *testing.T) {
	assertEqual(t, readOne("+"), readOne("+"))
}

func TestReadInt(t *testing.T) {
	assertEqual(t, readOne("100"), Int(100))
}

func TestReadStr(t *testing.T) {
	assertEqual(t, readOne("\"A\""), Str("A"))
}

func TestReadSexpr(t *testing.T) {
	assertEqual(t, readOne("(+ 1 2 3)"), sexpr(sym("+"), Int(1), Int(2), Int(3))) // basic
	assertEqual(t, readOne("(x (y))"), sexpr(sym("x"), sexpr(sym("y"))))          // nesting
}

func TestReadQuote(t *testing.T) {
	assertEqual(t, readOne("'(foo)"), sexpr(sym("quote"), sexpr(sym("foo"))))
	assertEqual(t, readOne("'f"), sexpr(sym("quote"), (sym("f"))))
}

func doPop(ts TokenStream) string {
	x, _ := ts.Pop()
	return x
}

func TestTokenStream(t *testing.T) {
	ts := NewTokenStream("a", "b", "c")
	assertEqual(t, []string{doPop(ts), doPop(ts), doPop(ts)}, []string{"a", "b", "c"})
}
