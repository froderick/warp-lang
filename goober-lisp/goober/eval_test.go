package goober

import "testing"
import "fmt"
import "runtime/debug"
import "reflect"

// eval utilities

func test_eval_ns(ns *Ns, s string) Value {
	defer func() {
		if e := recover(); e != nil {
			fmt.Printf("%s: %s", e, debug.Stack())
		}
	}()
	sexpr := Read(s)[0]
	return Eval(ns, sexpr)
}

func test_eval(s string) Value {
	ns := DefaultNs()
	return test_eval_ns(ns, s)
}

// eval test data

type xform func(Value) Value

type testPair struct {
	input    string
	expected Value
	xform    xform
}

var tests = []testPair{

	// special functions (macros)

	{
		input: `(let (def-result (def x 100))
	              (list def-result x))`,
		expected: sexpr(Nil{}, Int(100)),
	},

	{input: "(let (a 1 b 2) a)", expected: Int(1)},

	{input: "(if nil 'y 'n)", expected: Symbol("n")},
	{input: "(if true 'y 'n)", expected: Symbol("y")},
	{input: "(if false 'y 'n)", expected: Symbol("n")},
	{input: "(if 'x 'y 'n)", expected: Symbol("y")},
	{input: "(if -1 'y 'n)", expected: Symbol("y")},
	{input: "(if 0 'y 'n)", expected: Symbol("n")},
	{input: "(if 1 'y 'n)", expected: Symbol("y")},
	{input: "(if \"\" 'y 'n)", expected: Symbol("n")},
	//{input: "(if \" \" 'y 'n)", expected: Symbol("y")}, // TODO: this will not work because our lexer sucks
	{input: "(if \"test\" 'y 'n)", expected: Symbol("y")},
	{input: "(if () 'y 'n)", expected: Symbol("y")},
	{input: "(if '(1) 'y 'n)", expected: Symbol("y")},

	{
		input: "(fn (a) (+ 1 2) (+ a 10))",
		expected: fn{
			args: argsInfo{
				declared: []Symbol{Symbol("a")},
			},
			exprs: sexpr(
				sexpr(Symbol("+"), Int(1), Int(2)),
				sexpr(Symbol("+"), Symbol("a"), Int(10)),
			),
		},
		xform: func(v Value) Value { // empty out the context so the data structures match
			fn := v.(fn)
			fn.context = context{}
			return fn
		},
	},
	{input: "((fn (a) (+ 1 2) (+ a 10)) 5)", expected: Int(15)},

	{input: "'y", expected: Symbol("y")},
	{input: "'(1 2 3)", expected: sexpr(Int(1), Int(2), Int(3))},
	{input: "(quote (1 2 3))", expected: sexpr(Int(1), Int(2), Int(3))},
	{input: "(not 'x)", expected: Boolean(false)},

	{input: "(do (+ 1 2 3) 5)", expected: Int(5)},

	// builtin functions (not macros)

	{input: "(list 1 2 3)", expected: sexpr(Int(1), Int(2), Int(3))},
	{input: "(first '(1 2 3))", expected: Int(1)},
	{input: "(rest '(1 2 3))", expected: sexpr(Int(2), Int(3))},
	{input: "(cons 100 '())", expected: sexpr(Int(100))},
	{input: "(+ 1 2 3)", expected: Int(6)},

	// keywords as basic functions

	{input: "(:a (hash-map :a :B))", expected: Keyword("B")},

	// keywords as higher-order functions
	{
		input:    "(map :a (list (hash-map :a \"ONE\") (hash-map :a \"TWO\")))",
		expected: sexpr(Str("ONE"), Str("TWO")),
	},
}

func TestEval(t *testing.T) {
	for _, pair := range tests {
		v := test_eval(pair.input)

		if pair.xform != nil {
			v = pair.xform(v)
		}

		if !reflect.DeepEqual(pair.expected, v) {
			t.Error(
				"For", pair.input,
				"expected", fmt.Sprintf("%v", pair.expected),
				"got", fmt.Sprintf("%v", v),
			)
		}
	}
}
