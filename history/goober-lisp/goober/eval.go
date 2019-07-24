package goober

import "math"
import "fmt"
import "strings"

// incorporate functions as value types

type fn struct {
	name    string
	args    argsInfo
	exprs   []Value
	context context
	isMacro bool
}

type recur []Value

func (v fn) truthy() bool {
	return true
}

func (v fn) prn() string {

	args := make([]string, 0, len(v.args.declared)+2)
	for _, arg := range v.args.declared {
		args = append(args, arg.prn())
	}
	if v.args.useRest {
		args = append(args, "&", string(v.args.rest))
	}

	exprs := make([]string, 0, len(v.exprs))
	for _, expr := range v.exprs {
		exprs = append(exprs, expr.prn())
	}

	return "(fn (" + strings.Join(args, " ") + ") " + strings.Join(exprs, " ") + ")"
}

func (v fn) String() string {
	return v.prn()
}

func (v recur) truthy() bool {
	return true
}

func (v recur) prn() string {
	return fmt.Sprintf("#recur[%v]", v)
}

// data structures to support vars and bindings

type Ns struct {
	Name string
	vars map[string]Value
}

func NewNs(name string) Ns {
	return Ns{Name: "user", vars: map[string]Value{}}
}

func (ns *Ns) def(name string, value Value) {
	ns.vars[name] = value
}

func (ns *Ns) undef(name string) {
	delete(ns.vars, name)
}

type binding struct {
	name  string
	value Value
}

type context struct {
	ns       *Ns
	bindings []binding
}

func (c *context) push(name Symbol, value Value) {
	b := binding{name: string(name), value: value}
	c.bindings = append(c.bindings, b)
}

func (c *context) pop() binding {

	if len(c.bindings) == 0 {
		panic("no bindings remain to be popped")
	}

	popped := c.bindings[len(c.bindings)-1]
	c.bindings = c.bindings[:len(c.bindings)-1]

	return popped
}

func (c context) get(name Symbol) Value {

	s := string(name)

	if len(c.bindings) > 0 {
		for i := len(c.bindings); i > 0; i-- {
			binding := c.bindings[i-1]
			if s == binding.name {
				return binding.value
			}
		}
	}

	if v, ok := c.ns.vars[s]; ok {
		return v
	}

	if b, ok := builtinMap[string(s)]; ok {
		return b.(Value)
	}

	panic("cannot find a binding or var with this symbol name: " + name)
}

// type casting utilities

func requireSymbol(v Value, msg string) Symbol {
	switch x := v.(type) {
	case Symbol:
		return x
	default:
		panic(fmt.Sprintf(msg+": %v", v))
	}
}

func requireInt(v Value, msg string) Int {
	switch x := v.(type) {
	case Int:
		return x
	default:
		panic(fmt.Sprintf(msg+": %v", v))
	}
}

func requireSexpr(v Value, msg string) Sexpr {
	switch x := v.(type) {
	case Sexpr:
		return x
	default:
		panic(fmt.Sprintf(msg+": %v", v))
	}
}

func requireKeyword(v Value, msg string) Keyword {
	switch x := v.(type) {
	case Keyword:
		return x
	default:
		panic(fmt.Sprintf(msg+": %v", v))
	}
}

func requireHashMap(v Value, msg string) HashMap {
	switch x := v.(type) {
	case HashMap:
		return x
	default:
		panic(fmt.Sprintf(msg+": %v", v))
	}
}

func requireFn(v Value, msg string) fn {
	switch x := v.(type) {
	case fn:
		return x
	default:
		panic(fmt.Sprintf(msg+": %v", v))
	}
}

// This function is special because it acts like a macro,
// it operates on the raw values haneded to it from the
// reader.
func special_def(context *context, vals []Value) Value {

	if len(vals) != 2 {
		panic(fmt.Sprintf("def takes only 2 parameters: %v", vals))
	}

	switch varname := vals[0].(type) {
	case Symbol:
		context.ns.def(string(varname), eval(context, vals[1]))
	default:
		panic(fmt.Sprintf("vars can only be named by symbols: %v", varname))
	}

	return Nil{}
}

func special_let(context *context, vals []Value) Value {

	if len(vals) < 1 {
		panic(fmt.Sprintf("let takes at least 1 parameter: %v", vals))
	}

	bindings := requireSexpr(vals[0], "vars can only be named by symbols")

	if math.Mod(float64(len(bindings)), 2) != 0 {
		panic(fmt.Sprintf("let's binding list must be an even number of values: %v", bindings))
	}

	pushes := 0

	// eval each binding, add it to the context
	for i := 0; i < len(bindings); i += 2 {

		sym := requireSymbol(bindings[i], "bindings can only be made for symbols")
		expr := bindings[i+1]
		val := eval(context, expr)

		context.push(sym, val)
		defer context.pop()

		pushes++
	}

	// eval the rest of the let arguments

	var result Value
	for i := 1; i < len(vals); i++ {
		expr := vals[i]
		result = eval(context, expr)
	}

	// return the result of the last statement in the let block

	return result
}

func special_if(context *context, vals []Value) Value {

	if len(vals) < 2 {
		panic(fmt.Sprintf("if takes at least 2 parameters: %v", vals))
	}

	if len(vals) > 3 {
		panic(fmt.Sprintf("if takes at most 3 parameters: %v", vals))
	}

	test := eval(context, vals[0])

	switch v := test.(type) {
	case Value:
		if v.truthy() {
			return eval(context, vals[1])
		} else {
			if len(vals) == 2 {
				return Nil{}
			} else {
				return eval(context, vals[2])
			}
		}
	default:
		panic(fmt.Sprintf("only value types can be tested for truthiness: %v", test))
	}
}

type argsInfo struct {
	declared []Symbol
	useRest  bool
	rest     Symbol
}

func getArgs(args Value) argsInfo {

	params := requireSexpr(args, "expected args in the form of a list")
	result := argsInfo{}

	result.declared = make([]Symbol, 0, len(params))
	for i := range params {
		name := requireSymbol(params[i], "arguments to functions must be symbols")

		if name == Symbol("&") {
			result.useRest = true
			continue
		}

		if !result.useRest {
			result.declared = append(result.declared, name)
		} else {
			result.rest = name
		}
	}

	return result
}

func special_fn(context *context, vals []Value) Value {

	if len(vals) < 2 {
		panic(fmt.Sprintf("fn takes at least 2 parameters: %v", vals))
	}

	// intentionally copying the context here, that becomes part of the fn
	return fn{args: getArgs(vals[0]), exprs: vals[1:], context: *context}
}

func special_defmacro(context *context, vals []Value) Value {

	if len(vals) < 2 {
		panic(fmt.Sprintf("defmacro takes 2 parameters: %v", vals))
	}

	// intentionally copying the context here, that becomes part of the fn
	f := fn{args: getArgs(vals[1]), exprs: vals[2:], context: *context, isMacro: true}

	switch varname := vals[0].(type) {
	case Symbol:
		context.ns.def(string(varname), f)
	default:
		panic(fmt.Sprintf("vars can only be named by symbols: %v", varname))
	}

	return Nil{}
}

func packageArgs(name string, fn *fn, supplied []Value) ([]Value, []Value) { // take list of args, handle var-args

	if len(supplied) < len(fn.args.declared) {
		if fn.args.useRest {
			panic(fmt.Sprintf("%v takes at least %v parameters: called %v with args %v", name, len(fn.args.declared), fn, supplied))
		} else {
			panic(fmt.Sprintf("%v takes %v parameters: called %v with args %v", name, len(fn.args.declared), fn, supplied))
		}
	}

	if len(supplied) > len(fn.args.declared) && !fn.args.useRest {
		panic(fmt.Sprintf("%v takes %v parameters: called %v with args %v", name, len(fn.args.declared), fn, supplied))
	}

	declared := supplied[0:len(fn.args.declared)]
	rest := supplied[len(fn.args.declared):]

	return declared, rest
}

func special_fn_call_inner(name string, fn *fn, context *context, vals []Value) Value {

	//fmt.Printf("calling %v with args %v\n", fn, Sexpr(vals))

	declared, rest := packageArgs(name, fn, vals)

	for i, bindingName := range fn.args.declared {
		bindingValue := declared[i]
		fn.context.push(bindingName, bindingValue)
		defer fn.context.pop()
	}

	if fn.args.useRest {
		bindingName := fn.args.rest

		var bindingValue Value
		if len(rest) == 0 {
			bindingValue = Nil{}
		} else {
			bindingValue = Sexpr(rest)
		}

		fn.context.push(bindingName, bindingValue)
		defer fn.context.pop()
	}

	var result Value
	for _, expr := range fn.exprs {
		result = eval(&fn.context, expr)
	}

	//fmt.Printf("DONE calling %v with args %v => %v\n", fn, Sexpr(vals), result)

	return result
}

func special_fn_call(name string, fn fn, context *context, vals []Value) Value {

	result := special_fn_call_inner(name, &fn, context, vals)
	for {
		switch r := result.(type) {
		case recur:
			result = special_fn_call_inner(name, &fn, context, r)
		default:
			return result
		}
	}

}

func special_keyword_call(context *context, k Keyword, args []Value) Value {

	if len(args) != 1 {
		panic(fmt.Sprintf("a keyword as a function takes only one argument: %v", args))
	}

	replacement := Sexpr([]Value{
		Symbol("get"),
		args[0],
		k,
	})

	return eval(context, replacement)
}

func special_do(context *context, vals []Value) Value {

	var result Value
	for _, expr := range vals {
		result = eval(context, expr)
	}

	return result
}

func special_quote(context *context, vals []Value) Value {
	if len(vals) != 1 {
		panic(fmt.Sprintf("quote takes only 1 parameter: %v", vals))
	}
	param := vals[0]
	return param
}

func special_recur(context *context, vals []Value) Value {
	return recur(evalAll(context, vals))
}

func evalRest(context *context, v Sexpr) []Value {
	rest := make([]Value, 0, len(v)-1)
	for _, item := range v[1:] {
		evaluated := eval(context, item)
		rest = append(rest, evaluated)
	}
	return rest
}

// TODO: macro syntax quotes

type IFn interface {
	Name() string
	IsMacro() bool
	Invoke(context *context, args []Value) Value
}

func evalAll(context *context, vals []Value) []Value {
	evaluated := make([]Value, 0, len(vals))
	for _, val := range vals {
		evaluated = append(evaluated, eval(context, val))
	}
	return evaluated
}

func (f fn) Name() string {
	return f.name
}

func (f fn) IsMacro() bool {
	return f.isMacro
}

func (f fn) Invoke(context *context, args []Value) Value {
	var name string
	if f.name == "" {
		name = "#<anonymous>"
	} else {
		name = f.name
	}

	if !f.isMacro {
		args = evalAll(context, args)
	}

	return special_fn_call(name, f, context, args)
}

func (f Keyword) Name() string {
	return ":" + string(f)
}

func (f Keyword) IsMacro() bool {
	return false
}

func (f Keyword) Invoke(context *context, args []Value) Value {
	return special_keyword_call(context, f, evalAll(context, args))
}

type special_f func(*context, []Value) Value

type special struct {
	name string
	f    special_f
}

func (f special) Name() string {
	return f.name
}

func (f special) IsMacro() bool {
	return false
}

func (f special) Invoke(context *context, args []Value) Value {
	return f.f(context, args)
}

func makeSpecial(name string, f func(*context, []Value) Value) IFn {
	return special{name: name, f: special_f(f)}
}

func getIFn(context *context, v Value) IFn {

	switch first := v.(type) {
	case IFn:
		return first
	case Symbol:

		switch first { // special functions
		case "def":
			return makeSpecial("def", special_def)
		case "defmacro":
			return makeSpecial("defmacro", special_defmacro)
		case "let":
			return makeSpecial("let", special_let)
		case "if":
			return makeSpecial("if", special_if)
		case "fn":
			return makeSpecial("fn", special_fn)
		case "quote":
			return makeSpecial("quote", special_quote)
		case "do":
			return makeSpecial("do", special_do)
		case "recur":
			return makeSpecial("recur", special_recur)
		}

		if builtin, ok := builtinMap[string(first)]; ok { // builtin functions
			return builtin
		}

		resolved := context.get(first) // IFn's bound to symbols, vars
		if f, ok := resolved.(IFn); ok {
			return f
		} else {
			panic(fmt.Sprintf("symbol '%v' is not bound to an IFn: %v", first, resolved))
		}
	default:
		panic(fmt.Sprintf("not a valid function: %", first))
	}
}

// TODO: implement debug message mode
// TODO: implement stack traces in reader and eval

// Evaluates a Value data structure as code.
func eval(context *context, v Value) Value {

	//fmt.Printf("eval() input: %v\n", v)
	//fmt.Printf("eval() bindings: %v\n", context.bindings)

	var result Value

	switch v := v.(type) {
	case Sexpr:

		if len(v) == 0 {
			return v
		}

		first := v[0]
		rest := v[1:]

		if sexpr, ok := first.(Sexpr); ok {
			resolved := make([]Value, 0)
			resolved = append(resolved, eval(context, sexpr))
			resolved = append(resolved, rest...)
			return eval(context, Sexpr(resolved))
		} else {
			f := getIFn(context, first)
			if f.IsMacro() {
				expanded := f.Invoke(context, rest)
				//fmt.Printf("expanded %v to %v\n", f, expanded)
				result = eval(context, expanded)
			} else {
				result = f.Invoke(context, rest)
			}
		}

	case Symbol:
		result = context.get(v)

	default:
		result = v
	}

	//fmt.Printf("eval() output: %v\n", result)

	return result
}

func Eval(ns *Ns, v Value) Value {

	context := context{
		ns:       ns,
		bindings: make([]binding, 0),
	}

	return eval(&context, v)
}
