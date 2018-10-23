package goober

import "math"
import "fmt"
import "strings"

type builtin_f func([]Value) Value

type builtin struct {
	name string
	f    builtin_f
}

func (v builtin) truthy() bool {
	return true
}

func (v builtin) prn() string {
	return v.name
}

func (v builtin) String() string {
	return v.prn()
}

func (f builtin) Name() string {
	return f.name
}

func (f builtin) IsMacro() bool {
	return false
}

func (f builtin) Invoke(context *context, args []Value) Value {
	return f.f(evalAll(context, args))
}

func makeBuiltin(name string, f func([]Value) Value) IFn {
	return builtin{name: name, f: builtin_f(f)}
}

// TODO: would be nice to avoid this duplication
var builtinMap = map[string]IFn{
	"list":     makeBuiltin("list", builtin_list),
	"first":    makeBuiltin("first", builtin_first),
	"last":     makeBuiltin("last", builtin_last),
	"rest":     makeBuiltin("rest", builtin_rest),
	"nth":      makeBuiltin("nth", builtin_nth),
	"cons":     makeBuiltin("cons", builtin_cons),
	"+":        makeBuiltin("+", builtin_plus),
	"-":        makeBuiltin("-", builtin_minus),
	"=":        makeBuiltin("=", builtin_eq),
	">":        makeBuiltin(">", builtin_gt),
	">=":       makeBuiltin(">=", builtin_gteq),
	"<":        makeBuiltin("<", builtin_lt),
	"<=":       makeBuiltin("<=", builtin_lteq),
	"hash-map": makeBuiltin("hash-map", builtin_hashmap),
	"get":      makeBuiltin("get", builtin_get),
	"put":      makeBuiltin("put", builtin_put),
	"seq":      makeBuiltin("seq", builtin_seq),
	"println":  makeBuiltin("println", builtin_println),
	"count":    makeBuiltin("count", builtin_count),
	"str":      makeBuiltin("str", builtin_str),
}

func builtin_list(vals []Value) Value {
	return Sexpr(vals)
}

func builtin_first(vals []Value) Value {

	if len(vals) != 1 {
		panic(fmt.Sprintf("first takes only 1 parameter: %v", vals))
	}

	seq := seq(vals[0])
	if len(seq) == 0 {
		return Nil{}
	} else {
		return seq[0]
	}
}

func builtin_last(vals []Value) Value {

	if len(vals) != 1 {
		panic(fmt.Sprintf("first takes only 1 parameter: %v", vals))
	}

	seq := seq(vals[0])
	if len(seq) == 0 {
		return Nil{}
	} else {
		return seq[len(seq)-1]
	}
}

func builtin_rest(vals []Value) Value {

	if len(vals) != 1 {
		panic(fmt.Sprintf("rest takes only 1 parameter: %v", vals))
	}

	list := requireSexpr(vals[0], "rest takes a list")

	if len(list) == 0 {
		return Nil{}
	} else {
		return Sexpr(list[1:]) // this will probably bite me in the ass
	}
}

func builtin_nth(vals []Value) Value {

	if len(vals) != 2 {
		panic(fmt.Sprintf("nth takes only 2 parameters: %v", vals))
	}

	list := requireSexpr(vals[0], "nth takes a list")
	n := requireInt(vals[1], "nth takes an int")

	return list[n]
}

func builtin_cons(vals []Value) Value {

	if len(vals) != 2 {
		panic(fmt.Sprintf("cons takes only 2 parameters: %v", vals))
	}

	x := vals[0]

	var list Sexpr
	switch y := vals[1].(type) {
	case Sexpr:
		list = y
	case Nil:
		list = Sexpr([]Value{})
	default:
		panic(fmt.Sprintf("second argument must be a list or nil: %v", y))
	}

	newList := make([]Value, 0, len(list)+1)
	newList = append(newList, x)
	newList = append(newList, list...)

	return Sexpr(newList)
}

func builtin_count(vals []Value) Value {

	if len(vals) != 1 {
		panic(fmt.Sprintf("count takes only 1 parameters: %v", vals))
	}

	x := vals[0]

	switch x := x.(type) {
	case Sexpr:
		return Int(len(x))
	case HashMap:
		return Int(len(x))
	default:
		panic(fmt.Sprintf("count requires a collection: %v", vals))
	}
}

func builtin_println(vals []Value) Value {
	newList := make([]string, 0, len(vals))
	for _, v := range vals {
		newList = append(newList, v.prn())
	}
	fmt.Println(strings.Join(newList, " "))
	return Nil{}
}

type HashMap map[Value]Value

func (v HashMap) truthy() bool {
	return len(v) > 0
}

func (v HashMap) prn() string {

	var items string

	if len(v) > 0 {
		kvs := make([]string, 0, len(v)+2)
		for k, val := range v {
			kvs = append(kvs, k.prn(), val.prn())
		}
		items = " " + strings.Join(kvs, " ")
	}

	return "(hash-map" + items + ")"
}

func (v HashMap) String() string {
	return v.prn()
}

func builtin_hashmap(vals []Value) Value {

	if math.Mod(float64(len(vals)), 2) != 0 {
		panic(fmt.Sprintf("hash-map's arguments must be an even number of values: %v", vals))
	}

	kvs := map[Value]Value{}
	for i := 0; i < len(vals); i += 2 {
		k := vals[i]
		val := vals[i+1]
		kvs[k] = val
	}

	return HashMap(kvs)
}

func builtin_get(vals []Value) Value {

	if len(vals) != 2 {
		panic(fmt.Sprintf("get takes 2 parameters: %v", vals))
	}

	m := requireHashMap(vals[0], "first argument must be a map")

	return m[vals[1]]
}

func builtin_put(vals []Value) Value {

	if len(vals) != 3 {
		panic(fmt.Sprintf("get takes 3 parameters: %v", vals))
	}

	m := requireHashMap(vals[0], "first argument must be a map")

	copy := make(map[Value]Value, len(m))
	for k, v := range m {
		copy[k] = v
	}
	copy[vals[1]] = vals[2]

	return HashMap(copy)
}

func seq(val Value) Sexpr {
	switch val := val.(type) {
	case HashMap:
		seq := make([]Value, 0, len(val)*2)
		for k, v := range val {
			seq = append(seq, Sexpr([]Value{k, v}))
		}
		return Sexpr(seq)
	case Sexpr:
		return val
	default:
		panic(fmt.Sprintf("not seq-able: %v", val))
	}
}

func builtin_seq(vals []Value) Value {

	if len(vals) != 1 {
		panic(fmt.Sprintf("seq takes 1 parameter: %v", vals))
	}

	return seq(vals[0])
}

func builtin_plus(vals []Value) Value {
	var base int = 0
	for i := range vals {
		val := requireInt(vals[i], "arguments to '+' must be numbers")
		base = base + int(val)
	}
	return Int(base)
}

func builtin_minus(vals []Value) Value {

	if len(vals) == 0 {
		panic(fmt.Sprintf("- takes 1 parameter: %v", vals))
	}

	base := requireInt(vals[0], "arguments to '-' must be numbers")
	for _, i := range vals[1:] {
		val := requireInt(i, "arguments to '-' must be numbers")
		base = base - val
	}
	return Int(base)
}

func builtin_eq(vals []Value) Value {

	if len(vals) < 1 {
		panic(fmt.Sprintf("= takes at least 1 parameter: %v", vals))
	}

	base := int(requireInt(vals[0], "arguments to '=' must be numbers"))
	for _, i := range vals[1:] {
		val := int(requireInt(i, "arguments to '=' must be numbers"))
		if val != base {
			return Boolean(false)
		}
	}
	return Boolean(true)
}

func builtin_lt(vals []Value) Value {

	if len(vals) < 1 {
		panic(fmt.Sprintf("< takes at least 1 parameter: %v", vals))
	}

	base := int(requireInt(vals[0], "arguments to '<' must be numbers"))
	for _, i := range vals[1:] {
		val := int(requireInt(i, "arguments to '<' must be numbers"))
		if base >= val {
			return Boolean(false)
		}
		base = val
	}
	return Boolean(true)
}

func builtin_lteq(vals []Value) Value {

	if len(vals) < 1 {
		panic(fmt.Sprintf("<= takes at least 1 parameter: %v", vals))
	}

	base := int(requireInt(vals[0], "arguments to '<=' must be numbers"))
	for _, i := range vals[1:] {
		val := int(requireInt(i, "arguments to '<=' must be numbers"))
		if base > val {
			return Boolean(false)
		}
		base = val
	}
	return Boolean(true)
}

func builtin_gt(vals []Value) Value {

	if len(vals) < 1 {
		panic(fmt.Sprintf("> takes at least 1 parameter: %v", vals))
	}

	base := int(requireInt(vals[0], "arguments to '>' must be numbers"))
	for _, i := range vals[1:] {
		val := int(requireInt(i, "arguments to '>' must be numbers"))
		if base <= val {
			return Boolean(false)
		}
	}
	return Boolean(true)
}

func builtin_gteq(vals []Value) Value {

	if len(vals) < 1 {
		panic(fmt.Sprintf(">= takes at least 1 parameter: %v", vals))
	}

	base := int(requireInt(vals[0], "arguments to '>=' must be numbers"))
	for _, i := range vals[1:] {
		val := int(requireInt(i, "arguments to '>=' must be numbers"))
		if base < val {
			return Boolean(false)
		}
	}
	return Boolean(true)
}

func builtin_str(vals []Value) Value {

	strs := make([]string, 0)
	for _, i := range vals {
		strs = append(strs, i.prn())
	}
	return Str(strings.Join(strs, ""))
}
