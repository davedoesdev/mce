import fs from 'fs';
const { readFile, open } = fs.promises;

class Char extends String {}
class Symbol extends String {}

class Pair {
    constructor(car, cdr) {
        this.car = car;
        this.cdr = cdr;
    }
}

function cons(car, cdr) {
    return new Pair(car, cdr);
}

function list_to_vector(l) {
    let v = [];
    while (l !== null) {
        v.push(l.car);
        l = l.cdr;
    }
    return v;
}

function cmap(f, l, tab, set_entry) {
    const ref = tab.get(l);
    if (ref !== undefined) {
        return ref;
    }
    const entry = set_entry(tab, l, cons(null, null));
    entry.car = f(l.car);
    entry.cdr = f(l.cdr);
    return entry;
}

function vector_cmap(f, vec, tab, set_entry) {
    const ref = tab.get(vec);
    if (ref !== undefined) {
        return ref;
    }
    const entry = set_entry(tab, vec, []);
    for (let el of vec) {
        entry.push(f(el));
    }
    return entry;
}

function is_yield_defn(args) {
    return args &&
           (args.car instanceof Symbol) &&
           (args.car.toString() === 'MCE-YIELD-DEFINITION');
}

function memoize_lambda(proc, defn) {
    return args => {
        if (is_yield_defn(args)) {
            if (typeof defn === 'function') {
                return defn();
            }
            return defn;
        }
        return proc(args);
    };
}

const forms = []

function define_form(f) {
    const len = forms.length;
    forms.push(f);
    return len;
}

const send = cons;

const symbol_lookup = define_form(
    (self, i) => (k, env) => send(k, ctenv_lookup(i, env)));

const send_value = define_form(
    (self, exp) => (k, env) => send(k, exp));

const runtime_lookup = define_form(
    (self, name) => (k, env) => send(k, lookup(name, env));
}

function make_form(defn) {
    const f2 = memoize_lambda(args => f(args), defn);
    const f = memoize_lambda(forms[defn.car](cons(f2, defn.cdr)), defn);
    return f;
}


function table_set(tab, v, entry) {
    tab.set(v, entry);
    return entry;
}

function is_unmemoized(v) {
    return (v.length === 2) &&
           (v[0] instanceof Symbol) &&
           (v[0].toString() === 'MCE-UNMEMOIZED');
}

function unmemoized_repexp(v) {
    return v[1];
}

function memoize_aux(exp, tab, fn) {
    if (exp instanceof Pair) {
        return cmap(fn, exp, tab, table_set);
    }
    if (Array.isArray(exp)) {
        if (is_unmemoized(exp)) {
            const ref = tab.get(exp)
            if (ref !== undefined) {
                return ref;
            }
            const repexp = unmemoized_repexp(exp);
            const entry = table_set(tab, exp,
                memoize_lambda(args => f(args), () => r));
            const r = fn(repexp);
            let f = args => {
                f = make_form(r);
                return f(args);
            };
            return entry;
        }
        return vector_cmap(fn, exp, tab, table_set);
    }
    return exp;
}

function memoize(exp) {
    const tab = new Map();
    const fn = x => memoize_aux(x, tab, fn);
    return fn(exp);
}

function make_serialized(n) {
    return ['MCE-SERIALIZED', n];
}

function is_serialized(v) {
    return (v.length === 2) &&
           (v[0] instanceof Symbol) &&
           (v[0].toString() === 'MCE-SERIALIZED');
}

function serialized_n(v) {
    return v[1];
}

function deserialize_aux(exp, tab, fn, set_entry) {
    if (exp instanceof Pair) {
        return cmap(fn, exp, tab, set_entry);
    }
    if (Array.isArray(exp)) {
        if (is_serialized(exp)) {
            return tab.get(serialized_n(exp));
        }
        return vector_cmap(fn, exp, tab, set_entry);
    }
    return exp;
}

function deserialize(exp) {
    let counter = 0;
    const tab = new Map();
    const set_entry = (tab, v, entry) => {
        return table_set(tab, counter++, entry);
    };
    const fn = x => deserialize_aux(x, tab, fn, set_entry);
    return fn(exp);
}

const null_code    = "a";
const boolean_code = "b";
const number_code  = "c";
const char_code    = "d";
const string_code  = "e";
const symbol_code  = "f";
const pair_code    = "g";
const vector_code  = "h";

function unpickle_aux(exp) {
    switch (exp[0]) {
    case boolean_code:
        return exp[1] === "t";
    case number_code:
        return exp[1];
    case char_code:
        return new Char(exp[1]);
    case string_code:
        return exp[1];
    case symbol_code:
        return new Symbol(exp[1]);
    case pair_code:
        return cons(unpickle_aux(exp[1]), unpickle_aux(exp[2]));
    case vector_code:
        return list_to_vector(unpickle_aux(exp[1]));
    default:
        return null;
    }
}

function unpickle(s) {
    return unpickle_aux(JSON.parse(s));
}

function mce_restore(s) {
    return memoize(deserialize(unpickle(s)));
}

function result(exp) {
    return ['MCE-RESULT', exp];
}

function is_result(exp) {
    return Array.isArray(exp) &&
           (exp.length === 2) &&
           (exp[0] instanceof Symbol) &&
           (exp[0].toString() === 'MCE-RESULT');
}

function result_val(exp) {
    return v[1];
}

function step(state) {
    return state.car(cons(state.cdr, null));
}

function run(state) {
    while (!is_result(state)) {
        state = step(state);
    }

    return result_val(state);
}

(async function () {
    const stdin = await open('/dev/stdin');
    const s = JSON.parse(await stdin.readFile());
    const r = mce_restore(s);
    if (typeof r === 'function') {
        r(cons(nil, nil));
    } else {
        run(r);
    }
})();
