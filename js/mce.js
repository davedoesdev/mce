import fs from 'fs';
import { EOL } from 'os';
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

function list_ref(l, i) {
    while (i > 0) {
        l = l.cdr;
        --i;
    }
    return l.car;
}

function list_rest(l, i) {
    while (i > 0) {
        l = l.cdr;
        --i;
    }
    return l.cdr;
}

function list_to_vector(l) {
    let v = [];
    while (l !== null) {
        v.push(l.car);
        l = l.cdr;
    }
    return v;
}

function vector_to_list(v) {
    let l = null;
    for (let i = v.length - 1; i >= 0; --i) {
        l = cons(v[i], l);
    }
    return l;
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

function get_procedure_defn(proc) {
    return proc(cons(new Symbol('MCE-YIELD-DEFINITION'), null));
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

function ctenv_lookup(i, env) {
    const r = list_ref(env, i.car).cdr[i.cdr];
    return r === undefined ? null : r;
}

function ctenv_setvar(name, i, val, env) {
    const first = i.car;
    let second = i.cdr;
    const bindings = list_ref(env, first);
    const v = bindings.cdr;

    const len = v.length;
    if (second >= len) {
        v.length = second + 1;
        v.fill(null, len);
    }
    v[second] = val;

    if (!bindings.car) {
        bindings.car = cons(null, null);
    }

    let p = bindings.car;
    while (second > 0) {
        if (!p.cdr) {
            p.cdr = cons(null, null);
        }
        p = p.cdr;
        --second;
    }
    p.car = name;

    return val;
}

class SymbolNotFoundError extends Error {
    constructor(proc, sym) {
        super(`${proc}: symbol not found: ${sym}`);
    }
}

class UnknownDisplayExpressionError extends Error {
    constructor(exp) {
        super(`unknown display expression: ${exp}`);
    }
}

class UnknownPickleExpressionError extends Error {
    constructor(exp) {
        super(`unknown pickle expression: ${exp}`);
    }
}

class UnexpectedType extends Error {
    constructor(type, exp) {
        super(`not a ${type}: ${exp}`);
    }
}

function xdisplay(exp, out, is_write) {
    if (exp === null) {
        out.write('()');
    } else if (typeof exp === 'boolean') {
        out.write(exp ? '#t' : '#f');
    } else if (typeof exp === 'number') {
        out.write(exp.toString());
    } else if (exp instanceof Char) {
        if (is_write) {
            out.write('#\\x');
            out.write(exp.toString().charCodeAt(0).toString(16));
        } else {
            out.write(exp.toString());
        }
    } else if (typeof exp === 'string') {
        if (is_write) {
            out.write(JSON.stringify(exp));
        } else {
            out.write(exp);
        }
    } else if (exp instanceof Symbol) {
        out.write(exp.toString());
    } else if (exp instanceof Pair) {
        let first = true;
        out.write('(');
        while (exp instanceof Pair) {
            if (!first) {
                out.write(' ');
            }
            first = false;
            xdisplay(exp.car, out, is_write);
            exp = exp.cdr;
        }
        if (exp !== null) {
            out.write(' . ');
            xdisplay(exp, out, is_write);
        }
        out.write(')');
    } else if (Array.isArray(exp)) {
        let first = true;
        out.write('#(');
        for (let v of exp) {
            if (!first) {
                out.write(' ');
            }
            first = false;
            xdisplay(v, out, is_write);
        }
        out.write(')');
    } else if (typeof exp === 'function') {
        out.write('#<procedure>');
    } else {
        throw new UnknownDisplayExpressionError(exp);
    }

    return exp;
}

function newline(out) {
    out.write(EOL);
    return null;
}

function xprint(args, out) {
    let r = null;
    for (let arg of args) {
        r = arg;
        xdisplay(arg, out, false);
    }
    newline(out);
    return r;
}

function print(...args) {
    return xprint(args, process.stdout);
}

function eprint(...args) {
    return xprint(args, process.stderr);
}

function xwrite(args, out) {
    let r = null;
    for (let arg of args) {
        r = arg;
        xdisplay(arg, out, true);
    }
    return r;
}

function write(...args) {
    return xwrite(args, process.stdout);
}

function ewrite(...args) {
    return xwrite(args, process.stderr);
}

function plus(...args) {
    let r = 0;
    for (let arg of args) {
        r += arg;
    }
    return r;
}

function multiply(...args) {
    let r = 1;
    for (let arg of args) {
        r *= arg;
    }
    return r;
}

function less_than(x, y) {
    return x < y;
}

function greater_than(x, y) {
    return x > y;
}

function is_null(exp) {
    return exp === null;
}

function is_string(exp) {
    return typeof exp === 'string';
}

function is_pair(exp) {
    return exp instanceof Pair;
}

function is_vector(exp) {
    return Array.isArray(exp);
}

function is_procedure(exp) {
    return typeof exp === 'function';
}

function vector_length(v) {
    return v.length;
}

function vector_ref(v, i) {
    return v[i];
}

function car(p) {
    return p.car;
}

function cdr(p) {
    return p.cdr;
}

function is_eq(x, y) {
    if (((x instanceof Char) && (y instanceof Char)) ||
        ((x instanceof Symbol) && (y instanceof Symbol))) {
        return x.toString() === y.toString();
    }

    return x === y;
}

function is_number_equal(x, y) {
    if (typeof x !== 'number') {
        throw new UnexpectedType('number', x);
    }

    if (typeof y !== 'number') {
        throw new UnexpectedType('number', y);
    }

    return x === y;
}

function is_string_equal(x, y) {
    if (typeof x !== 'string') {
        throw new UnexpectedType('string', x);
    }

    if (typeof y !== 'string') {
        throw new UnexpectedType('string', y);
    }

    return x === y;
}

function set_car(p, exp) {
    p.car = exp;
    return p;
}

function set_cdr(p, exp) {
    p.cdr = exp;
    return p;
}

function getpid() {
    return process.pid;
}

const global_table = new Map([
    ['result', result],
    ['+', plus],
    ['*', multiply],
    ['<', less_than],
    ['>', greater_than],
    ['print', print],
    ['eprint', eprint],
    ['write', write],
    ['ewrite', ewrite],
    ['null?', is_null],
    ['string?', is_string],
    ['pair?', is_pair],
    ['vector?', is_vector],
    ['procedure?', is_procedure],
    ['vector-length', vector_length],
    ['vector-ref', vector_ref],
    ['string=?', is_string_equal],
    ['car', car],
    ['cdr', cdr],
    ['set-car!', set_car],
    ['set-cdr!', set_cdr],
    ['cons', cons],
    ['eq?', is_eq],
    ['=', is_number_equal],
    ['unmemoize', unmemoize],
    ['serialize', serialize],
    ['apply', applyx],
    ['save', mce_save],
    ['restore', mce_restore],
    ['transfer', transfer],
    ['getpid', getpid]
]);

const kenvfn_set = new Set([
    applyx,
    transfer
]);

function find_global(sym) {
    const s = sym.toString();
    const r = global_table.get(s);
    if (r === undefined) {
        throw new SymbolNotFoundError('find_global', sym);
    }
    return r;
}

function make_step_contn(k, args) {
    return cons(new Symbol('MCE-STEP-CONTN'), cons(k, args));
}

function is_step_contn(args) {
    return args &&
           (args.car instanceof Symbol) &&
           (args.car.toString() === 'MCE-STEP-CONTN');
}

function step_contn_k(args) {
    return list_ref(args, 1);
}

function step_contn_args(args) {
    return list_rest(args, 1);
}

function make_global_env() {
    const values = [];
    const bindings = cons(null, values);
    return cons(bindings, null);
}

function make_env_args(env, args) {
    return cons(new Symbol('MCE-ENV-ARGS'), cons(env, args));
}

function is_env_args(args) {
    return args &&
           (args.car instanceof Symbol) &&
           (args.car.toString() === 'MCE-ENV-ARGS');
}

function env_args_env(args) {
    return is_env_args(args) ? list_ref(args, 1) : make_global_env();
}

function env_args_args(args) {
    return is_env_args(args) ? list_rest(args, 1) : args;
}

function is_transfer(args) {
    return args &&
           (args.car instanceof Symbol) &&
           (args.car.toString() === 'MCE-TRANSFER');
}

function transfer_args(args) {
    return list_rest(args, 0);
}

function transfer(k, env, fn, ...args) {
    return fn(cons(new Symbol('MCE-TRANSFER'), vector_to_list(args)));
}

function globalize(x, args, cf) {
    if (typeof x !== 'function') {
        return x;
    }

    const defn = cons(constructed_function, cons(args, cons(cf, null)));
    const f2 = memoize_lambda(args => f(args), defn);
    const f = memoize_lambda(wrap_global_lambda(x, f2), defn);
    return f;
}

function call_global(f, args) {
    return f(...list_to_vector(args));
}

function handle_global_lambda(args, fn, cf) {
    if (is_transfer(args)) {
        return fn(args);
    }

    if (is_step_contn(args)) {
        const sck = step_contn_k(args);
        const sca = step_contn_args(args);
        const eaa = env_args_args(sca);
        return send(sck, globalize(call_global(fn, eaa), eaa, cf));
    }

    const eaa = env_args_args(args);
    return globalize(call_global(fn, eaa), eaa, cf);
}

function handle_global_lambda_kenv(args, fn) {
    if (is_step_contn(args)) {
        const sca = step_contn_args(args);
        return call_global(fn, cons(step_contn_k(args),
                                    cons(env_args_env(sca),
                                         env_args_args(sca))));
    }

    return run(call_global(fn, cons(lookup_global(new Symbol('result')),
                                    cons(env_args_env(args),
                                         env_args_args(args)))));
}

function wrap_global_lambda(fn, cf) {
    if (kenvfn_set.has(fn)) {
        return args => handle_global_lambda_kenv(args, fn);
    }
    return args => handle_global_lambda(args, fn, cf);
}

function lookup_global(sym) {
    const r = find_global(sym);
    const defn = cons(global_lambda, cons(sym, null));
    const f2 = memoize_lambda(args => f(args), defn);
    const f = memoize_lambda(wrap_global_lambda(r, f2), defn);
    return f;
}

function extend_env(env, syms, values) {
    return cons(cons(syms, list_to_vector(values)), env);
}

function improper_extend_env(env, syms, values) {
    const s = [];
    const v = [];

    while (syms) {
        if (syms instanceof Symbol) {
            s.push(syms);
            v.push(values);
            break;
        }
        if (!values) {
            break;
        }

        s.push(syms.car);
        v.push(values.car);

        syms = syms.cdr;
        values = values.cdr;
    }

    let sl = null;
    for (let sym of s) {
        sl = cons(sym, sl);
    }

    return cons(cons(sl, v), env);
}

function handle_lambda(args, params, fn, env, extend_env) {
    if (is_step_contn(args)) {
        const sca = step_contn_args(args);
        return fn(cons(step_contn_k(args),
                       cons(extend_env(env, params, env_args_args(sca)),
                            null)));
    }

    return run(fn(cons(lookup_global(new Symbol('result')),
                       cons(extend_env(env, params, env_args_args(args)),
                            null))));
}

function handle_contn_lambda(args, k) {
    if (is_transfer(args)) {
        return k(env_args_args(transfer_args(args)));
    }

    if (is_step_contn(args)) {
        return k(env_args_args(step_contn_args(args)));
    }

    return run(k(env_args_args(args)));
}

function applyx(k, env, fn, args) {
    return fn(make_step_contn(k, make_env_args(env, args)));
}

const forms = []

function define_form(f) {
    const len = forms.length;
    forms.push(f);
    return len;
}

const send = cons;

const symbol_lookup = define_form((self, i) =>
    args => {
        const [k, env] = list_to_vector(args);
        return send(k, ctenv_lookup(i, env));
    });

const send_value = define_form((self, exp) =>
    args => {
        const [k] = list_to_vector(args);
        return send(k, exp);
    });

const constructed_function = define_form((self, args, cf) => {
    const r = cf(args);
    if (typeof r === 'function') {
        return wrap_global_lambda(r, self);
    }
    return r;
});

const global_lambda = define_form((self, defn) =>
    wrap_global_lambda(find_global(defn), self));

const if0 = define_form((self, scan0, scan1, scan2) =>
    args => {
        const [k, env] = list_to_vector(args);
        return scan0(cons(make_form(if1, k, env, scan1, scan2),
                          cons(env, null)));
    });

const if1 = define_form((self, k, env, scan1, scan2) =>
    args => {
        const [v] = list_to_vector(args);
        const f = v ? scan1 : scan2;
        return f(cons(k, cons(env, null)));
    });

const sclis0 = define_form((self, first, rest) =>
    args => {
        const [k, env] = list_to_vector(args);
        return first(cons(make_form(sclis1, k, env, rest),
                          cons(env, null)));
    });

const sclis1 = define_form((self, k, env, rest) =>
    args => {
        const [v] = list_to_vector(args);
        return rest(cons(make_form(sclis2, k, v),
                         cons(env, null)));
    });

const sclis2 = define_form((self, k, v) =>
    args => {
        const [w] = list_to_vector(args);
        return send(k, cons(v, w));
    });

const scseq0 = define_form((self, first, rest) =>
    args => {
        const [k, env] = list_to_vector(args);
        return first(cons(make_form(scseq1, k, env, rest),
                          cons(env, null)));
    });

const scseq1 = define_form((self, k, env, rest) =>
    () => rest(cons(k, cons(env, null))));

const lambda0 = define_form((self, params, scanned) =>
    args => {
        const [k, env] = list_to_vector(args);
        return send(k, make_form(lambda1, params, scanned, env));
    });

const lambda1 = define_form((self, params, scanned, env) =>
    args => handle_lambda(args, params, scanned, env, extend_env));

const improper_lambda0 = define_form((self, params, scanned) =>
    args => {
        const [k, env] = list_to_vector(args);
        return send(k, make_form(improper_lambda1, params, scanned, env));
    });

const improper_lambda1 = define_form((self, params, scanned, env) =>
    args => handle_lambda(args, params, scanned, env, improper_extend_env));

const letcc0 = define_form((self, name, scanned) =>
    args => {
        const [k, env] = list_to_vector(args);
        return scanned(cons(k,
                            cons(extend_env(env,
                                            cons(name, null),
                                            cons(make_form(letcc1, k), null)),
                                 null)));
    });

const letcc1 = define_form((self, k) =>
    args => handle_contn_lambda(args, k));

const define0 = define_form((self, name, i, scanned) =>
    args => {
        const [k, env] = list_to_vector(args);
        return scanned(
            cons(make_form(define1, k, env, name, i),
                 cons(env, null)));
    });

const define1 = define_form((self, k, env, name, i) =>
    args => {
        const [v] = list_to_vector(args);
        return send(k, ctenv_setvar(name, i, v, env));
    });

const application0 = define_form((self, scanned) =>
    args => {
        const [k, env] = list_to_vector(args);
        return scanned(cons(make_form(application1, k, env),
                            cons(env, null)));
    });

const application1 = define_form((self, k, env) =>
    args => {
        const [v] = list_to_vector(args);
        return applyx(k, env, list_ref(v, 0), list_rest(v, 0));
    });

const evalx_initial = define_form((self, k, env, scanned) =>
    () => scanned(cons(k, cons(env, null))));

function make_form(n, ...args) {
    const defn = cons(n, vector_to_list(args));
    const f2 = memoize_lambda(args => f(args), defn);
    args.unshift(f2);
    const f = memoize_lambda(forms[defn.car](...args), defn);
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
                f = make_form(r.car, ...list_to_vector(r.cdr));
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

function unmemoize_aux(exp, tab, fn) {
    if (exp instanceof Pair) {
        return cmap(fn, exp, tab, table_set);
    }
    if (Array.isArray(exp)) {
        return vector_cmap(fn, exp, tab, table_set);
    }
    if (typeof exp === 'function') {
        const ref = tab.get(exp);
        if (ref !== undefined) {
            return ref;
        }
        const entry = table_set(tab, exp, []);
        entry.push(new Symbol('MCE-UNMEMOIZED'));
        entry.push(fn(get_procedure_defn(exp)));
        return entry;
    }
    return exp;
}

function unmemoize(exp) {
    const tab = new Map();
    const fn = x => unmemoize_aux(x, tab, fn);
    return fn(exp);
}

function make_serialized(n) {
    return [new Symbol('MCE-SERIALIZED'), n];
}

function is_serialized(v) {
    return (v.length === 2) &&
           (v[0] instanceof Symbol) &&
           (v[0].toString() === 'MCE-SERIALIZED');
}

function serialized_n(v) {
    return v[1];
}

function serialize_aux(exp, tab, fn, set_entry) {
    if (exp instanceof Pair) {
        return cmap(fn, exp, tab, set_entry);
    }
    if (Array.isArray(exp)) {
        return vector_cmap(fn, exp, tab, set_entry);
    }
    return exp;
}

function serialize(exp) {
    let counter = 0;
    const tab = new Map();
    const set_entry = (tab, v, entry) => {
        table_set(tab, v, make_serialized(counter++));
        return entry;
    };
    const fn = x => serialize_aux(x, tab, fn, set_entry);
    return fn(exp);
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

const null_code    = 'a';
const boolean_code = 'b';
const number_code  = 'c';
const char_code    = 'd';
const string_code  = 'e';
const symbol_code  = 'f';
const pair_code    = 'g';
const vector_code  = 'h';

function pickle_aux(exp) {
    const j = [];
    if (exp === null) {
        j.push(null_code);
    } else if (typeof exp === 'boolean') {
        j.push(boolean_code);
        j.push(exp ? 't' : 'f');
    } else if (typeof exp === 'number') {
        j.push(number_code);
        j.push(exp);
    } else if (exp instanceof Char) {
        j.push(char_code);
        j.push(exp.toString());
    } else if (typeof exp === 'string') {
        j.push(string_code);
        j.push(exp);
    } else if (exp instanceof Symbol) {
        j.push(symbol_code);
        j.push(exp.toString());
    } else if (exp instanceof Pair) {
        j.push(pair_code);
        j.push(pickle_aux(exp.car));
        j.push(pickle_aux(exp.cdr));
    } else if (Array.isArray(exp)) {
        j.push(vector_code);
        j.push(pickle_aux(vector_to_list(exp)));
    } else {
        throw new UnknownPickleExpressionError(exp);
    }
    return j;
}

function pickle(exp) {
    return JSON.stringify(pickle_aux(exp));
}

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

function mce_save(exp) {
    return pickle(serialize(unmemoize(exp)));
}

function mce_restore(s) {
    return memoize(deserialize(unpickle(s)));
}

function result(exp) {
    return [new Symbol('MCE-RESULT'), exp];
}

function is_result(exp) {
    return Array.isArray(exp) &&
           (exp.length === 2) &&
           (exp[0] instanceof Symbol) &&
           (exp[0].toString() === 'MCE-RESULT');
}

function result_val(exp) {
    return exp[1];
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
    //print(list_ref(get_procedure_defn(r), 3));
    if (typeof r === 'function') {
        r(cons(null, null));
    } else {
        run(r);
    }
})();
