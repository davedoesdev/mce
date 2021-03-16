import fs from 'fs';
import util from 'util';
const { promisify } = util;
import os from 'os';
const { EOL } = os;
import yargs from 'yargs';

export class Char extends String {}
export class Symbol extends String {}

export function make_runtime() {

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
           (args[0] instanceof Symbol) &&
           (args[0].toString() === 'MCE-YIELD-DEFINITION');
}

function get_procedure_defn(proc) {
    return proc([new Symbol('MCE-YIELD-DEFINITION'), null]);
}

function memoize_lambda(proc, defn) {
    const f = args => {
        if (is_yield_defn(args)) {
            if (typeof defn === 'function') {
                return defn();
            }
            return defn;
        }
        return proc(args);
    };
    f.memoized = true;
    return f;
}

function vlist_ref(vl, i) {
    while (i > 0) {
        vl = vl[1];
        --i;
    }
    return vl[0];
}

function vlist_rest(vl, i) {
    while (i > 0) {
        vl = vl[1];
        --i;
    }
    return vl[1];
}

function vlist_to_vector(vl) {
    let v = [];
    while (vl !== null) {
        v.push(vl[0]);
        vl = vl[1];
    }
    return v;
}

function vector_to_vlist(v) {
    let vl = null;
    for (let i = v.length - 1; i >= 0; --i) {
        vl = [v[i], vl];
    }
    return vl;
}

function rtenv_lookup(i, env) {
    const r = vlist_ref(env, i[0])[i[1]];
    return r === undefined ? null : r;
}

function rtenv_setvar(i, val, env) {
    const v = vlist_ref(env, i[0]);
    const len = v.length;
    if (i[1] >= len) {
        v.length = i[1] + 1;
        v.fill(null, len);
    }
    v[i[1]] = val;
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

class UnknownUnpickleExpressionError extends Error {
    constructor(exp) {
        super(`unknown unpickle expression: ${exp}`);
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

function minus(n, ...args) {
    for (let arg of args) {
        n -= arg;
    }
    return n;
}

function multiply(...args) {
    let r = 1;
    for (let arg of args) {
        r *= arg;
    }
    return r;
}

function divide(n, ...args) {
    for (let arg of args) {
        n /= arg;
    }
    return n;
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

function vector(...args) {
    return args;
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

function vector_set(v, i, exp) {
    v[i] = exp;
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

function getpid() {
    return process.pid;
}

function cf_test(n, x) {
    if (n == 0) {
        return n2 => cf_test(n2, x);
    } else {
        return x + n;
    }
}

function transfer_test(k, ...args) {
    return applyx(lookup_global(new Symbol('result')),
                  make_global_rtenv(), k, vector_to_vlist(args));
}

const config_table = new Map();

function set_config(k, v) {
    config_table.set(k, v);
}

function get_config(k) {
    const v = config_table.get(k);
    return v === undefined ? false : v;
}

const global_table = new Map([
    ['result', result],
    ['+', plus],
    ['-', minus],
    ['*', multiply],
    ['/', divide],
    ['<', less_than],
    ['>', greater_than],
    ['print', print],
    ['eprint', eprint],
    ['write', write],
    ['write-state', write],
    ['ewrite', ewrite],
    ['null?', is_null],
    ['string?', is_string],
    ['vector?', is_vector],
    ['procedure?', is_procedure],
    ['vector-length', vector_length],
    ['vector-ref', vector_ref],
    ['string=?', is_string_equal],
    ['eq?', is_eq],
    ['=', is_number_equal],
    ['apply', applyx],
    ['save', mce_save],
    ['restore', grestore],
    ['transfer', transfer],
    ['getpid', getpid],
    ['get-config', get_config],
    ['cf-test', cf_test],
    ['transfer-test', transfer_test],
    ['set-gc-callback!', () => null]
]);

const core_globals = [
        result,
        applyx,
        less_than,
        greater_than,
        plus,
        minus,
        multiply,
        divide,
        is_number_equal,
        null,
        is_null,
        vector,
        is_vector,
        vector_length,
        vector_ref,
        vector_set,
        is_procedure,
        is_eq,
        is_string,
        is_string_equal,
        transfer,
        transfer_test
];

function get_global_function(name) {
    return global_table.get(name);
}

function register_global_function(name, f) {
    global_table.set(name, f);
}

function unregister_global_function(name) {
    global_table.delete(name);    
}

const kenvfn_set = new Set([
    applyx,
    transfer,
    grestore
]);

function register_kenv_function(f) {
    kenvfn_set.add(f);
}

function find_global(sym) {
    const r = sym instanceof Symbol ? global_table.get(sym.toString()) : core_globals[sym];
    if (r === undefined) {
        throw new SymbolNotFoundError('find_global', sym);
    }
    return r;
}

function make_step_contn(k, env, args) {
    return [new Symbol('MCE-STEP-CONTN'), [k, [env, args]]];
}

function is_step_contn(args) {
    return args &&
           (args[0] instanceof Symbol) &&
           (args[0].toString() === 'MCE-STEP-CONTN');
}

function step_contn_k(args) {
    return vlist_ref(args, 1);
}

function step_contn_env(args) {
    return vlist_ref(args, 2);
}

function step_contn_args(args) {
    return vlist_rest(args, 2);
}

function make_global_rtenv() {
    return [[], null];
}

function is_transfer(args) {
    return args &&
           (args[0] instanceof Symbol) &&
           (args[0].toString() === 'MCE-TRANSFER');
}

function transfer_args(args) {
    return args[1];
}

function transfer(k, env, fn, ...args) {
    return send(fn, [new Symbol('MCE-TRANSFER'), vector_to_vlist(args)]);
}

function globalize(x, args, cf) {
    if ((typeof x !== 'function') || x.memoized) {
        return x;
    }

    const defn = [constructed_function0, [args, [cf, null]]];
    const f2 = memoize_lambda(args => f(args), defn);
    const f = memoize_lambda(wrap_global_lambda(x, f2), defn);
    return f;
}

function call_global(f, args) {
    return f(...vlist_to_vector(args));
}

function handle_global_lambda(args, fn, cf) {
    if (is_transfer(args)) {
        return call_global(fn, transfer_args(args));
    }

    if (is_step_contn(args)) {
        const sck = step_contn_k(args);
        const sca = step_contn_args(args);
        return sendv(sck, globalize(call_global(fn, sca), sca, cf));
    }

    return globalize(call_global(fn, args), args, cf);
}

function handle_global_lambda_kenv(args, fn) {
    if (is_step_contn(args)) {
        return call_global(fn, [
            step_contn_k(args), [
                step_contn_env(args),
                step_contn_args(args)
            ]
        ]);
    }

    return run(call_global(fn, [
        lookup_global(new Symbol('result')), [
            make_global_rtenv(),
            args
        ]
    ]));
}

function wrap_global_lambda(fn, cf) {
    if (kenvfn_set.has(fn)) {
        return args => handle_global_lambda_kenv(args, fn);
    }
    return args => handle_global_lambda(args, fn, cf);
}

function lookup_global(sym) {
    const r = find_global(sym);
    const defn = [global_lambda, [sym, null]];
    const f2 = memoize_lambda(args => f(args), defn);
    const f = memoize_lambda(wrap_global_lambda(r, f2), defn);
    return f;
}

function extend_rtenv(env, len, values) {
    return [vlist_to_vector(values), env];
}

function improper_extend_rtenv(env, len, values) {
    const v = [];

    for (let i = 0; (i < len) && values; ++i) {
        if (i === (len - 1)) {
            v.push(values);
        } else {
            v.push(values[0]);
            values = values[1];
        }
    }

    return [v, env];
}

function handle_lambda(args, len, fn, env, extend_rtenv) {
    if (is_step_contn(args)) {
        return send(fn, [
            step_contn_k(args), [
                extend_rtenv(env, len, step_contn_args(args)),
                null
            ]
        ]);
    }

    return run(send(fn, [
        lookup_global(new Symbol('result')), [
            extend_rtenv(env, len, args),
            null
        ]
    ]));
}

function handle_contn_lambda(args, k) {
    if (is_transfer(args)) {
        return send(k, transfer_args(args));
    }

    if (is_step_contn(args)) {
        return send(k, step_contn_args(args));
    }

    return run(send(k, args));
}

function applyx(k, env, fn, args) {
    return send(fn, make_step_contn(k, env, args));
}

const forms = []

function define_form(f) {
    const len = forms.length;
    forms.push(f);
    return len;
}

function send(k, vl) {
    return [k, vl];
}

function sendv(k, v) {
    return send(k, [v, null]);
}

const symbol_lookup = define_form((self, i) =>
    args => {
        const [k, env] = vlist_to_vector(args);
        return sendv(k, rtenv_lookup(i, env));
    });

const send_value = define_form((self, exp) =>
    args => {
        const [k] = vlist_to_vector(args);
        return sendv(k, exp);
    });

const constructed_function0 = define_form((self, args, cf) =>
    args2 => {
        return applyx(make_form(constructed_function1, args2),
                      make_global_rtenv(), cf, args);
    });

const constructed_function1 = define_form((self, args) =>
    args2 => {
        const [f] = vlist_to_vector(args2);
        return send(f, args);
    });

const global_lambda = define_form((self, defn) =>
    wrap_global_lambda(find_global(defn), self));

const if0 = define_form((self, scan0, scan1, scan2) =>
    args => {
        const [k, env] = vlist_to_vector(args);
        return send(scan0, [make_form(if1, k, env, scan1, scan2), [env, null]]);
    });

const if1 = define_form((self, k, env, scan1, scan2) =>
    args => {
        const [v] = vlist_to_vector(args);
        const f = v ? scan1 : scan2;
        return send(f, [k, [env, null]]);
    });

const sclis0 = define_form((self, first, rest) =>
    args => {
        const [k, env] = vlist_to_vector(args);
        return send(first, [make_form(sclis1, k, env, rest), [env, null]]);
    });

const sclis1 = define_form((self, k, env, rest) =>
    args => {
        const [v] = vlist_to_vector(args);
        return send(rest, [make_form(sclis2, k, v), [env, null]]);
    });

const sclis2 = define_form((self, k, v) =>
    args => {
        const [w] = vlist_to_vector(args);
        return sendv(k, [v, w]);
    });

const scseq0 = define_form((self, first, rest) =>
    args => {
        const [k, env] = vlist_to_vector(args);
        return send(first, [make_form(scseq1, k, env, rest), [env, null]]);
    });

const scseq1 = define_form((self, k, env, rest) =>
    () => send(rest, [k, [env, null]]));

const lambda0 = define_form((self, len, scanned) =>
    args => {
        const [k, env] = vlist_to_vector(args);
        return sendv(k, make_form(lambda1, len, scanned, env));
    });

const lambda1 = define_form((self, len, scanned, env) =>
    args => handle_lambda(args, len, scanned, env, extend_rtenv));

const improper_lambda0 = define_form((self, len, scanned) =>
    args => {
        const [k, env] = vlist_to_vector(args);
        return sendv(k, make_form(improper_lambda1, len, scanned, env));
    });

const improper_lambda1 = define_form((self, len, scanned, env) =>
    args => handle_lambda(args, len, scanned, env, improper_extend_rtenv));

const letcc0 = define_form((self, scanned) =>
    args => {
        const [k, env] = vlist_to_vector(args);
        return send(scanned, [
            k, [extend_rtenv(env, 1, [make_form(letcc1, k), null]), null]
        ]);
    });

const letcc1 = define_form((self, k) =>
    args => handle_contn_lambda(args, k));

const define0 = define_form((self, i, scanned) =>
    args => {
        const [k, env] = vlist_to_vector(args);
        return send(scanned, [make_form(define1, k, env, i), [env, null]]);
    });

const define1 = define_form((self, k, env, i) =>
    args => {
        const [v] = vlist_to_vector(args);
        return sendv(k, rtenv_setvar(i, v, env));
    });

const application0 = define_form((self, scanned) =>
    args => {
        const [k, env] = vlist_to_vector(args);
        return send(scanned, [make_form(application1, k, env), [env, null]]);
    });

const application1 = define_form((self, k, env) =>
    args => {
        const [v] = vlist_to_vector(args);
        return applyx(k, env, vlist_ref(v, 0), vlist_rest(v, 0));
    });

const evalx_initial = define_form((self, k, env, scanned) =>
    () => send(scanned, [k, [env, null]]));

function make_form(n, ...args) {
    const defn = [n, vector_to_vlist(args)];
    const f2 = memoize_lambda(args => f(args), defn);
    args.unshift(f2);
    const f = memoize_lambda(forms[n](...args), defn);
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
                f = make_form(r[0], ...vlist_to_vector(r[1]));
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
    return [fn(exp), counter];
}

function deserialize_aux(exp, tab, fn, set_entry) {
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
    return fn(exp[0]);
}

const null_code    = 'a';
const boolean_code = 'b';
const number_code  = 'c';
const char_code    = 'd';
const string_code  = 'e';
const symbol_code  = 'f';
const vector_code  = 'g';

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
    } else if (Array.isArray(exp)) {
        j.push(vector_code);
        j.push(exp.length);
        for (let v of exp) {
            j.push(pickle_aux(v));
        }
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
    case null_code:
        return null;
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
    case vector_code:
        return exp.slice(2).map(unpickle_aux);
    default:
        throw new UnknownUnpickleExpressionError(exp);
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

function grestore(k, env, s) {
    return sendv(k, mce_restore(s));
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

async function step(state) {
    return await state[0](state[1]);
}

async function run(state) {
    while (!is_result(state)) {
        state = await step(state);
    }

    return result_val(state);
}

const read_all = promisify((stream, cb) => {
    const bufs = [];
    stream.on('error', cb);
    stream.on('data', buf => bufs.push(buf));
    stream.on('end', () => cb(null, Buffer.concat(bufs)));
});

async function start_string(s, args = null) {
    const r = mce_restore(s);
    if (typeof r === 'function') {
        return await r(args);
    }
    return await run(r);
}

async function start_stream(stream, args = null) {
    return await start_string(JSON.parse(await read_all(stream)), args);
}

async function start(argv) {
    const args = yargs(argv)
        .option('run', {
            describe: 'CPS form or state to run',
            type: 'string'
        })
        .option('config', {
            describe: 'Set configuration',
            type: 'string'
        })
        .argv;
    if (args.config) {
        const pos = args.config.indexOf('=');
        set_config(args.config.substr(0, pos), args.config.substr(pos + 1));
    }
    if (args.run) {
        // yargs removes enclosing double quotes!
        return await start_string(JSON.parse(`"${args.run}"`));
    }
    return await start_stream(process.stdin);
}

return {
    get_config,
    set_config,
    get_global_function,
    register_global_function,
    unregister_global_function,
    register_kenv_function,
    start_string,
    start_stream,
    start,
    send,
    sendv
};

}

export async function start(argv) {
    return await make_runtime().start(argv);
}
