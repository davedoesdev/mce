import util from 'util';
const { promisify } = util;
import yargs from 'yargs';

export function make_runtime() {

const true_code    = '0';
const false_code   = '1';
const number_code  = '2';
const vector_code  = '3';
const marker_code  = 'A';

const nil = [];

function vector_cmap(f, vec, tab, set_entry) {
    if (vec.length === 0) {
        return vec;
    }
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

function mark(type) {
    return Buffer.from(marker_code + "MCE-" + type);
}

const yield_defn_mark = mark("YIELD-DEFINITION");

function is_yield_defn(args) {
    return args && Buffer.isBuffer(args[0]) && args[0].equals(yield_defn_mark);
}

function get_procedure_defn(proc) {
    return proc([yield_defn_mark, nil]);
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
    while (true) {
        if (Array.isArray(vl) && (vl.length === 2)) {
            v.push(vl[0]);
            vl = vl[1];
        } else {
            if (!(Array.isArray(vl) && (vl.length == 0))) {
                v.push(vl);
            }
            break;
        }
    }
    return v;
}

function vector_to_vlist(v) {
    let vl = nil;
    for (let i = v.length - 1; i >= 0; --i) {
        vl = [v[i], vl];
    }
    return vl;
}

function rtenv_lookup(i, env) {
    const r = vlist_ref(env, i[0])[i[1]];
    return r === undefined ? nil : r;
}

function rtenv_setvar(i, val, env) {
    const v = vlist_ref(env, i[0]);
    const len = v.length;
    if (i[1] >= len) {
        for (let j = len; j < i[1]; ++j) {
            v[j] = nil;
        }
    }
    v[i[1]] = val;
    return val;
}

class UnexpectedTypeError extends Error {
    constructor(type, exp) {
        super(`not a ${type}: ${exp}`);
    }
}

function check_type(x, pred, name) {
    if (!pred(x)) {
        throw new UnexpectedTypeError(name, x);
    }
}

function check_type_is(x, type) {
    check_type(x, x => typeof x === type, type);
}

class TooShortError extends Error {
    constructor(exp, i) {
        super(`too short for ${i} ref: ${exp}`);
    }
}

function check_length(x, i) {
    if (x.length <= i) {
        throw new TooShortError(x, i);
    }
}

class SymbolNotFoundError extends Error {
    constructor(proc, sym) {
        super(`${proc}: symbol not found: ${sym}`);
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

function plus(...args) {
    let r = 0;
    for (let arg of args) {
        check_type_is(arg, 'number');
        r += arg;
    }
    return r;
}

function minus(n, ...args) {
    check_type_is(n, 'number');
    if (args.length === 0) {
        return -n;
    }
    for (let arg of args) {
        check_type_is(arg, 'number');
       n -= arg;
    }
    return n;
}

function multiply(...args) {
    let r = 1;
    for (let arg of args) {
        check_type_is(arg, 'number');
        r *= arg;
    }
    return r;
}

function divide(n, ...args) {
    check_type_is(n, 'number');
    if (args.length === 0) {
        return 1 / n;
    }
    for (let arg of args) {
        check_type_is(arg, 'number');
        n /= arg;
    }
    return n;
}

function less_than(x, y) {
    check_type_is(x, 'number');
    check_type_is(y, 'number');
    return x < y;
}

function greater_than(x, y) {
    check_type_is(x, 'number');
    check_type_is(y, 'number');
    return x > y;
}

function is_procedure(exp) {
    return typeof exp === 'function';
}

function is_boolean(exp) {
    return typeof exp === 'boolean';
}

function is_number(exp) {
    return typeof exp === 'number';
}

function make_vector(n) {
    check_type_is(n, 'number');
    const v = new Array(n);
    for (let i = 0; i < n; ++i) {
        v[i] = nil;
    }
    return v;
}

function vector_length(v) {
    check_type(v, Array.isArray, 'vector');
    return v.length;
}

function vector_ref(v, i) {
    check_type(v, Array.isArray, 'vector');
    check_length(v, i);
    return v[i];
}

function vector_set(v, i, exp) {
    check_type(v, Array.isArray, 'vector');
    check_length(v, i);
    v[i] = exp;
    return nil;
}

function make_binary(n) {
    check_type_is(n, 'number');
    return Buffer.alloc(n);
}

function binary_length(b) {
    check_type(b, Buffer.isBuffer, 'binary');
    return b.length;
}

function binary_ref(b, i) {
    check_type(b, Buffer.isBuffer, 'binary');
    return b[i];
}

function binary_set(b, i, n) {
    check_type(b, Buffer.isBuffer, 'binary');
    check_type_is(i, 'number');
    b[i] = n;
    return nil;
}

function is_same_object(x, y) {
    return x === y;
}

function is_number_equal(x, y) {
    check_type_is(x, 'number');
    check_type_is(y, 'number');
    return x === y;
}

function getpid() {
    return process.pid;
}

function cf_test(n, x) {
    if (n === 0) {
        return n2 => cf_test(n2, x);
    } else {
        return x + n;
    }
}

function transfer_test(k, ...args) {
    return applyx(lookup_global(g_result),
                  make_global_rtenv(), k, vector_to_vlist(args));
}

const config_table = new Map();

function set_config(k, v) {
    config_table.set(k, v);
}

function get_config(k) {
    const v = config_table.get(k.slice(1).toString());
    return v === undefined ? false : v;
}

function error(proc, msg, obj) {
    throw Error(`${proc}: ${msg} -- ${obj}`);
}

function output_binary(stream, b, start, end) {
    stream.write(b.slice(start, end));
    return nil;
}

const global_table = new Map([
    ['save', mce_save],
    ['restore', restore],
    ['getpid', getpid],
    ['get-config', get_config],
    ['cf-test', cf_test],
    ['set-gc-callback!', () => nil],
    ['output-binary-to-stdout', (...args) => output_binary(process.stdout, ...args)],
    ['output-binary-to-stderr', (...args) => output_binary(process.stderr, ...args)],
    ['error', error]
]);

const core_globals = [
        result,
        applyx,
        is_boolean,
        is_number,
        less_than,
        greater_than,
        plus,
        minus,
        multiply,
        divide,
        is_number_equal,
        Math.floor,
        make_vector,
        Array.isArray,
        vector_length,
        vector_ref,
        vector_set,
        is_procedure,
        make_binary,
        Buffer.isBuffer,
        binary_length,
        binary_ref,
        binary_set,
        error,
        is_same_object,
        transfer,
        transfer_test
];

const g_result = core_globals.indexOf(result);

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
    restore
]);

function register_kenv_function(f) {
    kenvfn_set.add(f);
}

function find_global(sym) {
    const r = Buffer.isBuffer(sym) ? global_table.get(sym.toString('utf8', 1)) : core_globals[sym];
    if (r === undefined) {
        throw new SymbolNotFoundError('find_global', sym);
    }
    return r;
}

const step_contn_mark = mark('STEP-CONTN');

function make_step_contn(k, env, args) {
    return [step_contn_mark, [k, [env, args]]];
}

function is_step_contn(args) {
    return args && Buffer.isBuffer(args[0]) && args[0].equals(step_contn_mark);
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
    return [[], nil];
}

const transfer_mark = mark('TRANSFER');

function is_transfer(args) {
    return args && Buffer.isBuffer(args[0]) && args[0].equals(transfer_mark);
}

function transfer_args(args) {
    return args[1];
}

function transfer(k, env, fn, ...args) {
    return send(fn, [transfer_mark, vector_to_vlist(args)]);
}

function globalize(x, args, cf) {
    if ((typeof x !== 'function') || x.memoized) {
        return x;
    }

    const defn = [constructed_function0, [args, [cf, nil]]];
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
        lookup_global(g_result), [
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

function lookup_global(n) {
    const r = find_global(n);
    const defn = [global_lambda, [n, nil]];
    const f2 = memoize_lambda(args => f(args), defn);
    const f = memoize_lambda(wrap_global_lambda(r, f2), defn);
    return f;
}

function extend_rtenv(env, len, values) {
    return [vlist_to_vector(values), env];
}

function improper_extend_rtenv(env, len, values) {
    const v = [];
    let i = 0;

    while (true) {
        if ((i === (len - 1)) || !Array.isArray(values) || (values.length !== 2)) {
            if (!Array.isArray(values) || (values.length !== 0)) {
                v.push(values);
            }
            break;
        }

        v.push(values[0]);
        values = values[1];
        ++i;
    }

    return [v, env];
}

function handle_lambda(args, len, fn, env, extend_rtenv) {
    if (is_step_contn(args)) {
        return send(fn, [
            step_contn_k(args), [
                extend_rtenv(env, len, step_contn_args(args)),
                nil
            ]
        ]);
    }

    return run(send(fn, [
        lookup_global(g_result), [
            extend_rtenv(env, len, args),
            nil
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
    return send(k, [v, nil]);
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
        return send(scan0, [make_form(if1, k, env, scan1, scan2), [env, nil]]);
    });

const if1 = define_form((self, k, env, scan1, scan2) =>
    args => {
        const [v] = vlist_to_vector(args);
        const f = v ? scan1 : scan2;
        return send(f, [k, [env, nil]]);
    });

const sclis0 = define_form((self, first, rest) =>
    args => {
        const [k, env] = vlist_to_vector(args);
        return send(first, [make_form(sclis1, k, env, rest), [env, nil]]);
    });

const sclis1 = define_form((self, k, env, rest) =>
    args => {
        const [v] = vlist_to_vector(args);
        return send(rest, [make_form(sclis2, k, v), [env, nil]]);
    });

const sclis2 = define_form((self, k, v) =>
    args => {
        const [w] = vlist_to_vector(args);
        return sendv(k, [v, w]);
    });

const scseq0 = define_form((self, first, rest) =>
    args => {
        const [k, env] = vlist_to_vector(args);
        return send(first, [make_form(scseq1, k, env, rest), [env, nil]]);
    });

const scseq1 = define_form((self, k, env, rest) =>
    () => send(rest, [k, [env, nil]]));

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
            k, [extend_rtenv(env, 1, [make_form(letcc1, k), nil]), nil]
        ]);
    });

const letcc1 = define_form((self, k) =>
    args => handle_contn_lambda(args, k));

const define0 = define_form((self, i, scanned) =>
    args => {
        const [k, env] = vlist_to_vector(args);
        return send(scanned, [make_form(define1, k, env, i), [env, nil]]);
    });

const define1 = define_form((self, k, env, i) =>
    args => {
        const [v] = vlist_to_vector(args);
        return sendv(k, rtenv_setvar(i, v, env));
    });

const application0 = define_form((self, scanned) =>
    args => {
        const [k, env] = vlist_to_vector(args);
        return send(scanned, [make_form(application1, k, env), [env, nil]]);
    });

const application1 = define_form((self, k, env) =>
    args => {
        const [v] = vlist_to_vector(args);
        return applyx(k, env, vlist_ref(v, 0), vlist_rest(v, 0));
    });

const evalx_initial = define_form((self, k, env, scanned) =>
    () => send(scanned, [k, [env, nil]]));

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

const unmemoized_mark = mark('UNMEMOIZED');

function is_unmemoized(v) {
    return (v.length === 2) &&
           Buffer.isBuffer(v[0]) &&
           v[0].equals(unmemoized_mark);
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
        entry.push(unmemoized_mark);
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

const serialized_mark = mark('SERIALIZED');

function make_serialized(n) {
    return [serialized_mark, n];
}

function is_serialized(v) {
    return (v.length === 2) &&
           Buffer.isBuffer(v[0]) &&
           v[0].equals(serialized_mark);
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

function pickle_aux(exp) {
    if (typeof exp === 'boolean') {
        return [exp ? true_code : false_code];
    }
    if (typeof exp === 'number') {
        return [number_code, exp];
    }
    if (Buffer.isBuffer(exp)) {
        return exp.toString('base64');
    }
    if (Array.isArray(exp)) {
        const r = [vector_code, exp.length];
        for (let v of exp) {
            r.push(pickle_aux(v));
        }
        return r;
    }
    throw new UnknownPickleExpressionError(exp);
}

function pickle(exp) {
    return JSON.stringify(pickle_aux(exp));
}

function unpickle_aux(exp) {
    if (Array.isArray(exp)) {
        switch (exp[0]) {
        case true_code:
            return true;
        case false_code:
            return false;
        case number_code:
            return exp[1];
        case vector_code:
            return exp.slice(2).map(unpickle_aux);
        default:
            throw new UnknownUnpickleExpressionError(exp);
        }
    }
    if (typeof exp === 'string') {
        return Buffer.from(exp, 'base64');
    }
    throw new UnknownUnpickleExpressionError(exp);
}

function unpickle(s) {
    return unpickle_aux(JSON.parse(s));
}

function mce_save(exp) {
    return Buffer.from(pickle(serialize(unmemoize(exp))));
}

function mce_restore(s) {
    return memoize(deserialize(unpickle(s)));
}

function restore(k, env, s) {
    return sendv(k, mce_restore(s));
}

const result_mark = mark('RESULT');

function result(exp) {
    return [result_mark, exp];
}

function is_result(exp) {
    return Array.isArray(exp) &&
           (exp.length === 2) &&
           Buffer.isBuffer(exp[0]) &&
           exp[0].equals(result_mark);
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

async function start_string_or_buffer(s, args = nil) {
    const r = mce_restore(s);
    if (typeof r === 'function') {
        return await r(args);
    }
    return await run(r);
}

async function start_stream(stream, args = nil) {
    return await start_string_or_buffer(await read_all(stream), args);
}

async function start(argv) {
    const args = yargs(argv)
        .option('run', {
            describe: 'CPS form or state to run',
            type: 'string'
        })
        .option('config', {
            describe: 'Set configuration',
            type: 'string',
            array: true
        })
        .argv;
    if (args.config) {
        for (let config of args.config) {
            const pos = config.indexOf('=');
            set_config(config.substr(0, pos),
                       mce_restore(config.substr(pos + 1)));
        }
    }
    if (args.run) {
        return await start_string_or_buffer(args.run);
    }
    return await start_stream(process.stdin);
}

return {
    set_config,
    get_global_function,
    register_global_function,
    unregister_global_function,
    register_kenv_function,
    start_string_or_buffer,
    start_stream,
    start,
    send,
    sendv,
    nil
};

}

export async function start(argv) {
    return await make_runtime().start(argv);
}
