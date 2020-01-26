import { make_runtime } from '@davedoesdev/mce';
import { shtml_to_html } from './shtml.js';

import simple_crypt from 'simple-crypt';
import util from 'util';
const { promisify } = util;

const make = promisify(simple_crypt.Crypt.make.bind(simple_crypt.Crypt));

export default async (v, priv_pem, args = null) => {
    const crypt = await make(priv_pem, { json: false });
    const sign = promisify(crypt.sign.bind(crypt));

    const runtime = make_runtime();
    const save = runtime.get_global_function('save');
    const save_and_sign = async exp => {
        return JSON.stringify(await sign(Buffer.from(save(exp))));
    };
    const new_save = async (k, env, exp) => {
        return runtime.send(k, await save_and_sign(exp));
    };
    runtime.register_global_function('save', new_save);
    runtime.register_kenv_function(new_save);
    runtime.unregister_global_function('restore');

    let alist = null;
    if (args) {
        for (let [k, v] of args) {
            alist = runtime.cons(runtime.cons(k, v), alist);
        }
        alist = runtime.cons(alist, null);
    }

    let shtml;
    if (Array.isArray(v)) {
        shtml = await runtime.start(v);
    } else if ((typeof v === 'string') || Buffer.isBuffer(v)) {
        shtml = await runtime.start_string(v, alist);
    } else {
        shtml = await runtime.start_stream(v, alist);
    }
    return await shtml_to_html(shtml, save_and_sign);
}
