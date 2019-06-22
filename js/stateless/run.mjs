import { make_runtime } from '../mce.mjs';
import { shtml_to_html } from './shtml.mjs';

import simple_crypt from 'simple-crypt';
import util from 'util';
const { promisify } = util;

const make = promisify(simple_crypt.Crypt.make.bind(simple_crypt.Crypt));

export default async (v, priv_pem) => {
    const crypt = await make(priv_pem, { json: false });
    const sign = promisify(crypt.sign.bind(crypt));

    const runtime = make_runtime();
    const save = runtime.get_global_function('save');
    const save_and_sign = async exp => {
        return JSON.stringify(await sign(save(exp)));
    };
    const new_save = async (k, env, exp) => {
        return runtime.send(k, await save_and_sign(exp));
    };
    runtime.register_global_function('save', new_save);
    runtime.register_kenv_function(new_save);
    let shtml;
    if (Array.isArray(v)) {
        shtml = await runtime.start(v);
    } else if (typeof v === 'string') {
        shtml = await runtime.start_string(v);
    } else {
        shtml = await runtime.start_stream(v);
    }
    return await shtml_to_html(shtml, save_and_sign);
}
