import { make_runtime } from '@davedoesdev/mce';
import { shtml_to_html } from './shtml.mjs';
import sodium_plus from 'sodium-plus';
const { CryptographyKey, SodiumPlus } = sodium_plus;

export default async (v, key64, args = null) => {
    const sign = async buf => {
        const key = new CryptographyKey(Buffer.from(key64, 'base64'));
        const sodium = await SodiumPlus.auto();
        return {
            msg: buf.toString('base64'),
            mac: (await sodium.crypto_auth(buf, key)).toString('base64')
        };
    };

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

    let vlist = runtime.nil;
    if (args) {
        for (let [k, v] of args) {
            vlist = runtime.cons(runtime.cons(k, v), vlist);
        }
        vlist = runtime.cons(vlist, runtime.nil);
    }

    let shtml;
    if (Array.isArray(v)) {
        shtml = await runtime.start(v);
    } else if ((typeof v === 'string') || Buffer.isBuffer(v)) {
        shtml = await runtime.start_string_or_buffer(v, vlist);
    } else {
        shtml = await runtime.start_stream(v, vlist);
    }
    return await shtml_to_html(shtml, save_and_sign);
}
