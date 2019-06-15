// use simple_crypt to sign states

(async () => {
    const priv_pem = process.env.STATELESS_PRIV_PEM;
    delete process.env.STATELESS_PRIV_PEM;

    const { createRequire } = await import('module');
    const require = createRequire(import.meta.url);
    const { Crypt } = require('simple-crypt');
    const { promisify } = require('util');
    //const parse_key = promisify(Crypt.parse_key);
    //const priv_key = await parse_key(priv_pem);
    const make = promisify(Crypt.make.bind(Crypt));
    const crypt = await make(priv_pem, { json: false });
    const sign = promisify(crypt.sign.bind(crypt));

    const { make_runtime } = await import('../mce.mjs');
    const { shtml_to_html } = await import('./shtml.mjs');

    const runtime = make_runtime();
    const save = runtime.get_global_function('save');
    const url_save = async exp => {
        const url = new URL(runtime.get_config('url'));
        const result = await sign(save(exp));
        url.searchParams.set('state', result.data);
        url.searchParams.set('signature', result.signature);
        url.searchParams.set('version', result.version);
        return url.toString();
    };
    const new_save = async (k, env, exp) => {
        return runtime.send(k, await url_save(exp));
    };
    runtime.register_global_function('save', new_save);
    runtime.register_kenv_function(new_save);
    const shtml = await runtime.start(process.argv);
    const html = await shtml_to_html(shtml, url_save);
    console.log(html);
})();
