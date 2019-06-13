// use env to set keys; delete envs first? (remember modules are loaded first)
// use simple_crypt to sign and encrypt states

(async () => {
    const { make_runtime } = await import('../mce.mjs');
    const { shtml_to_html } = await import('./shtml.mjs');

    const runtime = make_runtime();
    const save = runtime.get_global_function('save');
    const url_save = exp => {
        const url = new URL(runtime.get_config('url'));
        url.searchParams.set('state', save(exp));
        return url.toString();
    };
    runtime.register_global_function('save', url_save);
    const shtml = await runtime.start(process.argv);
    const html = shtml_to_html(shtml, url_save);
    console.log(html);
})();
