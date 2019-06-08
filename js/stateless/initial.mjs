// should mce be a class and we create instances? yes
// can we automatically save functions?
// use env to set keys; delete envs first? (remember modules are loaded first)
// use simple_crypt to sign and encrypt states

(async () => {
    const {
        start,
        get_global_function,
        register_global_function,
        get_config
    } = await import('../mce.mjs');

    const { shtml_to_html } = await import('./shtml.mjs');

    const save = get_global_function('save');
    register_global_function('save', exp => {
        const url = new URL(get_config('url'));
        url.searchParams.set('state', save(exp));
        return url.toString();
    });

    const shtml = await start(process.argv);
    const html = shtml_to_html(shtml);
    console.log(html);
})();
