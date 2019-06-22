export default async (req, res) => {
    const priv_pem = process.env.STATELESS_PRIV_PEM;
    const pub_pem = process.env.STATELESS_PUB_PEM;
    delete process.env.STATELESS_PRIV_PEM;
    delete process.env.STATELESS_PUB_PEM;

    const qs = (await import('querystring')).default;
    const { text, send } = (await import('micro')).default;
    const { data } = JSON.parse(qs.parse(await text(req)).state);

    const run = (await import('./run.mjs')).default;
    // rework start_string in each lang so they don't expect quoted string
    // only remove in argv version
    console.log(await run(`${JSON.stringify(data)}`, priv_pem));
    // Need to verify signature
    // make runtime and run data, generate html
    //   split code in initial into separate file
    // may have to set content type
    send(res, 200, 'Hello, World');
}
