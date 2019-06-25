export default async (req, res) => {
    const priv_pem = process.env.STATELESS_PRIV_PEM.replace(/\\n/g, '\n');
    const pub_pem = process.env.STATELESS_PUB_PEM.replace(/\\n/g, '\n');

    const qs = (await import('querystring')).default;
    const { text, send } = (await import('micro')).default;
    const { data } = JSON.parse(qs.parse(await text(req)).state);
    // Need to verify signature

    const run = (await import('./run.mjs')).default;
    res.setHeader('Content-Type', 'text/html');
    send(res, 200, await run(data, priv_pem));
}
