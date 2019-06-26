import qs from 'querystring';
import micro from 'micro';
const { text, send } = micro;
import run from './run.mjs';

export default async (req, res) => {
    const priv_pem = process.env.STATELESS_PRIV_PEM.replace(/\\n/g, '\n');
    const pub_pem = process.env.STATELESS_PUB_PEM.replace(/\\n/g, '\n');

    const form = qs.parse(await text(req));
    const { data } = JSON.parse(form.state);
    const args = new Map();
    for (let k in form) {
        args.set(k, form[k]);
    }
    // Need to verify signature
    //   Override restore - start_string should use registered version

    res.setHeader('Content-Type', 'text/html');
    send(res, 200, await run(data, priv_pem, args));
}
