import qs from 'querystring';
import micro from 'micro';
const { text, send } = micro;
import run from './run.js';
import simple_crypt from 'simple-crypt';
import util from 'util';
const { promisify } = util;

const make = promisify(simple_crypt.Crypt.make.bind(simple_crypt.Crypt));

export default async (req, res) => {
    const priv_pem = process.env.STATELESS_PRIV_PEM.replace(/\\n/g, '\n');
    const pub_pem = process.env.STATELESS_PUB_PEM.replace(/\\n/g, '\n');

    const form = qs.parse(await text(req));
    const crypt = await make(pub_pem, { json: false });
    const verify = promisify(crypt.verify.bind(crypt));
    const data = await verify(JSON.parse(form.state));

    const args = new Map();
    for (let k in form) {
        args.set(k, form[k]);
    }

    res.setHeader('Content-Type', 'text/html');
    send(res, 200, await run(data, priv_pem, args));
}
