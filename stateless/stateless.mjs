import qs from 'querystring';
import run from './run.mjs';
import sodium from 'sodium-native';

export async function handler(event) {
    const form = qs.parse(event.body);
    const state = JSON.parse(form.state);
    const msg = Buffer.from(state.msg, 'base64');
    const mac = Buffer.from(state.mac, 'base64');

    const key64 = process.env.STATELESS_KEY;
    const key = Buffer.from(key64, 'base64');
    if (!sodium.crypto_auth_verify(mac, msg, key)) {
        throw new Error('failed to verify state');
    }

    const args = new Map();
    for (let k in form) {
        args.set(k, form[k]);
    }

    return {
        statusCode: 200,
        headers: {
            'Content-Type': 'text/html'
        },
        body: await run(msg, key, args)
    };
}
