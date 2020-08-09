import qs from 'querystring';
import run from './run.mjs';
import sodium_plus from 'sodium-plus';
const { CryptographyKey, SodiumPlus } = sodium_plus;

export async function handler(event) {
    const key64 = process.env.STATELESS_KEY;
    const key = new CryptographyKey(Buffer.from(key64, 'base64'));
    const sodium = await SodiumPlus.auto();

    const form = qs.parse(event.body);
    const state = JSON.parse(form.state);
    const msg = Buffer.from(state.msg, 'base64');
    const mac = Buffer.from(state.mac, 'base64');
   
    if (!await sodium.crypto_auth_verify(msg, key, mac)) {
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
        body: await run(data, key64, args)
    };
}
