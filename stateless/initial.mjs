import run from './run.mjs';

(async () => {
    const key64 = process.env.STATELESS_KEY;
    delete process.env.STATELESS_KEY;
    const key = Buffer.from(key64, 'base64');
    console.log(await run(process.argv, key));
})();
