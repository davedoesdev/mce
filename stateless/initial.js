import run from './run.js';

(async () => {
    const priv_pem = process.env.STATELESS_PRIV_PEM;
    delete process.env.STATELESS_PRIV_PEM;
    console.log(await run(process.argv, priv_pem));
})();
