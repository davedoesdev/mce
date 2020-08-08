import run from './run.js';

(async () => {
    const key = process.env.STATELESS_KEY;
    delete process.env.STATELESS_KEY;
    console.log(await run(process.argv, key));
})();
