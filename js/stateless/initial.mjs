(async () => {
    const priv_pem = process.env.STATELESS_PRIV_PEM;
    delete process.env.STATELESS_PRIV_PEM;
    const run = (await import('./run.mjs')).default;
    console.log(await run(process.argv, priv_pem));
})();
