// These requires are to get Netlify to include them in the deployment
try { require('./stateless-function.mjs'); } catch {}
try { require('./run.mjs'); } catch {}
try { require('./shtml.mjs'); } catch {}
try { require('@davedoesdev/mce'); } catch {}
try { require('sodium-plus'); } catch {}
try { require('jsdom'); } catch {}

exports.handler = async event => {
    return await (await import('./stateless-function.mjs')).handler(event);
};
