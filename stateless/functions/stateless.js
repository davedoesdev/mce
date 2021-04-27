// These requires are to get Netlify to include them in the deployment
try { require('../stateless.mjs'); } catch {}
try { require('../run.mjs'); } catch {}
try { require('../shtml.mjs'); } catch {}
try { require('@davedoesdev/mce'); } catch {}
try { require('sodium-plus'); } catch {}
try { require('jsdom'); } catch {}

// This is for local testing (netlify dev) but gets used in deployment too.
// netlify dev doesn't support ES modules but the Netlify site does.
exports.handler = async event => {
    return await (await import('../stateless.mjs')).handler(event);
};
