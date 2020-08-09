// These requires are to get Netlify to include the .mjs files in the deployment
try {
    require('./stateless.mjs');
} catch {}
try {
    require('./run.mjs');
} catch {}
try {
    require('./shtml.mjs');
} catch {}
try {
    require('sodium-plus');
} catch {}
try {
    require('@davedoesdev/mce');
} catch {}

exports.handler = async event => {
    return await (await import('./stateless.mjs')).handler(event);
};
