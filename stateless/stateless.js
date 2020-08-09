// This require is to get Netlify to include stateless.mjs in the deployment
try {
    require('./stateless.mjs');
} catch {}

exports.handler = async event => {
    return await (await import('./stateless.mjs')).handler(event);
};
