// This is for local testing only (netlify dev).
// netlify dev doesn't support ES modules but the Netlify site does.

exports.handler = async event => {
    return await (await import('./stateless-function.mjs')).handler(event);
};
