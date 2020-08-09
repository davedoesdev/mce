exports.handler = async event => {
    return await (await import('./stateless.mjs')).handler(event);
};
