exports.handler = async event => {
    return await (await import('./stateless.js')).handler(event);
};
