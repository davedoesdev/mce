const path = require('path');

module.exports = {
    context: __dirname,
    entry: './stateless.js',
    output: {
        filename: 'stateless_packed.js',
        path: __dirname,
        libraryTarget: 'commonjs2'
    },
    target: 'node',
    stats: {
        warningsFilter: [/node_modules\/yargs/]
    },
    optimization: {
        minimize: false
    }
};
