import path from 'path';
import { promises as fs } from 'fs';
import { fileURLToPath } from 'url';
import WASI from 'wasi';
const wasi = new WASI({
    args: process.argv.slice(1)
});

(async () => {
    const buf = await fs.readFile(path.join(path.dirname(fileURLToPath(import.meta.url)), '..', 'cpp', 'wasm', 'mce.wasm'));
    const module = await WebAssembly.compile(buf);
    const instance = await WebAssembly.instantiate(module, {
        wasi_unstable: wasi.exports
    });
    wasi.setMemory(instance.exports.memory);
    instance.exports._start();
})();
