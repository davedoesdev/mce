import path from 'path';
import { promises as fs } from 'fs';
import { fileURLToPath } from 'url';
import { WASI } from 'wasi';
const wasi = new WASI({
    version: 'preview1',
    args: process.argv.slice(1)
});

(async () => {
    const buf = await fs.readFile(path.join(path.dirname(fileURLToPath(import.meta.url)), '..', 'cpp', 'wasm', 'mce.wasm'));
    const wasm = await WebAssembly.compile(buf);
    const instance = await WebAssembly.instantiate(wasm, {
        wasi_snapshot_preview1: wasi.wasiImport
    });
    wasi.start(instance);
})();
