import jsdom from 'jsdom';
import { start, Char, Symbol, Pair } from '../mce.mjs';
const { JSDOM } = jsdom;

class Attribute {
    constructor(el) {
        this.el = el;
        this.attr = null;
        this.ownerDocument = el.ownerDocument;
    }

    appendChild(child) {
        if (child.nodeType === child.ownerDocument.ELEMENT_NODE) {
            this.attr = child.tagName;
            return this;
        }
        
        if (this.attr !== null) {
            this.el.setAttribute(this.attr, child.nodeValue);
        }

        return child;
    }
}

function _parse_sxml(exp, elts) {
    if (typeof exp === 'boolean') {
        elts[0].appendChild(elts[0].ownerDocument.createTextNode(exp ? 'true' : 'false'));
    } else if ((typeof exp === 'number') ||
               (exp instanceof Char) ||
               (typeof exp === 'string')) {
        elts[0].appendChild(elts[0].ownerDocument.createTextNode(exp.toString()));
    } else if (exp instanceof Symbol) {
        if ((elts[0] !== elts[0].ownerDocument.documentElement) || 
            (exp.localeCompare(elts[0].tagName, undefined, { sensitivity: 'accent' }) !== 0)) {
            if (exp.toString() === '@') {
                elts.unshift(new Attribute(elts[0]));
            } else {
                elts.unshift(elts[0].appendChild(elts[0].ownerDocument.createElement(exp.toString())));
            }
        }
    } else if (exp instanceof Pair) {
        const sub_elts = [elts[0]];
        while (exp instanceof Pair) {
            _parse_sxml(exp.car, sub_elts);
            exp = exp.cdr;
        }
    } else if (Array.isArray(exp)) {
        const sub_elts = [elts[0]];
        for (let v of exp) {
            _parse_sxml(v, sub_elts);
        }
    }
}

function parse_sxml(sxml) {
    const dom = new JSDOM();
    _parse_sxml(sxml, [dom.window.document.documentElement]);
    return dom;
}

(async function () {
    const sxml = await start(process.argv);
    const dom = parse_sxml(sxml);
    console.log(dom.serialize());
    // we get 2 bodys - we should skip if any match existing
    // what about doctype?
    // don't need unshift, just current
    // tail recursive
})();
