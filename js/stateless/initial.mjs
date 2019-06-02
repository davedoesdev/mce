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

function _parse_sxml(exp, parent) {
    const doc = parent.ownerDocument;
    const append = parent.appendChild.bind(parent);
    if (typeof exp === 'boolean') {
        append(doc.createTextNode(exp ? 'true' : 'false'));
    } else if ((typeof exp === 'number') ||
               (exp instanceof Char) ||
               (typeof exp === 'string')) {
        append(doc.createTextNode(exp.toString()));
    } else if (exp instanceof Symbol) {
        const tag = exp.toString();
        if (tag === '@') {
            parent = new Attribute(parent);
        } else {
            if (parent === doc.documentElement) {
                if (tag.localeCompare(parent.tagName, undefined, { sensitivity: 'accent' }) !== 0) {
                    const matches = parent.getElementsByTagName(tag);
                    if (matches.length) {
                        parent = matches.item(0);
                    } else {
                        parent = append(doc.createElement(tag));
                    }
                }
            } else {
                parent = append(doc.createElement(tag));
            }
        }
    } else if (exp instanceof Pair) {
        let p = parent;
        while (exp instanceof Pair) {
            p = _parse_sxml(exp.car, p);
            exp = exp.cdr;
        }
    } else if (Array.isArray(exp)) {
        let p = parent;
        for (let v of exp) {
            p = _parse_sxml(v, p);
        }
    }
    return parent;
}

function parse_sxml(sxml) {
    const dom = new JSDOM('<!doctype html>');
    _parse_sxml(sxml, dom.window.document.documentElement);
    return dom;
}

(async function () {
    const sxml = await start(process.argv);
    const dom = parse_sxml(sxml);
    console.log(dom.serialize());
})();
