import jsdom from 'jsdom';
import { Char, Symbol, Pair } from './mce.js';
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
            let val = '';
            if (this.el.hasAttribute(this.attr)) {
                val = this.el.getAttribute(this.attr);
            }
            this.el.setAttribute(this.attr, val + child.nodeValue);
        }

        return child;
    }

    reset() {
        this.attr = null;
    }
}

async function _parse_shtml(exp, parent, save) {
    const doc = parent.ownerDocument;
    const append = parent.appendChild.bind(parent);
    if (typeof exp === 'boolean') {
        if ((parent instanceof Attribute) && !exp) {
            parent.reset();
        } else {
            append(doc.createTextNode(exp ? 'true' : 'false'));
        }
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
            p = await _parse_shtml(exp.car, p, save);
            exp = exp.cdr;
        }
    } else if (Array.isArray(exp)) {
        let p = parent;
        for (let v of exp) {
            p = await _parse_shtml(v, p, save);
        }
    } else if (typeof exp === 'function') {
        append(doc.createTextNode(await save(exp)));
    }
    return parent;
}

export async function parse_shtml(shtml, save) {
    const dom = new JSDOM('<!doctype html>');
    await _parse_shtml(shtml, dom.window.document.documentElement, save);
    return dom;
}

export async function shtml_to_html(shtml, save) {
    return (await parse_shtml(shtml, save)).serialize();
}
