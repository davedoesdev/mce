import jsdom from 'jsdom';
const { JSDOM } = jsdom;

const char_code = 'B'.charCodeAt(0);
const string_code = 'C'.charCodeAt(0);
const symbol_code = 'D'.charCodeAt(0);

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
    } else if (typeof exp === 'number') {
        append(doc.createTextNode(exp.toString()));
    } else if (exp instanceof Buffer) {
        switch (exp[0]) {
            case char_code:
            case string_code:
                append(doc.createTextNode(exp.slice(1).toString()));
                break;

            case symbol_code:
                const tag = exp.slice(1).toString();
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
                break;

            default:
                append(doc.createTextNode(exp.toString('base64')));
                break;
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
