
class Tooltip {
  constructor(el) {
    this.el = el
    this.dom = uiFromTemplate("tooltip-container")
    document.body.appendChild(this.dom)
    this.dom.style.display = "none"
    el.addEventListener("mouseenter", () => this.show())
    el.addEventListener("mouseleave", () => this.hide())
    this.dom.addEventListener("mouseleave", () => this.hide())
  }

  clear() { this.dom.innerHTML = "" }

  add(ch) { this.dom.appendChild(ch) }

  show() {
    const loc = this.el.getBoundingClientRect();
    const sty = this.dom.style
    sty.left = (loc.right + 5) + "px"
    sty.top  = (loc.bottom + 5) + "px"
    sty.display = "inline-block"
  }

  hide() {
    this.dom.style.display = "none"
  }

  destroy() { this.dom.remove() }
}

class TextTooltip extends Tooltip {
  constructor(el,txt) {
    super(el)
    const dom = uiFromTemplate("tooltip-entry")
    this.desc = new Text(dom,true)
    this.desc.set(txt)
    this.add(dom)
  }

  set(txt) { this.desc.set(txt) }
  destroy() { this.desc.destroy(); super.destroy() }
}