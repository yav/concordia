
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

  add(ch, front=false) { 
    if (front) this.dom.prepend(ch)
    else this.dom.append(ch)
  }

  show() {
    const loc = this.el.getBoundingClientRect();
    const sty = this.dom.style
    sty.left = (loc.left - 5) + "px"
    sty.top  = (loc.bottom + 5) + "px"
    sty.display = "inline-block"
  }

  hide() {
    this.dom.style.display = "none"
  }

  destroy() { this.dom.remove() }
}

class TextEntry  {
  constructor(tp, front = false) {
    const dom = uiFromTemplate("tooltip-entry")
    tp.add(dom, front)
    this.dom = dom
  }
  setHTML(x) {
    this.dom.innerHTML = x
  }
  getDOM() { return this.dom }
}


class TooltipEntry extends List {
  constructor(tp, front = false) {
    const dom = uiFromTemplate("tooltip-entry")
    super(() => new LogWord(dom,undefined))
    this.dom = dom
    tp.add(dom, front)
  }

  destroy() {
    super.destroy()
    this.dom.remove()
  }

  getDOM() { return this.dom }
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