class Resource {
  constructor(owner) {
    this.val = null
    const dom = uiFromTemplate("resource-icon")
    this.dom = dom
    owner.appendChild(dom)
  }

  destroy() { this.dom.remove() }

  set(x) {
    const xx = x.toLowerCase()
    if (this.val === xx) return
    if (this.val !== null) this.dom.classList.remove(this.val)
    this.val = xx
    this.dom.classList.add(this.val)
    this.dom.setAttribute("title", x)
    this.dom.textContent = x === "Any"? "?" : ""

  }
}

class ResourceCost extends Tagged {
  constructor(owner) {
    super({ Any: () => new Const(owner,uiFromTemplate("wild-resource"))
          , Resource: () => new Resource(owner)
          })
  }
}

class WorkerResource {
  constructor(owner) {
    const [dom,els] = uiFromTemplateNested("worker-icon")
    this.dom = dom
    this.els = els
    this.color = null
    this.worker = null
    owner.appendChild(dom)
  }
  destroy() {
    this.dom.remove()
    this.els = null
  }
  set(obj) {
    const w = obj.worker
    const ww = w.toLowerCase()
    if (ww !== this.worker) {
      this.els.img.setAttribute("href","icons/" + ww + ".svg#id")
      this.worker = ww
      this.dom.setAttribute("title",w)
    }

    const p = obj.color
    if (p !== this.color) {
      if (this.color !== null) this.dom.classList.remove(this.color)
      this.color = p
      this.dom.classList.add(this.color)
    }
  }
}

class PlayerResource extends Tagged {
  constructor(owner) {
    super({ Available: () => new Const(owner,uiFromTemplate("available-spot"))
          , HasWorker: () => new WorkerResource(owner)
          , HasResource: () => new Resource(owner)
          })
  }
}


class BoardResource {
  constructor() {
    this.val = null
    const dom = uiFromTemplate("board-resource")
    this.dom = dom
    uiGet("board").appendChild(dom)
  }
  destroy() { this.dom.remove() }
  set(x) {
    const xx = x.toLowerCase()
    if (this.val === xx) return
    if (this.val !== null) this.dom.classList.remove(this.val)
    this.val = xx
    this.dom.classList.add(this.val)
    this.dom.setAttribute("title", x)
  }
  setPos(x,y) {
    const sty = this.dom.style
    sty.left = x + "px"
    sty.top = y + "px"
  }
}

