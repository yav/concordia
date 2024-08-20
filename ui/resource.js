class Resource {
  constructor(owner) {
    this.val = null
    const dom = uiFromTemplate("resource-icon")
    this.dom = dom
    owner.appendChild(dom)
  }

  destroy() { this.dom.remove() }

  set(x) {
    if (this.val === x) return
    if (this.val !== null) this.dom.classList.remove(this.val)
    this.val = x.toLowerCase()
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

class PlayerResource extends Tagged {
  constructor(owner) {
    super({ Available: () => new Const(owner,uiFromTemplate("available-spot"))
          , HasWorker: () => new Resource(owner) // XXX
          , HasResource: () => new Resource(owner)
          })
  }
}

