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

class ResourceCost {
  constructor(owner) {
    this.resource = new Resource(owner)
  }
  destroy() { this.resource.destroy() }
  set(x) { this.resource.set(x.tag === "Resource"? x.contents : x.tag) }
}

