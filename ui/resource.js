

class DOMNode {
  constructor(dom,dim,board) {
    this.dom = dom
    if (board === undefined) return
    this.dim = dim
    this.board = board
    this.setSize()
  }

  setSize() {
    if (this.board === undefined) return
    this.board.setDim(this.dom,this.dim)
  }

  addClass(c) { this.dom.classList.add(c) }
  removeClass(c) { this.dom.classList.remove(c) }
  setOwner(owner) { owner.appendChild(this.dom) }
  setText(txt) { this.dom.textContent = txt }
  setTitle(txt) { this.dom.setAttribute("title",txt) }

  destroy() { this.dom.remove() }
}

class Resource {
  constructor(owner,board) {
    this.val = null
    const dom = new DOMNode(uiFromTemplate("resource-icon"), [20,20], board)
    this.dom = dom
    dom.setOwner(owner)
  }

  destroy() { this.dom.destroy() }

  set(x) {
    const xx = x.toLowerCase()
    if (this.val === xx) return
    if (this.val !== null) this.dom.removeClass(this.val)
    this.val = xx
    this.dom.addClass(this.val)
    this.dom.setTitle(x)
    this.dom.setText(x === "Any"? "?" : "")
  }

  setSize() { this.dom.setSize() }
}

class ResourceCost extends Tagged {
  constructor(owner,board) {
    super({ Any: () => {
              const dom = new DOMNode(uiFromTemplate("wild-resource"),[20,20],board)
              dom.setOwner(owner)
              return new Const(dom)
            }
          , Resource: () => new Resource(owner,board)
          })
  }


}

class PlayerResource {
  constructor(owner, board) {
    const [dom,els] = uiFromTemplateNested("player-resource")
    this.dom = new DOMNode(dom,[32,32],board)
    this.els = els
    this.player = null
    this.worker = null
    owner.appendChild(dom)
  }

  setSize() { this.dom.setSize() }

  destroy() {
    this.dom.destroy()
    this.els = null
  }
  set(obj) {
    const w = obj.thing
    const ww = w.toLowerCase()
    if (ww !== this.worker) {
      this.els.img.setAttribute("href","icons/" + ww + ".svg#id")
      this.worker = ww
      this.dom.setTitle(w)
    }

    const p = obj.player
    if (p !== this.player) {
      if (this.player !== null) this.dom.classList.remove(playerColors[this.player])
      this.player = p
      this.dom.addClass(playerColors[this.player])
    }
  }
}

class StoredResource extends Tagged {
  constructor(owner) {
    super({ Available: () => {
              const dom = new DOMNode(uiFromTemplate("available-spot"))
              dom.setOwner(owner)
              return new Const(dom)
            }
          , HasWorker: () => new PlayerResource(owner)
          , HasResource: () => new Resource(owner)
          })
  }
}



