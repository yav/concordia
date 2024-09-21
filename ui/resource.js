

class DOMNode {
  constructor(dom,dim,board) {
    this.dom = dom
    this.desc = new TextTooltip(this.dom,"")
    this.dim = dim
    this.board = board
    this.setSize()
  }

  setSize() {
    if (this.board === undefined) {
      if (this.dim === undefined) return
      const [w,h] = this.dim
      this.dom.style.width = w + "px"
      this.dom.style.height = h + "px"
    } else {
      this.board.setDim(this.dom,this.dim)
    }
  }

  addClass(c) { this.dom.classList.add(c) }
  removeClass(c) { this.dom.classList.remove(c) }
  setOwner(owner) { owner.appendChild(this.dom) }
  setText(txt) { this.dom.textContent = txt }
  setTitle(txt) { this.desc.set(txt) }

  destroy() { this.desc.destroy(); this.dom.remove() }
  ask(q) { gui.quest.existing(this.dom,this.desc,q) }
}

class Resource {
  constructor(owner,dim,board) {
    this.val = null
    const dom = new DOMNode(uiFromTemplate("resource-icon"), dim, board)
    this.dom = dom
    dom.setOwner(owner)
  }

  destroy() { this.dom.destroy() }

  set(x) {
    const xx = x.toLowerCase()
    if (this.val === xx) return false
    if (this.val !== null) this.dom.removeClass(this.val)
    this.val = xx
    this.dom.addClass(this.val)
    const tip = x === "magnus"? "Prefectus Magnus": x
    const prices = { "Brick": 3, "Wheat": 4, "Tool": 5, "Wine": 6, "Cloth": 7 }
    const p = prices[x]
    this.dom.setTitle(p == undefined? tip : tip + " (cost: " + p + ")")
    return true
  }

  setSize() { this.dom.setSize() }

  is(val) { return this.val === val.toLowerCase() }
  ask(q) { this.dom.ask(q) }
}


class WildCost extends Const {
  constructor(owner,board) {
    const dom = new DOMNode(uiFromTemplate("wild-resource"),[32,32],board)
    dom.setOwner(owner)
    super(dom)
    this.dom = dom
    dom.textContent = "?"
    dom.setTitle("Any resource")
  }

  setSize() { this.dom.setSize() }
}


class ResourceCost extends Tagged {
  constructor(owner,board) {
    super({ Any: () => new WildCost(owner,board) 
          , Resource: () => new Resource(owner,[32,32],board)
          })
  }

  setSize() {
    super.map((el) => el.setSize())
  }


}

class PlayerResource {
  constructor(owner, dim, board) {
    const [dom,els] = uiFromTemplateNested("player-resource")
    this.dom = new DOMNode(dom,dim,board)
    this.els = els
    this.player = null
    this.worker = null
    owner.appendChild(dom)
  }

  setSize() { this.dom.setSize() }

  destroy() {
    this.dom.destroy()
    this.els = null
    this.player = null
    this.wolrker = null
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
      if (this.player !== null) this.dom.removeClass(playerColors[this.player])
      this.player = p
      this.dom.addClass(playerColors[this.player])
    }
  }

  is(val) { return this.player === conn.playerId &&
                   val.toLowerCase() === this.worker }
  ask(q) { this.dom.ask(q) }
}

class StoredResource extends Tagged {
  constructor(owner) {
    super({ Available: () => {
              const dom = new DOMNode(uiFromTemplate("available-spot"),[20,20])
              dom.setOwner(owner)
              dom.setTitle("Available")
              return new Const(dom)
            }
          , HasWorker: () => new PlayerResource(owner,[20,20])
          , HasResource: () => new Resource(owner,[20,20])
          })
  }

  isWorker(ty) { return this.tag === "HasWorker" && this.val.is(ty) }
  isResource(ty) { return this.tag === "HasResource" && this.val.is(ty) }
  ask(q) {
    this.val.ask(q)
  }
}



