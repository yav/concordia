
class City {
  constructor(board) {
    const [dom,els] = uiFromTemplateNested("city")
    this.dom = dom
    this.els = els
    this.city = null
    this.produce = new Optional(() => new Resource(els.produces))
    this.workers = new List(() => new PlayerResource(els.workers))
    this.houses = new List(() => new PlayerResource(els.houses))
    this.board = board
    uiGet("board").appendChild(dom)
  }

  destroy() {
    this.produce.destroy()
    this.workers.destroy()
    this.houses.destroy()
    this.dom.remove()
    this.board = null
  }

  setPos() {
    if (this.city === null) return
    const loc = this.board.json.loc.city[this.city]
    const [x,y] = this.board.fromMapLoc(loc)
    const style = this.dom.style
    style.left = x + "px"
    style.top = y + "px"
    const [w,h] = this.board.fromMapLoc([64,64])
    style.width = w + "px"
    style.height = h + "px"
  }

  set(obj) {
    const isNew = this.city === null
    this.city = obj.city
    if (isNew) this.setPos()
    this.produce.set(obj.produces)
    this.workers.set(obj.workers)
    const hs = []
    for (const p in obj.houses) {
      hs.push({ player: p, thing: "House" })
    }
    this.houses.set(hs)
  }

}

