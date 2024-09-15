
class City {
  constructor(board) {
    const [dom,els] = uiFromTemplateNested("city")
    this.dom = dom
    this.els = els
    this.city = null
    this.produce = new Optional(() => new Resource(els.produces, [20,20], board))
    this.workers = new List(() => new PlayerResource(els.workers, [32,32], board))
    this.houses = new List(() => new PlayerResource(els.houses, [32,32], board))
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
    this.board.setLoc(this.dom, loc)
    const [w,h] = this.board.fromMapLoc([64,72])
    this.dom.style.width = w + "px"
    this.dom.style.minHeight = h + "px"
    this.workers.map((el) => el.setSize())
    this.produce.map((el) => el.setSize())
    this.houses.map((el) => el.setSize()) 
  }

  set(obj) {
    const isNew = this.city === null
    this.city = obj.city
    if (isNew) this.setPos()
    this.produce.set(obj.produces)
    this.workers.set(obj.workers)
    const hs = []
    for (const p of obj.playerHouses) {
      hs.push({ player: p, thing: "House" })
    }
    this.houses.set(hs)
  }

  askWorker(ty,q) {
    for (const w of this.workers.getElements()) {
      if (w.is(ty)) { w.ask(q); break }
    }
  }

  ask(q) {
    gui.quest.existing(this.dom,q)
  }

  is(city) { return this.city === city }

  select(yes) {
    if (yes) { this.dom.classList.add("selected") }
    else { this.dom.classList.remove("selected") }
  }

}

