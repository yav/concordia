class Region {
  constructor(board) {
    const dom = uiFromTemplate("region")
    uiGet("board").appendChild(dom)
    this.val = new Tagged(
      {
        NoBonus: () => new NoBonus(dom)
        , Money: () => new Money(dom)
        , Goods: () => new Resource(dom,[32,32])
      })
    this.region = null
    this.board = board
    this.dom = dom
  }

  set([r, s]) {
    if (this.region !== r) {
      this.region = r
      this.setPosition()
    }
    this.val.set(s)
  }

  destroy() {
    this.region = null
    this.val.destroy()
    this.dom.remove()
  }

  setPosition() {
    const board = this.board
    const loc = board.json.loc.region[this.region]
    board.setLoc(this.dom, loc)
    board.setDim(this.dom, [32, 32])
  }

  is(r) { return this.region === r }

  ask(q) { quest.existing(this.dom,q) }

  select(yes) {
    if (yes) { this.dom.classList.add("selected") }
    else { this.dom.classList.remove("selected") }
  }
}

class NoBonus extends Const {
  constructor(owner) {
    const dom = uiFromTemplate("no-bonus")
    super(owner, dom)
  }
}

class Money extends Text {
  constructor(owner) {
    const dom = uiFromTemplate("gain-money")
    super(dom, true)
    owner.appendChild(dom)
  }
}