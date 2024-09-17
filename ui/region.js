class Region {
  constructor(board) {
    const dom = uiFromTemplate("region")
    uiGet("board").appendChild(dom)
    this.val = new Tagged(
      {
        NoBonus: () => new NoBonus(dom,board)
        , Money: () => new Money(dom,board)
        , Goods: () => new Resource(dom,[32,32],board)
      })
    this.region = null
    this.board = board
    this.dom = dom
  }

  set([r, s]) {
    if (this.region !== r) {
      this.region = r
      this.setPos()
    }
    this.val.set(s)
  }

  destroy() {
    this.region = null
    this.val.destroy()
    this.dom.remove()
  }

  setPos() {
    const board = this.board
    const loc = board.json.loc.region[this.region]
    board.setLoc(this.dom, loc)
    board.setDim(this.dom, [32, 32])
    this.val.map((x) => x.setSize())
  }

  is(r) { return this.region === r }

  ask(q) { gui.quest.existing(this.dom,undefined,q) }

  select(yes) {
    if (yes) { this.dom.classList.add("selected") }
    else { this.dom.classList.remove("selected") }
  }
}

class NoBonus extends Const {
  constructor(owner,board) {
    const dom = uiFromTemplate("no-bonus")
    super(owner, dom)
    this.board = board
    this.dom = dom
  }
  setSize() { this.board.setDim(this.dom,[32,32]) }
}

class Money extends Text {
  constructor(owner, board) {
    const dom = uiFromTemplate("gain-money")
    super(dom, true)
    owner.appendChild(dom)
    this.board = board
    this.dom = dom
  }
  setSize() { this.board.setDim(this.dom,[32,32]) }
}