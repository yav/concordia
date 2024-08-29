class Path {
  constructor(board) {
    this.path = null
    this.board = board
    this.dom = uiFromTemplate("path-spot")
    this.worker = new Optional(() => new PlayerResource(this.dom,board))
    uiGet("board").appendChild(this.dom)
  }
  destroy() { this.worker.destroy(); this.dom.remove(); this.board = null }

  setPos() {
    if (this.path === null) return
    const loc = this.board.json.loc.edge[this.path]
    this.board.setLoc(this.dom, loc)
    this.board.setDim(this.dom, [32, 32])
  }

  set([eid,mb]) {
    const isNew = this.path === null
    this.path = eid
    if (isNew) this.setPos()
    this.worker.set(mb)
  }

  is(pid) { return this.path === pid }

  ask(q) {
    quest.existing(this.dom, q)
  }
}