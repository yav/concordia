class GUI {
  constructor() {
    const playerContainer = uiGet("players")
    const handContainer   = uiGet("hand")
    this.players          = new List(() => new Player(playerContainer))
    this.hand             = new List(() => new Card(handContainer))
    this.board            = new Board()
  }

  async set(obj) {
    this.players.set(obj.playerInfo)
    this.hand.set(obj.hand)
    await this.board.set(obj.boardInfo)
  }

  destroy() {
    this.players.destroy()
    this.hand.destroy()
    this.board.destroy()
  }

  setQuestion(q) {
    const dom = uiFromTemplate("question-text")
    dom.textContent = q
    uiGet("log").appendChild(dom)
  }

  ask(q) {
    const ch = q.chChoice
    switch(ch.tag) {
      case "AskHand": this.hand.getElements()[ch.contents].ask(q); break
      default: console.log(q) 
    }
  }

}
