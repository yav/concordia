class GUI {
  constructor() {
    const playerContainer = uiGet("players")
    const handContainer   = uiGet("hand")
    this.question         = new Text(uiGet("question"),false)
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

  setQuestion(q) { this.question.set(q) }

  ask(q) {
    const ch = q.chChoice
    switch(ch.tag) {
      case "AskHand":
        const [card,act] = ch.contents
        this.hand.getElements()[card].askAct(act,q); break
      case "AskMarket":
        this.board.askMarket(ch.contents,q)
        break
      case "AskWorker":
        this.players.getElements()[0].askWorker(ch.contents,q)
        break
      case "AskResource":
        this.players.getElements()[0].askResource(ch.contents,q)
        break
      default: console.log(q) 
    }
  }

}
