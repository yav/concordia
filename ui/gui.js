class GUI {
  constructor() {
    const playerContainer = uiGet("players")
    const handContainer   = uiGet("hand")
    this.question         = new Text(uiGet("question"),false)
    this.players          = new List(() => new Player(playerContainer))
    this.hand             = new List(() => new Card(handContainer))
    this.board            = new Board()
    this.log              = new List(() => {
      const d = uiFromTemplate("log-entry")
      uiGet("log-entries").appendChild(d)
      return new Text(d, true)
    })
  }

  async set(obj) {
    await this.board.set(obj.boardInfo)
    this.players.set(obj.playerInfo)
    this.hand.set(obj.hand)
    this.log.set(obj.logMessages)
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
      case "AskText":
        uiGet("question").appendChild(quest.button(q))
        break
      default: console.log(q) 
    }
  }

}
