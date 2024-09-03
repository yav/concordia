class GUI {
  constructor() {
    const playerContainer = uiGet("players")
    const handContainer   = uiGet("hand")
    this.question         = new Text(uiGet("question"),false)
    this.players          = new List(() => new Player(playerContainer))
    this.hand             = new List(() => new Card(handContainer))
    this.board            = new Board()
    this.log              = new List(() => new LogEntry(this.board))
    this.undo             = new UndoButton()
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
    this.log.destroy()
    this.undo.destroy()
  }

  setQuestion(q) { this.question.set(q) }

  ask(q) {
    const ch = q.chChoice
    switch(ch.tag) {
      case "AskHand":
        const [card,act] = ch.contents
        this.hand.getElements()[card].askAct(act,q); break
      case "AskDiscard":
        const [p,a] = ch.contents
        for (const el of this.players.getElements()) {
          if (el.is(p)) el.askDiscardAction(a,q)
        }
        break
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
      case "AskCityWorker":
        const [city,ty] = ch.contents
        this.board.askCityWorker(city,ty,q)
        break
      case "AskPath":
        this.board.askThing("paths", ch.contents,q)
        break
      case "AskCity":
        this.board.askThing("cities", ch.contents,q)
        break
      case "AskRegion":
        this.board.askThing("regions", ch.contents,q)
        break
      case "AskTextResource":
        uiGet("question").appendChild(quest.buttonResource(q))
        break
      default: console.log(q) 
    }
  }

}
