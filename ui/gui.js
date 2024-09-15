class GUI {
  constructor() {
    const playerContainer = uiGet("players")
    const handContainer   = uiGet("hand")
    this.question         = new Text(uiGet("question"),false)
    this.players          = new List(() => new Player(playerContainer))
    this.hand             = new Hand()
    this.score            = new FinalScore()
    this.board            = new Board()
    this.log              = new List(() => new LogEntry(this.board))
    this.undo             = new UndoButton()
    monitorSize(this)
  }

  resize() { this.board.resize() }

  async set(obj) {
    await this.board.set(obj.boardInfo)
    this.players.set(obj.playerInfo)
    this.log.set(obj.logMessages)
    this.hand.set(obj.hand)
    this.score.set(obj.finished) 
  }

  destroy() {
    this.players.destroy()
    this.hand.destroy()
    this.score.destroy()
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
        this.hand.getElements()[card].askAct(act,q)
        break
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



class Hand extends List {
  constructor() {
    const dom = uiGet("hand")
    super(() => new Card(dom))
    this.visible = new Toggle(dom,"hidden")
  }

  set(xs) {
    this.visible.set(xs.length > 0)
    super.set(xs)
  }
}

class FinalScore extends List {
  constructor() {
    const dom = uiGet("final-score") 
    super (() => new FinalScoreEntry(dom))
    this.visible = new Toggle(dom,"hidden")
    uiGet("final-score-container").appendChild(dom)
  }
  set(xs) {
    const val = xs === null? [] : xs
    this.visible.set(val.length > 0)
    super.set(val)
  }
}

class FinalScoreEntry extends Tuple {
  constructor(own) {
    const [dom,els] = uiFromTemplateNested("final-score-entry")
    super( [ new Text(els.fs_player, true)
           , new Text(els.fs_score, true)
           ] )
    own.appendChild(dom)
  }
}


function monitorSize(gui) {
  let timeout = null
  window.onresize = () => {
    if (timeout !== null) clearTimeout(timeout)
    timeout = setTimeout(() => gui.resize(), 500)
  }
}