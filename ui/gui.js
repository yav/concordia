class GUI {
  constructor() {
    const playerContainer = uiGet("players")
    const handContainer = uiGet("hand")
    this.quest            = new Question
    this.question         = new Text(uiGet("question"),false)
    this.board            = new Board()
    this.attention        = new Attention()
    this.components = new Record({
      playerInfo: new List(() => new Player(playerContainer)),
      hand: new List(() => new Card(handContainer)),
      handForum: new List(() => new ForumTile(handContainer)),
      logMessages: new List(() => new LogEntry(this.board)),
      finished: new FinalScore()

    })    
    this.undo             = new UndoButton()
    this.version          = new Text(uiGet("version"), false)
    monitorSize(this)
  }

  resize() { this.board.resize() }

  async set(obj) {
    await this.board.set(obj.boardInfo)
    this.components.set(obj)
    this.version.set("version: " + obj.version)
    this.attention.set(obj.attention)
  }

  destroy() {
    this.components.destroy()
    this.question.destroy()
    this.board.destroy()
    this.undo.destroy()
    this.version.destroy()
  }

  setQuestion(q) { this.question.set(q) }

  ask(q) {
    const ch = q.chChoice
    switch(ch.tag) {
      case "AskHand":
        const [card,act] = ch.contents
        this.components.getElement("hand").getElements()[card].askAct(act,q)
        break
      case "AskHandForum":
        this.components.getElement("handForum").getElements()[ch.contents].ask(q)
        break
      case "AskDiscard":
        const [p,a] = ch.contents
        for (const el of this.components.getElement("playerInfo").getElements()) {
          if (el.is(p)) el.askDiscardAction(a,q)
        }
        break
      case "AskMarket":
        this.board.askMarket(ch.contents,q)
        break
      case "AskMarketAct":
        this.board.askMarketAct(ch.contents,q)
        break
      case "AskWorker":
        this.components.getElement("playerInfo").getElements()[0].askWorker(ch.contents,q)
        break
      case "AskResource":
        this.components.getElement("playerInfo").getElements()[0].askResource(ch.contents,q)
        break
      case "AskText":
        uiGet("question").appendChild(this.quest.button(q))
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
        uiGet("question").appendChild(this.quest.buttonResource(q))
        break
      default: console.log(q) 
    }
  }

}


function monitorSize(gui) {
  let timeout = null
  window.onresize = () => {
    if (timeout !== null) clearTimeout(timeout)
    timeout = setTimeout(() => gui.resize(), 500)
  }
}

class Attention {
  constructor() {
    this.ding = uiGet("ding")
    this.sound = uiGet("sound-control")
    this.val = false

    this.enabled = true
    this.update_sound = () => {
      this.enabled = !this.enabled
      const [remove,add] = this.enabled? ["sound_off","sound_on" ]
                                       : ["sound_on", "sound_off"]
      this.sound.classList.remove(remove)
      this.sound.classList.add(add)
    }
    this.update_sound()
    this.sound.addEventListener("click", this.update_sound)
  }

  destroy() {
    this.sound.removeEventListener("click", this.update_sound)
  }

  set(x) {
    if (this.enabled && !this.val && x) {
      this.ding.play()
    }
    this.val = x
  }
}