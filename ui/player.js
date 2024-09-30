class Player {

  constructor(owner) {
    const [dom,els] = uiFromTemplateNested("player")
    this.dom = dom
    this.player = null
    this.cur = false
    this.magnus = new Optional(() => new Resource(els.magnus, [20,20]))
    this.endGame = new Optional(() => new Resource(els.end_triggered, [20,20]))

    new TextTooltip(els.name,"Player name")
    new TextTooltip(els.houses, "Available houses")
    new TextTooltip(els.cards, "Hand size")
    new TextTooltip(els.money, "Money")
    new TextTooltip(els.top_card, "Last card played")

    this.discardButton = new DiscardButton(els.view_discard, els.discard)

    this.val = new Record(
      { player:   new Text(els.name, true)
      , houses:   new Text(els.houses, true)
      , handSize: new Text(els.cards, true)
      , money:    new Text(els.money, true)
      , discardTop:  new Optional(() => new Card(els.top_card))
      , discard:  new List(() => new Card(els.discard))
      })
    this.houses_label = new PlayerResource(els.houses_label,[20,20])
    this.resources = new List(() => new StoredResource(els.storage))
    owner.appendChild(dom)
  }


  set(obj) {
    this.player = obj.player
    this.discardButton.set(this.player)
    this.houses_label.set({player: obj.player, thing: "House" })
    this.magnus.set(obj.isDouble? "magnus" : null)
    this.endGame.set(obj.triggeredEndGame? "concordia" : null)
    this.val.set(obj)
    const rs = []
    for (const r of obj.resources) {
      if (r.tag === "HasWorker")
        r.contents = { player: obj.player, thing: r.contents }
    }
    this.resources.set(obj.resources)
    if (obj.isCurrent) {
      if (!this.cur) { this.dom.classList.add("current")} 
    }
    else {
      if (this.cur) { this.dom.classList.remove("current") }
    }
    this.cur = obj.isCurrent
  }

  destroy() {
    this.houses_label.destroy()
    this.discardButton.destroy()
    this.resources.destroy()
    this.dom.remove()
  }

  is(p) { return this.player === p }

  askWorker(ty,q) {
    for (const r of this.resources.getElements()) {
      if (r.isWorker(ty)) { r.ask(q); break }
    }
  }
  askResource(ty,q) {
    for (const r of this.resources.getElements()) {
      if (r.isResource(ty)) { r.ask(q); break }
    }
  }
  askDiscardAction(a,q) {
    for (const el of this.val.getElement("discardTop").getElements()) {
      el.askAct(a,q)
    }
  }
}


class DiscardButton {
  constructor(btn,discard) {
    this.player = null
    this.btn = btn
    this.discard = discard
    this.tooltip = new TextTooltip(btn, "View played cards")

    let discardVisible = false
    btn.addEventListener("click",() => {
      discardVisible = !discardVisible
      discard.style.display = discardVisible? "flex" : "none"
    })
  }

  set(p) {
    if (this.player === p) return
    this.player = p
    this.discard.style.display = "none"
    this.btn.style.display =
      this.player === conn.playerId? "inline-block" : "none"
  }

  destroy() {
    this.tooltip.destroy()
  } 
}