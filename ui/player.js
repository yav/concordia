class Player {

  constructor(owner) {
    const [dom,els] = uiFromTemplateNested("player")
    this.dom = dom
    this.cur = false
    this.val = new Record(
      { player:   new Text(els.name, true)
      , houses:   new Text(els.houses, true)
      , handSize: new Text(els.cards, true)
      , money:    new Text(els.money, true)
      })
    this.houses_label = new PlayerResource(els.houses_label)
    this.resources = []
    for (let i = 0; i < 12; ++i) {
      this.resources.push(new StoredResource(els[i]))
    }

    owner.appendChild(dom)
  }


  set(obj) {
    this.houses_label.set({player: obj.player, thing: "House" })
    this.val.set(obj)
    for (let i = 0; i < 12; ++i) {
      const r = obj.resources[i]
      if (r.tag === "HasWorker")
        r.contents = { player: obj.player, thing: r.contents }
      this.resources[i].set(r)
    }
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
    for (let i = 0; i < 12; ++i) {
      this.resources[i].destroy()
    }
    this.dom.remove()
  }

  askWorker(ty,q) {
    for (let i = 0; i < 12; ++i) {
      const r = this.resources[i]
      if (r.isWorker(ty)) { r.ask(q); break }
    }
  }
  askResource(ty,q) {
    for (let i = 0; i < 12; ++i) {
      const r = this.resources[i]
      if (r.isResource(ty)) { r.ask(q); break }
    }
  }
}

