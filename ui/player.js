class Player {

  constructor(owner) {
    const [dom,els] = uiFromTemplateNested("player")
    this.dom = dom
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
  }

  destroy() {
    this.houses_label.destroy()
    for (let i = 0; i < 12; ++i) {
      resources[i].destroy()
    }
    this.dom.remove()
  }
}

