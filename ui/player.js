class Player {

  constructor(owner) {
    const [dom,els] = uiFromTemplateNested("player")
    this.dom = dom
    this.els = els
    this.val = {}
    this.resources = []
    this.houses_label = new PlayerResource(els.houses_label)

    for (let i = 0; i < 12; ++i) {
      this.resources.push(new StoredResource(this.els[i]))
    }

    owner.appendChild(dom)
  }

  setText(key,x) {
    if (this.val[key] === x) return
    this.val[key]= x
    this.els[key].textContent = x
  }

  set(obj) {
    this.houses_label.set({player: obj.player, thing: "House" })
    this.setText("name",    obj.player)
    this.setText("houses",  obj.houses)
    this.setText("cards",   obj.handSize)
    this.setText("money",   obj.money)
    for (let i = 0; i < 12; ++i) {
      const r = obj.resources[i]
      if (r.tag === "HasWorker")
        r.contents = { player: obj.player, thing: r.contents }
      this.resources[i].set(r)
    }
  }

  destroy() {
    this.dom.remove()
  }
}

