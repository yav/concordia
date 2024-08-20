class Player {

  constructor(owner) {
    const [dom,els] = uiFromTemplateNested("player")
    this.dom = dom
    this.els = els
    this.val = {}
    this.resources = []

    for (let i = 0; i < 12; ++i) {
      this.resources.push(new PlayerResource(this.els[i]))
    }

    owner.appendChild(dom)
  }

  setText(key,x) {
    if (this.val[key] === x) return
    this.val[key]= x
    this.els[key].textContent = x
  }

  set(obj) {
    this.setText("name",    obj.player)
    this.setText("houses",  obj.houses)
    this.setText("cards",   obj.handSize)
    this.setText("money",   obj.money)
    for (let i = 0; i < 12; ++i) {
      this.resources[i].set(obj.resources[i])
    }
  }

  destroy() {
    this.dom.remove()
  }
}

