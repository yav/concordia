

class CardAction {
  constructor(owner) {
    const dom = uiFromTemplate("card-element")
    this.val = null
    this.dom = dom
    this.dom.classList.add("action")
    owner.appendChild(dom)
  }

  destroy() {
    this.dom.remove()
  }

  set(x) {
    const lab = this.actionLabel(x)
    if (this.val === lab) return
    this.dom.textContent = lab
    this.text = lab
  }

  actionLabel(act) {
    switch (act.tag) {
      case "Mercator": return act.tag + " (" + act.contents + ")"
      case "Specialist":
        switch (act.contents) {
          case "Brick": return "Mason"
          case "Wheat": return "Farmer"
          case "Tool":  return "Smith"
          case "Wine":  return "Vinter"
          case "Cloth": return "Taylor"
          default: return act.tag + " (" + act.contents + ")"
        }
    }
    return act.tag
  }

}

class CardType {
  constructor(owner) {
    const dom = uiFromTemplate("card-element")
    this.val = null
    this.dom = dom
    owner.appendChild(dom)
  }
  destroy() {
    this.dom.remove()
  }
  set(x) {
    if (this.val === x) return
    const dom = this.dom
    if (this.val !== null) dom.classList.remove(this.val.toLowerCase())
    this.val = x
    dom.classList.add(x.toLowerCase())
    dom.textContent = x.replaceAll("u","v")
  }
}

class Card {
  constructor(owner) {
    const [dom,els] = uiFromTemplateNested("card-template")
    this.dom = dom
    this.acts = new List(() => new CardAction(els.card_header))
    this.gods = new List(() => new CardType(els.card_footer))
    owner.appendChild(dom)
  }

  set(obj) {
    this.acts.set(obj.cardActions)
    this.gods.set(obj.cardColor)
  }

  destroy() {
    this.acts.destroy()
    this.gods.destroy()
    this.dom.remove()
  }

  ask(q) {
    quest.existing(this.dom,q)
  }

}
