

class CardAction {
  constructor(owner) {
    const dom = uiFromTemplate("card-element")
    this.val = null
    this.dom = dom
    this.dom.classList.add("action")
    this.tooltip = new Tooltip(this.dom)
    this.help = new TextEntry(this.tooltip)
    this.act = null
    owner.appendChild(dom)
  }

  destroy() {
    this.dom.remove()
    this.tooltip.destroy()
  }

  set(x) {
    const lab = this.actionLabel(x)
    if (this.val === lab) return
    this.act = x
    this.dom.textContent = lab
    this.text = lab
    this.setHelp()
  }

  actionLabel(act) {
    switch (act.tag) {
      case "Mercator": return act.tag + " (" + act.contents + ")"
      case "Colonist": return act.tag + " " + act.contents
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

  setHelp() {
    if (this.act === null) return
    let msg = "TOD"
    switch (this.act.tag) {
      case "Magister":
        msg = "Activate the top card of your discard pile." +
          "<p>The action has <u>no effect</u> if the top card is <u>Senator</u>, <u>Magister</u>, " + 
          "or there is no top card.</p>"
        break
      case "Diplomat":
        msg = "Activate the top card another player's discard pile."
        msg += "<p>May not choose another <u>Diplomat</u>. "
        msg += "The action has no effect if there are no valid targets."
        break
 
    }
    this.help.setHTML(msg)
  }

  ask(q) {
    quest.existing(this.dom,q)
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

  askAct(act,q) {
    this.acts.getElements()[act].ask(q)
  }

  ask(q) {
    quest.existing(this.dom,q)
  }

}
