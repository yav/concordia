

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
        msg = "Activate the top card of another player's discard pile."
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
    this.tooltip = new Tooltip(dom)
    this.help = new TextEntry(this.tooltip)
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
    this.setHelp()
  }

  setHelp() {
    const dom = this.help.getDOM()
    function text(t) { dom.appendChild(document.createTextNode(t)) }
    function res(r) { new Resource(dom,[14,14]).set(r) }
    function pres(r) { new PlayerResource(dom,[14,14]).set({player: conn.playerId,thing:r})}
    dom.innerHTML = ""
    switch (this.val) {
      case "Vesta":
        dom.textContent = "1 VP per 10 "; res("money")
        break
      case "Jupiter":
        dom.textContent = "1 VP for each "
        pres("house")
        text(" in a non ")
        res("brick")
        text(" city")
        break
        /*
      case "Saturnus":
        lab = "1 VP for each province with at least 1 house"
        break
      case "Mercurius":
        lab = "2 VP for each type of good production, excluding salt"
        break
      case "Mars":
        lab = "2 VP for each worker on the board"
        break
      case "Minerva":
        lab = "Scoring is listed in the the action"
        break
      case "Venus":
        lab = "2 VP for each province with 2 houses (or 1 house for each team member)"
*/
    }
    
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
