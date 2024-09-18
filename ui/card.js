

function whenOver(control,msg) {
  const t = new Toggle(msg,"hidden")
  control.addEventListener("mouseenter", () => t.set(true))
  control.addEventListener("mouseleave", () => t.set(false))
  msg.addEventListener("mouseleave", () => t.set(false))
}

class CardAction {
  constructor(owner, tooltip) {
    const dom = uiFromTemplate("card-element")
    this.val = null
    this.dom = dom
    this.dom.classList.add("action")
    this.help = new TextEntry(tooltip)
    whenOver(dom, this.help.getDOM())
    this.act = null
    owner.appendChild(dom)
  }

  destroy() {
    this.dom.remove()
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
          case "Wine":  return "Vintner"
          case "Cloth": return "Weaver"
          default: return act.tag + " (" + act.contents + ")"
        }
    }
    return act.tag
  }

  setHelp() {
    if (this.act === null) return
    let msg = "TODO"
    switch (this.act.tag) {
      case "Magister":
        msg = "<div>Activate the top card of your discard pile." +
          "<p>The action has <u>no effect</u> if the top card is <u>Senator</u>, <u>Magister</u>, " + 
          "or there is no top card.</p></div>"
        break
      case "Diplomat":
        msg = "<div>Activate the top card of another player's discard pile."
        msg += "<p>May not choose another <u>Diplomat</u>. "
        msg += "The action has no effect if there are no valid targets.</p></div>"
        break
 
    }
    this.help.setHTML(msg)
  }

  ask(q) {
    gui.quest.existing(this.dom,this.tooltip, q)
  }
}


class CardType {
  constructor(owner, tooltip) {
    const [dom,els] = uiFromTemplateNested("card-type")
    this.val      = null
    this.dom      = dom
    this.score    = new Text(els.card_score, true)
    this.lab      = els.card_type_name
    this.ownTooltip = tooltip === undefined
    this.toolip     = this.ownTooltip? new Tooltip(dom) : tooltip
    const tp      = tooltip === undefined? new Tooltip(dom) : tooltip
    this.help     = new TextEntry(this.toolip)
    whenOver(dom, this.help.getDOM())
    owner.appendChild(dom)
  }
  destroy() {
    this.dom.remove()
    if (this.ownTooltip) this.toolip.destroy()
  }
  set([x,v]) {
    const vv = v < 0? "?" : v.toString()
    this.score.set(vv)
    if (this.val !== null && this.val.tag === x.tag && this.val.content == x.content) return
    const dom = this.dom
    const lab = this.lab
    if (this.val !== null) dom.classList.remove(this.val.tag.toLowerCase())
    this.val = x
    dom.classList.add(x.tag.toLowerCase())
    lab.textContent = x.tag.replaceAll("u","v")
    this.setHelp()
  }

  setHelp() {
    const dom = this.help.getDOM()
    function text(t) { dom.appendChild(document.createTextNode(t)) }
    function res(r) { new Resource(dom,[14,14]).set(r) }
    function pres(r) { new PlayerResource(dom,[14,14]).set({player: conn.playerId,thing:r})}
    dom.innerHTML = ""
    switch (this.val.tag) {
      case "Vesta":
        dom.textContent = "1 VP per 10 "; res("money")
        break
      case "Jupiter":
        dom.textContent = "1 VP per "
        pres("house")
        text(" in a non ")
        res("brick")
        text(" city")
        break
      case "Saturnus":
        dom.textContent = "1 VP per province with a "
        pres("house")
        break
      case "Mercurius":
        dom.textContent = "2 VP for each type of good production, excluding salt"
        break
      case "Mars":
        dom.textContent = "2 VP per deployed "
        pres("person")
        text(" / ")
        pres("ship")
        break
      case "Minerva":
        const r = this.val.contents
        const amt = r === "Cloth"? "5" : "Wine"? "4" : "3"
        dom.textContent = amt + " VP per"
        pres("house")
        text(" in ")
        res(r)
        text(" city")
        break
      case "Venus":
        dom.textContent = "2 VP per province with "
        pres("house")
        pres("house")
    }
    
  }


}

class Card {
  constructor(owner) {
    const [dom,els] = uiFromTemplateNested("card-template")
    this.dom = dom
    this.tooltip = new Tooltip(dom)
    this.acts = new List(() => new CardAction(els.card_header,this.tooltip))
    this.gods = new List(() => new CardType(els.card_footer,this.tooltip))
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
    this.tooltip.destroy()
  }

  askAct(act,q) {
    this.acts.getElements()[act].ask(q)
  }

  ask(q) {
    gui.quest.existing(this.dom,this.tooltip,q)
  }

}
