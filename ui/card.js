

function whenOver(control,msg) {
  const t = new Toggle(msg,"hidden")
  function show() { t.set(true) }
  function hide() { t.set(false) }

  control.addEventListener("mouseenter", show)
  control.addEventListener("mouseleave", hide)
  msg.addEventListener("mouseleave", hide)
  return () => {
    control.removeEventListener("mouseenter", show);
    control.removeEventListener("mouseleave",hide)
    msg.removeEventListener("mouseleave", hide)
  }
}

class CardAction {
  constructor(owner, tooltip) {
    const dom = uiFromTemplate("card-element")
    this.val = null
    this.dom = dom
    this.dom.classList.add("action")
    this.help = new TextEntry(tooltip)
    this.tooltip = tooltip
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

    const top = this.help.getDOM()
    top.innerHTML = ""
    const dom = [top]
    

    function push(tag) { dom.push(document.createElement(tag)) }
    function cur() { return dom[dom.length-1] }
    function pop() { const d = dom.pop(); cur().appendChild(d)   }
    function hl(t) { push("u"); text(t); pop() }
    function sep(tag) { cur().appendChild(document.createElement(tag)) }
    function next() {
      pop()
      const d = document.createElement("div")
      d.style.marginTop = "1ex"
      dom.push(d)
    }
    function list() {
      const d = document.createElement("ol")
      d.setAttribute("type","i")
      d.style.marginLeft = "-2em"
      dom.push(d);
    }

    function text(t) { cur().appendChild(document.createTextNode(t)) }
    function res(r) { new Resource(cur(),[14,14]).set(r) }
    function pres(r) { new PlayerResource(cur(),[14,14]).set({player: conn.playerId,thing:r})}
    function m() { res("Money"); }
    function w() { pres("person"); text("/ "); pres("ship") }
    function h() { pres("house") }
  
    push("div")
    switch (this.act.tag) {
      case "Magister":
        text("Activate the top card of your discard pile.")
        next()
        text("No effect if the activated card is ")
        hl("Senator"); text(", "); hl("Magister")
        text(", or there is no top card.")
        break
      case "Diplomat":
        text("Activate the top card of another player's discard pile.")
        next()
        text("May not choose another "); hl("Diplomat"); text(".")
        break
      case "Tribune":
        list()
        push("li"); text("Take the cards from your discard to your hand."); pop()
        push("li"); text("Gain 1"); res("Money")
        text(" for each card you took, in excess of 3."); pop()
        push("li"); text("May deploy "); w(); text("in the Capital for ")
        res("wheat"); text("+"); res("tool"); pop()
        pop()
        break
      case "Prefect":
        hl("Resource Province")
        list()
        push("li"); text("Gain the resource"); pop()
        push("li"); res("magnus");
        text(": gain the resource again, then ");
        res("magnus");
        text("passes to the previous player."); pop()
        push("li"); hl("All"); text (" players gain "); hl("city");
        text(" resources for their "); res("house"); text("in the province")
        pop()
        push("li"); text("Province switches to"); m(); pop()
        pop()

        push("u"); res("Money"); text ("Province"); pop()
        list()
        push("li"); text("Gain "); res("Money");
        text("for "); hl("all"); m(); text("provinces"); pop()
        push("li"); text("All provinces switch to resource"); pop()
        pop()
        break
      case "Senator":
        text("Buy up to 2 cards from the market")
        break

      case "Architect":
        list()
        push("li"); text("Up to "); hl("N"); text(" times:"); sep("br");
        text("move a "); w(); text("to an adjacent path"); sep("br");
        hl("N"); text(" is the number of deployed "); w(); pop()
        
        push("li"); text("Any number of times:");
        sep("br"); text("Build "); h(); text(" next to "); w();
        pop()
        pop()
        break
      
      case "Mercator":
        list()
        push("li"); text("Gain " + this.act.contents + " "); m(); pop()
        push("li"); text("Up to 2 times:"); sep("br")
        text("buy/sell any number of 1 resource"); pop()
        pop()
        break

      case "Specialist":
        text("Gain 1 "); res(this.act.contents);
        text("for each "); h(); text("in a "); res(this.act.contents); text("city")
        break

      case "Colonist":
        switch(this.act.contents) {
          case "Settle":
            text("Build "); w(); text("in a city with "); h()
            text(" or the Capital"); next()
            text("Cost: "); res("wheat"); text("+"); res("tool")
            next(); text("May repeat any number of times")
            break
          case "Tax":
            list()
            push("li"); text("Gain 5 "); m(); pop()
            push("li"); text("Gain 1 "); m(); text("per deployed "); w(); pop()
            pop()
            break
        }
        break
      default:
        text("TODO")
    }
    pop()
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
        const amt = r === "Cloth"? "5" : r == "Wine"? "4" : "3"
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
