class Question {
  constructor() {
    this.cleanup = []
    this.indicator = new TurnIndicator()
  }

  reset() {
    for(const f of this.cleanup) f()
    this.cleanup = []
    this.indicator.hide()
  }
  register(f) { this.cleanup.push(f); this.indicator.show() }
  resolve(q) {
    this.reset()
    conn.sendJSON(q)
  }
  undo() {
    this.reset()
    conn.sendJSON({tag: "undo"})
  } 
  existing(dom,tooltip,q) {
    const help = tooltip === undefined? new Tooltip(dom) : tooltip
    const ent = new TooltipEntry(help, true)
    const clean = whenOver(dom, ent.getDOM())
    ent.set(toLogWords(q.chHelp))
    const h = () => this.resolve(q)
    this.register(() => {
      dom.removeEventListener("click",h)
      dom.classList.remove("clickable")
      clean()
      ent.destroy()
      if (tooltip === undefined) help.destroy()
    })
    dom.addEventListener("click",h)
    dom.classList.add("clickable")
  }
  temporary(dom,q) {
    const help = new Tooltip(dom)
    const ent = new TooltipEntry(help)
    ent.set(toLogWords(q.chHelp))
    this.register(() => { dom.remove(); help.destroy() } )
    dom.addEventListener("click",() => this.resolve(q))
    dom.classList.add("clickable")
  }

  button(q) {
    const dom = uiFromTemplate("question-text")
    const txt = new LogText(dom)
    txt.set(q.chChoice.contents)
    this.temporary(dom,q)
    return dom
  }

  buttonResource(q) {
    const dom = uiFromTemplate("question-text")
    const [txt,r] = q.chChoice.contents
    dom.textContent = txt
    const icon = new Resource(dom,[14,14])
    icon.set(r)
    this.temporary(dom,q)
    return dom
  
  }


}

class TurnIndicator {
  constructor() {
    this.indicator = new Toggle(uiGet("turn-indicator"),"hidden")
    this.disappearing = null
  }

  hide() {
    if (this.disappearing !== null) return
    this.disappearing = setTimeout(() => {
       this.disappearing = null
       this.indicator.set(false)
       
    }, 200)
  }

  show() {
    if (this.disappearing !== null) clearTimeout(this.disappearing)
    this.indicator.set(true)
    this.disappearing = null
  }
}