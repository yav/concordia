class Question {
  constructor() { this.cleanup = [] }
  reset() {
    for(const f of this.cleanup) f()
    this.cleanup = []
  }
  register(f) { this.cleanup.push(f) }
  resolve(q) {
    this.reset()
    conn.sendJSON(q)
  }
  undo() {
    this.reset()
    conn.sendJSON({tag: "undo"})
  } 
  existing(dom,q) {
    const h = () => this.resolve(q)
    this.register(() => {
      dom.removeEventListener("click",h)
      dom.classList.remove("clickable")
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
    dom.textContent = q.chChoice.contents
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