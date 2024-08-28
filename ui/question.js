class Question {
  constructor() { this.cleanup = [] }
  reset() { this.cleanup = [] }
  register(f) { this.cleanup.push(f) }
  resolve(q) {
    for(const f of this.cleanup) f()
    this.reset()
    conn.sendJSON(q)
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
    this.register(() => dom.remove())
    dom.addEventListener("click",() => this.resolve(q))
    dom.classList.add("clickable")
  }

  button(q) {
    const dom = uiFromTemplate("question-text")
    dom.textContent = q.chChoice.contents
    this.temporary(dom,q)
    return dom
  }

}