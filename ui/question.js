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
}