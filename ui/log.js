

class LogEntry extends List {
  constructor(board) {
    const dom = uiFromTemplate("log-entry")
    uiGet("log-entries").appendChild(dom)
    super(() => new LogWord(dom,board))
  }
}


class Pointer {
  constructor (own, ty, lab, board) {
    const dom = uiFromTemplate("log-ref")
    dom.textContent = lab
    this.dom = dom
    this.tgt = null
    
    let sel = null
    dom.addEventListener("mouseenter",() => {
        if (this.tgt === null) return
        console.log(this.tgt)
        const tgt = board.getThing(ty,this.tgt,board)
        if (tgt === null) return
        sel = tgt
        sel.select(true)
    })
    dom.addEventListener("mouseleave",() => {
        if (sel === null) return
        sel.select(false)
        sel = null
    })
    own.appendChild(dom)
  }

  destroy() { this.dom.remove() }
  set(x) { this.tgt = x }
}


class LogWord extends Tagged {
  constructor(own, board) {
    super(
      { T: () => {
          const w = uiFromTemplate("log-word")
          own.appendChild(w)
          return new Text(w,true)
        }
      , G: () => new Resource(own,[14,14])
      , M: () => { const r = new Resource(own,[14,14]); r.set("Money")
                   return new Const(r) }
      , W: () => new PlayerResource(own,[14,14])
      , C: () => new Pointer(own, "cities", "this city", board)
      , P: () => new Pointer(own, "paths", "this path", board)
      , R: () => new Pointer(own, "regions", "this region", board)
      , L: () => {
              const w = new DOMNode(uiFromTemplate("log-line"))
              w.setOwner(own)
              return new Const(w)
           }
      })
  }
}