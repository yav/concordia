

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
        const tgt = board.getThing(ty,this.tgt,board)
        console.log(tgt)
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
      , CID: () => new Pointer(own, "cities", "this city", board)
      , PID: () => new Pointer(own, "paths", "this path", board)
      , RID: () => new Pointer(own, "regions", "this region", board)
      , L: () => {
              const w = new DOMNode(uiFromTemplate("log-line"))
              w.setOwner(own)
              return new Const(w)
           }
      })
  }
}

class LogText extends List {
  constructor(own) {
    super(() => new LogWord(own,undefined))
  }
  set(s) { super.set(toLogWords(s)) }
}

function toLogWords(s) {
  const ws = []
  let str = s
  function text(n) {
    ws.push({tag: "T", contents: str.slice(0,n)})
    str = str.slice(n)
  }

  while (str.length > 0) {
    switch (str.charAt(0)) {
      case '[':
        const end = str.search("]")
        if (end >= 0) {
          const w = str.slice(1,end)
          str = str.slice(end+1)
          let tag = "T"
          switch(w) {
            case "Brick": 
            case "Wheat": 
            case "Tool":  
            case "Wine":  
            case "Cloth": 
            case "House": tag = "G"; break
            case "Money": tag = "M"; break       
          }
          ws.push({tag: tag, contents: w})
        } else {
          text(1)
        }
        break
      
      default:
        const p = str.search("\\[")
        text(p < 0? str.length : p)
    }
  }
  return ws
}