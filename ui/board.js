class Board {
  constructor() {
    const marketContainer = uiGet("market")
    this.market = new List(() => new MarketSpot(marketContainer))
    this.board = new BoardMap()
  }
  destroy() {
    this.board.destroy()
    this.market.destroy()
    this.market = null
  }
  async set(obj) {
    this.market.set(obj.market)
    await this.board.set(obj.name)
  }
}

class BoardMap {
  constructor() {
    this.conatiner = uiGet("board")
    this.name = null

    this.imgResolve = null
    this.imgReject = null
    const img = new Image()
    img.addEventListener("loaderror",() => {
      if (this.imgReject !== null) {
        const f = this.imgReject
        this.imgReject = null
        f()
      }
    })
    img.addEventListener("load", () => {
      if (this.imgResolve !== null) {
        const f = this.imgResolve
        this.imgResolve = null
        f()
      }
    })

    this.img = img
    this.conatiner.append(img)
  }

  destroy() {
    this.imgResolve = null
    this.imgReject = null
    this.json = null
    this.img = null
    this.conatiner.innerHTML = ""
  }


  async load() {
    this.json = null
    
    const imgWait = new Promise((resolve, reject) => {
      this.imgReject = reject
      this.imgResolve = resolve
    })

    const tmp = this.name
    this.img.src = "maps/" + tmp + ".jpg"

    const jsonWait = async () => {
      const result = await fetch("maps/" + this.name + ".json")
      if (!result.ok) throw new Error("Failed to load " + json_url)
      this.json = await result.json()
    }
    await Promise.all([imgWait,jsonWait()])
  }

  async set(name) {
    if (this.name === name && this.json !== null) return
    this.name = name
    await this.load()
    const w = this.img.width
    const h = this.img.height
    const r = w/h
    const bo = document.body
    const rhs = document.getElementById("controls")
    const bow = bo.clientWidth
    const boh = bo.clientHeight
    const is = this.img.style
  /*
    let w1 = 0.8 * bow
    let h1 = w1 / r
    if (h1 > 
  
    // boh*r = bow
    is.width = w1 + "px"
    //is.height = h1 + "px"
    */
   is.width = "100%"
  }
}