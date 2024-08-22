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
    await this.board.set(obj.name)
    this.market.set(obj.market)
    
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

  fromMapLoc([x,y]) { return [ x * this.scaleX, y * this.scaleY ] }

  async set(name) {
    if (this.name === name && this.json !== null) return
    this.name = name
    await this.load()
    // The JSON data is on image which has been scaled
    // to fit in 1920x1080
    const r = this.img.width / this.img.height // r * h = w    
    const normW1 = r * 1080
    const normW = normW1 > 1920 ? 1920 : normW1
    const normH = normW / r
    console.log(normW,normH)

    this.img.style.width = "100%"
    this.scaleX = this.img.width / normW
    this.scaleY = this.img.height / normH
 
    const market = document.getElementById("market").style
    const [tlx,tly] = this.fromMapLoc(this.json.loc.market.TL)
    const [blx,bly] = this.fromMapLoc(this.json.loc.market.BR)
    const marketW = 32 + blx - tlx
    const marketH = 32 + bly - tly
    market.left = tlx + "px"
    market.top = tly + "px"
    market.width = marketW + "px"
    market.height = marketH + "px"

  }
}