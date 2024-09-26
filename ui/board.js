class Board {
  constructor() {
    this.board = new BoardMap()
    this.market = new Market(this.board)
    this.cities = new List(() => new City(this.board))
    this.paths = new List(() => new Path(this.board))
    this.regions = new List(() => new Region(this.board))
  }
  destroy() {
    this.board.destroy()
    this.market.destroy()
    this.cities.destroy()
    this.paths.destroy()
    this.regions.destroy()
  }
  async set(obj) {
    if (await this.board.set(obj.name)) this.resize()
    this.market.set(obj.market)
    this.cities.set(obj.cities)
    this.paths.set(obj.paths)
    this.regions.set(obj.regions)
  }

  resize() {
    this.board.computeSizes()
    for (city of this.cities.getElements()) city.setPos()
    for (region of this.regions.getElements()) region.setPos()
    this.market.setPos()
  }

  askMarket(n,q) { this.market.ask(n,q) }
  askCityWorker(city,ty,q) { this.cities.getElements()[city].askWorker(ty,q) }

  getThing(ty,id) {
    for(const el of this[ty].getElements()) {
      if (el.is(id)) return el
    }
    return null
  }

  askThing(ty,id,q) {
    const t = this.getThing(ty,id)
    if (t !== null) t.ask(q)
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
    this.img.remove()
    this.img = null
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

  setLoc(dom,loc) {
    const [a,b] = this.fromMapLoc(loc)
    dom.style.left = a + "px"
    dom.style.top  = b + "px"
  }

  setDim(dom,loc) {
    const [a,b] = this.fromMapLoc(loc)
    dom.style.width = a + "px"
    dom.style.height = b + "px"
  }

  // Note that we don't compute the sizes here, this is done by the caller
  async set(name) {
    if (this.name === name && this.json !== null) return false
    this.name = name
    await this.load()
    return true
  }

  computeSizes() {
    // The JSON data is on image which has been scaled
    // to fit in 1920x1080
    const r = this.img.width / this.img.height // r * h = w    
    const normW1 = r * 1080
    let normW = 1920
    let normH = normW / r
    if (normH > 1080) {
      normH = 1080
      normW = r * normH
    }
    const actualW = 0.8 * document.body.clientWidth
    const actualH = actualW / r
    this.img.style.width = "100%"
    this.scaleX = actualW / normW
    this.scaleY = actualH / normH
  }
}


