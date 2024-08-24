class Board {
  constructor() {
    this.board = new BoardMap()
    this.market = new Market(this.board)
    this.cities = new List(() => new City(this.board))
  }
  destroy() {
    this.board.destroy()
    this.market.destroy()
    this.cities.destroy()
  }
  async set(obj) {
    if (await this.board.set(obj.name)) {
      for (city of this.cities.getElements()) city.setPos()
      this.market.setPos()
    }
    this.market.set(obj.market)
    this.cities.set(obj.cities)
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

  async set(name) {
    if (this.name === name && this.json !== null) return false
    this.name = name
    await this.load()
    // The JSON data is on image which has been scaled
    // to fit in 1920x1080
    const r = this.img.width / this.img.height // r * h = w    
    const normW1 = r * 1080
    let normW = 0
    let normH = 0
    if (normW1 > 1920) {
      normH = 1080
      normW = r * normH
    } else {
      normW = normW1
      normH = normW / r
    }
    const actualW = 0.8 * document.body.clientWidth
    const actualH = actualW / r

    console.log("norm", normW, normH)
    console.log("actual", actualW, actualH)

    this.img.style.width = "100%"
    this.scaleX = actualW / normW
    this.scaleY = actualH / normH
    return true
  }
}


