class Market extends List {
  constructor(board) {
    super(() => new MarketSpot(uiGet("market")))
    this.board = board
  }

  setPos() {
    const board = this.board
    const market = uiGet("market").style
    const [tlx,tly] = board.fromMapLoc(board.json.loc.market.TL)
    const [blx,bly] = board.fromMapLoc(board.json.loc.market.BR)
    const marketW = 32 + blx - tlx
    const marketH = 32 + bly - tly
    market.left = tlx + "px"
    market.top = tly + "px"
    market.width = marketW + "px"
    market.height = marketH + "px"
  }
}


class MarketSpot {
  constructor(owner) {
    const [dom,els] = uiFromTemplateNested("market-spot")
    this.card     = new Card(els.market_card)
    this.cardCost = new List(() => new Resource(els.card_cost))
    this.spotCost = new List(() => new ResourceCost(els.spot_cost))
    owner.appendChild(dom)
  }

  destroy() {
    this.card.destroy()
    this.cardCost.destroy()
    this.spotCost.destroy()
    this.dom.remove()
  }

  set(obj) {
    this.card.set(obj.card)
    this.spotCost.set(obj.cost)
    this.cardCost.set(obj.card.cardCost)
  }
}
