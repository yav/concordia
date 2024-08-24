class Market extends List {
  constructor(board) {
    super(() => new MarketSpot(uiGet("market")))
    this.board = board
  }

  setPos() {
    const board = this.board
    const market = uiGet("market")
    const tl = board.json.loc.market.TL
    const br = board.json.loc.market.BR
    board.setLoc(market, tl)
    board.setDim(market, [ 32 + br[0] - tl[0], 32 + br[1] - tl[1] ]) 
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
