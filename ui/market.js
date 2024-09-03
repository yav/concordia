class Market {
  constructor(board) {
    this.spots = new List(() => new MarketSpot(uiGet("market"), board))
    this.count = new Text(uiGet("card-count"), false)
    this.board = board
  }

  destroy() { this.spots.destroy(); this.count.destroy() }
  set([count,cards]) { this.spots.set(cards); this.count.set(count.toString()) }

  setPos() {
    const board = this.board
    const market = uiGet("market")
    const tl = board.json.loc.market.TL
    const br = board.json.loc.market.BR
    board.setLoc(market, tl)
    board.setDim(market, [ 32 + br[0] - tl[0], 32 + br[1] - tl[1] ]) 
    this.spots.map((el) => el.setSize())
  }

  ask(n,q) {
    this.spots.getElements()[n].ask(q)
  }
}


class MarketSpot {
  constructor(owner,board) {
    const [dom,els] = uiFromTemplateNested("market-spot")
    this.dom = dom
    this.card     = new Card(els.market_card)
    this.cardCost = new List(() => new Resource(els.card_cost, [32,32], board))
    this.spotCost = new List(() => new ResourceCost(els.spot_cost, board))
    owner.appendChild(dom)
  }

  setSize() {
    this.cardCost.map((el) => el.setSize())
    this.spotCost.map((el) => el.setSize())
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

  ask(q) { this.card.ask(q) }
}

