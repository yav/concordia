class Market {
  constructor(board) {
    this.spots = new List(() => new MarketSpot(uiGet("market"), board))
    this.button = new MarketButton()
    this.board = board
  }

  destroy() { this.spots.destroy(); this.button.destroy() }
  set([count,cards]) { this.spots.set(cards); this.button.set(count.toString()) }

  setPos() {
    const board = this.board
    board.setLoc(uiGet("market-button"),[0,48])
    board.setLoc(uiGet("market"), [0,85])
  }

  ask(n,q) {
    this.button.setVis(true)
    this.spots.getElements()[n].ask(q)
    gui.quest.register(() => this.button.setVis(false))
  }
  
  askAct([n,i],q) {
    this.button.setVis(true)
    this.spots.getElements()[n].askAct(i,q)
    gui.quest.register(() => this.button.setVis(false))
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
  askAct(i,q) { this.card.askAct(i,q) }
}

class MarketButton {
  constructor() {
    this.dom = uiGet("market-button")
    const market = uiGet("market")
    this.toggle = new Toggle(market,"hidden")
    this.click = () => this.toggle.set(!this.toggle.isVisible())
    this.dom.addEventListener("click",this.click)
    this.count = new Text(uiGet("market-count"),false)
  }

  destroy() { this.count.destroy(); this.dom.reomveEventListener("click",this.click) }
  set(count) { this.count.set(count) }
  setVis(b) { this.toggle.set(b) }

}
