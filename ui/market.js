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
