class GUI {
  constructor() {
    const playerContainer = uiGet("players")
    const handContainer   = uiGet("hand")
    const marketContainer = uiGet("market")
    this.players          = new List(() => new Player(playerContainer))
    this.hand             = new List(() => new Card(handContainer))
    this.market           = new List(() => new MarketSpot(marketContainer))
  }

  set(obj) {
    this.players.set(obj.playerInfo)
    this.hand.set(obj.hand)
    this.market.set(obj.boardInfo.market)
  }

  destroy() {
    this.players.destroy()
    this.hand.destroy()
    this.market.destroy()
  }

}
