class GUI {
  constructor() {
    const playerContainer = uiGet("players")
    const handContainer   = uiGet("hand")
    this.players          = new List(() => new Player(playerContainer))
    this.hand             = new List(() => new Card(handContainer))
    this.board            = new Board()
  }

  async set(obj) {
    this.players.set(obj.playerInfo)
    this.hand.set(obj.hand)
    await this.board.set(obj.boardInfo)
  }

  destroy() {
    this.players.destroy()
    this.hand.destroy()
    this.board.destroy()
  }

}
