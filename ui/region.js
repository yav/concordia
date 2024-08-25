class Region extends Tagged {
    constructor(board) {
      const owner = uiGet("board")
      super({ NoBonus: () => new NoBonus(owner)
            , Money: () => new Money(owner)
            , Goods: () => new Resource(owner)
            })

      this.board = board
    }

    setPosition() {
    }


}

class NoBonus extends Const {
    constructor(owner) {
        const dom = uiFromTemplate("resource-icon")
        dom.classList.add("no-resource")
        super(owner,dom)
    }
}

class Money {
    constructor(owner) {
    }
}