
class Card {
  constructor(obj) {
    const dom = document.createElement("div")
    dom.classList.add("card")

    const actions = document.createElement("div")
    for (const act in obj.cardActions) {
    }

    const gods    = document.createElement("div")


    this.dom = dom
  }

  getDom() { return this.dom }
}
