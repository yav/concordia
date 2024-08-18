
class Card {
  constructor(obj) {
    const dom = document.createElement("div")
    dom.classList.add("card")

    const header = document.createElement("div")
    header.classList.add("card-header")
    for (const act of obj.cardActions) {
      const a = document.createElement("div")
      a.classList.add("card-element")
      a.classList.add("action")
      let label = act.tag
      switch (act.tag) {
        case "Mercator": label += " (" + act.contents + ")"; break
        case "Specialist":
          switch (act.contents) {
            case "Brick": label = "Mason"; break;
            case "Wheat": label = "Farmer"; break;
            case "Tool": label = "Smith"; break;
            case "Wine": label = "Vinter"; break;
            case "Cloth": label = "Taylor"; break;
          }
      }
      a.textContent = label.replaceAll("u","v").toUpperCase()
      header.appendChild(a)
    }
    dom.appendChild(header)

    const footer = document.createElement("div")
    footer.classList.add("card-footer")

    const cost = document.createElement("div")
    cost.classList.add("card-element")
    cost.classList.add("card-cost")
    for (const resource of obj.cardCost) {
      cost.appendChild(resourceIcon(resource))
    }
    footer.appendChild(cost)

    for (const god of obj.cardColor) {
      const a = document.createElement("div")
      a.classList.add("card-element")
      let xxx = god.toLowerCase()
      a.classList.add(xxx)
      a.textContent = god.replaceAll("u","v").toUpperCase()
      footer.appendChild(a)
    }
    dom.appendChild(footer)

    this.dom = dom
  }

  getDom() { return this.dom }
}
