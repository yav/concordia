


class ForumTile {
  constructor(owner) {
    const dom = uiFromTemplate("forum-tile")
    this.dom = dom
    this.label = new Text(dom,true)
    this.tooltip = new Tooltip(dom)
    const [help_dom,help_els] = uiFromTemplateNested("forum-tile-help")
    this.tooltip.add(help_dom)
    this.help_header_dom = help_els["header"]
    this.help_header = new Text(help_els["header"],true)
    this.help = new LogText(help_els["help"])
    this.phase = new Text(help_els["action"],true)
    this.val = null
    this.class = null
    owner.appendChild(dom)
  }
  destroy() {
    this.label.destroy()
    this.dom.remove()
    this.help.destroy()
    this.tooltip.destroy()

  }
  set([l,t]) {
    if (this.val !== l) {
      this.val = l
      this.setLabelAndHelp()
      if (t !== this.class) {
        if (this.class !== null) {
          this.dom.classList.remove(this.class)
          this.help_header_dom.classList.remove(this.class)
        }
        this.class = t.toLowerCase()
        this.dom.classList.add(this.class)
        this.help_header_dom.classList.add(this.class)
      }

    }
  }

  setLabelAndHelp() {
    if (this.val === null) return
    switch(this.val) {
      case "Claudia":
        this.label.set("CA")
        this.help_header.set("Claudia Agrippina")
        this.help.set("Gain 4 storage and 1 [Brick]")
        break
      case "Aulus":
        this.label.set("AA")
        this.help_header.set("Aulus Arcadius")
        this.help.set("1 [Money] discount to build a [House]")
        this.phase.set("Architect")
        break
      case "Gaius":
        this.label.set("GM")
        this.help_header.set("Gaius Marcellus")
        this.help.set("Sell goods for 1 [Money] extra.")
        this.phase.set("Mercator")
        break
      case "Donatus":
        this.label.set("DP")
        this.help_header.set("Donatus Pompeius")
        this.help.set("When producing goods also gain [Money] from bonus tile")
        this.phase.set("Prefect")
        break
      case "Titus":
      case "Annaeus":
      case "Lucius":
      case "Servius":
      case "Sextus":
      case "Appius":
      case "Faustus":
      case "Claudius":
      case "Cornelius":
    
      case "Julius":
      case "Mamilius":
      case "Numerius":
      case "Spurius":
      case "Augstus":
      case "Laurentius":
      case "Marcus":
      case "Publius":
      case "Tiberus":
      case "Commodus":
      case "Mamercus":
      case "Novius":
      case "Quintus":
      case "Victoria":

      default:
        this.label.set("XXX")
        this.help_header.set("XXX")
        this.help.set("TODO")
    }
  }
}