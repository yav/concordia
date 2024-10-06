class ForumTileSmall {
  constructor(owner) {
    const dom     = uiFromTemplate("forum-tile-small")
    this.label    = new Text(dom,true)
    this.domClass = new DomClass(dom)
    this.tooltip  = new Tooltip(dom)
    this.help     = new ForumTile(this.tooltip.getDom())
    owner.appendChild(dom)
  }
  destroy() {
    this.help.destroy()
    this.label.destroy()
    this.tooltip.destroy()
  }
  set(x) {
    this.help.set(x)
    this.label.set(this.help.getShortLabel())
    console.log(this.help.getClass())
    this.domClass.set(this.help.getClass())
  }
}



class ForumTile {
  constructor(owner) {
    const [dom,els] = uiFromTemplateNested("forum-tile")
    this.dom    = dom
    this.header = new Text(els.name,true)
    this.domClass = new DomClass(els.header)
    this.action = new Text(els.action,true)
    this.help   = new LogText(els.help)
    this.val    = null
    owner.appendChild(dom)
  }
  destroy() {
    this.header.destroy()
    this.action.destroy()
    this.help.destroy() 
    this.dom.remove()
  }
  getClass() { return this.domClass.get() }
  getShortLabel() {
    const txt = this.header.get()
    if (txt === null) { return "??" }
    return txt.split(" ").map((w) => w.charAt(0).toUpperCase()).join("")
  }

  set([l,t]) {
    if (this.val === l) return
    this.val = l
    this.domClass.set(t.toLowerCase())
    this.update()
  }

  ask(q) {
    gui.quest.existing(this.dom, undefined, q)
  }

  update() {
    if (this.val === null) return
    switch(this.val) {
      case "Claudia":
        this.header.set("Claudia Agrippina")
        this.action.set("On Hire")
        this.help.set("Gain 4 storage and 1 [Brick]")
        break
      case "Aulus":
        this.header.set("Aulus Arcadius")
        this.action.set("Architect")
        this.help.set("1[Money] discount on total [House] cost")
        break
      case "Gaius":
        this.header.set("Gaius Marcellus")
        this.action.set("Mercator")
        this.help.set("Sell goods for 1[Money] more")
        break
      case "Donatus":
        this.header.set("Donatus Pompeius")
        this.action.set("Prefect")
        this.help.set("When producing goods also gain the [Money] of the bonus tile")
        break
      case "Titus":
        this.header.set("Titus Valerius")
        this.action.set("Tribune")
        this.help.set("Convert 1 good to [Salt]")
        break
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
        this.header.set(this.val + " XXX")
        this.help.set("TODO")
    }
  }
}