


class ForumTile {
  constructor(owner) {
    const dom = uiFromTemplate("forum-tile")
    this.dom = dom
    this.label = new Text(dom,true)
    this.tooltip = new Tooltip(dom)
    this.help = new TooltipEntry(this.tooltip)
    owner.appendChild(dom)
  }
  destroy() {
    this.label.destroy()
    this.dom.remove()
    this.tooltip.destroy()
  }
  set(l) {
    const changed = this.label.set(l)
    if (changed) this.setHelp()
  }

  setHelp() {
    const val = this.label.getValue()
    if (val === null) return
    switch(val) {
      default:
        this.help.set(toLogWords("TODO"))
    }
  }
}