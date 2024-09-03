class UndoButton {
  constructor() {
    this.handler = () => quest.undo()
    const dom = uiGet("undo")
    this.tooltip = new TextTooltip(dom, "Undo")
    dom.addEventListener("click",this.handler)
  }

  destroy() {
    this.tooltip.destroy()
    uiGet("undo").removeEventListener("click",this.handler)
    this.handler = null
  }
}