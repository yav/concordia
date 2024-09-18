class FinalScore extends List {
  constructor() {
    const dom = uiGet("final-score") 
    super (() => new FinalScorePlayer(dom))
    this.visible = new Toggle(dom,"hidden")
    uiGet("final-score-container").appendChild(dom)
  }
  set(xs) {
    const val = xs === null? [] : xs
    this.visible.set(val.length > 0)
    super.set(val)
  }
}
  
class FinalScorePlayer extends Tuple {
  constructor(own) {
    const [dom,els] = uiFromTemplateNested("final-score-entry")
    super( [ new Text(els.fs_player, true)
           , new FinalScoreBreakDown(els.fs_score)
           ] )
    own.appendChild(dom)
  }
 
}

 
class FinalScoreBreakDown extends Record {
  constructor(own) {
    super(
      { godPoints: new List(() => new FinalScoreGodEntry(own))
      , concordia: new Optional(() => new FinalScoreTextEntry(own, "Concordia"))
      , total: new Optional(() => new FinalScoreTextEntry(own, "Total"))
      }
      , ["godPoints", "concordia", "total"]
    )
  }
}



 class FinalScoreCategory extends Tuple {
  constructor(own, lab, val) {
    const [dom,els] = uiFromTemplateNested("score-category")
    super( [ lab(els["score-category-name"])
           , val(els["score-category-value"])
           ] )
    own.appendChild(dom)
    this.dom = dom
  }
  destroy() { super.destroy(); dom.remove() }
  set(x) { super.set(x) }
}

 class FinalScoreTextEntry extends FinalScoreCategory {
  constructor(own, lab) {
    super(own, el => new Text(el,true), el => new Text(el,true))
    this.lab = lab
  }
  set(xs) { super.set([this.lab, xs]) }
}

class FinalScoreGodEntry extends FinalScoreCategory {
  constructor(own) {
    super(own, el => new CardType(el, undefined)
             , el => new Text(el, true))
  }
  set([x,y,z]) { super.set([ [x,z], y === 0? "" : ("x " + y + " = " + (z * y)) ]) }
}

