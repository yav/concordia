// The elements in the list should support `set` and `destroy`
class List {
  constructor(mk) {
    this.mk = mk
    this.els = []
  }

  getLengt() { return this.els.length }

  set(xs) {
    const oldLen = this.els.length
    const newLen = xs.length
    const smaller = Math.min(oldLen,newLen)
    for (let i = 0; i < smaller; ++i) {
      this.els[i].set(xs[i])
    }
    for (let i = smaller; i < newLen; ++i) {
      const v = this.mk()
      v.set(xs[i])
      this.els.push()
    }
    for (let i = smaller; i < oldLen; ++i) {
      this.els.pop().destroy()
    }
  }

  destroy() {
    for (i of this.els) i.destroy()
    this.els = []
    this.mk = null
  }
}

class Const {
  constructor(owner,dom) {
    this.dom = dom
    owner.appendChild(dom)
  }
  destroy() { this.dom.remove(); this.dom = null }
  set() {}
}


class Tagged {
  constructor(mk) {
    this.mk  = mk
    this.tag = null
    this.val = null
  }

  destroy() {
    if (this.val !== null) this.val.destroy()
    this.mk = null
    this.tag = null
    this.val = null
  }

  set(v) {
    const tag = v.tag
    if (this.tag === tag) this.val.set(this.contents)
    else {
      if (this.val !== null) this.val.destory()
      const el = this.mk[tag]()
      el.set(v.contents)
      this.tag = tag
      this.val = el
    }
  }
}
