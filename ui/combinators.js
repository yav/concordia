// The elements in the list should support `set` and `destroy`
class List {
  constructor(mk) {
    this.mk = mk
    this.els = []
  }

  getElements() { return this.els }

  set(xs) {
    const oldLen = this.els.length
    const newLen = xs.length
    const smaller = Math.min(oldLen,newLen)
    for (let i = 0; i < smaller; ++i) {
      this.els[i].set(xs[i])
    }
    for (let i = smaller; i < newLen; ++i) {
      const v = this.mk(i)
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

class Optional {
  constructor(mk) {
    this.mk = mk
    this.el = null
  }

  getElements() { return this.el === null? [] : [this.el] }

  destroy() {
    if (this.el !== null) this.el.destroy()
    this.el = null
  }

  set(x) {
    if (this.el === null) {
      if (x !== null) {
        this.el = this.mk()
        this.el.set(x)
      }
    } else {
      if (x === null) {
        this.el.destroy()
        this.el = null
      } else this.el.set(x)
    }
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

class Text {
  constructor(dom, own) {
    this.own = own
    this.dom = dom
    this.val = null
  }
  destory() { if (this.own) this.dom.remove() }
  set(x) {
    if (this.val === x) return
    this.val = x
    this.dom.textContent = this.val
  }

}

// Assumes the fields of the object are not changing
class Record {
  constructor(obj) {
    this.obj = obj
  }
  set(obj) {
    for (const i in this.obj) {
      this.obj[i].set(obj[i])
    }
  }
  destroy() {
    for (const i in this.obj) {
      this.obj[i].destroy()
    }
  }
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
