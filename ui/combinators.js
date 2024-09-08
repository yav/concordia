// The elements in the list should support `set` and `destroy`
class List {
  constructor(mk) {
    this.mk = mk
    this.els = []
  }

  getElements() { return this.els }

  map(f) { for(const el of this.els) f(el) }

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
      this.els.push(v)
    }
    for (let i = smaller; i < oldLen; ++i) {
      this.els.pop().destroy()
    }
  }

  destroy() {
    for (const i of this.els) i.destroy()
    this.els = []
    this.mk = null
  }
}


class Optional {
  constructor(mk) {
    this.mk = mk
    this.el = null
  }

  map(f) { if (this.el !== null) f(e) }

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
  constructor(thing) { this.thing = thing }
  destroy() { this.thing.destroy(); this.thing = null }
  set() {}
  map(f) { f(this.thing) }
}


class Text {
  constructor(dom, own) {
    this.own = own
    this.dom = dom
    this.val = null
  }
  destroy() {
    if (this.own) { this.dom.remove(); }
    else { this.dom.textContent = "" }
  }
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
  
  map(f) { for (const i in this.obj) f(this.obj[i]) }

  getElement(i) { return this.obj[i] }
}

// Assumes the lenght of the array are not changing
class Tuple {
  constructor(arr) { this.arr = arr }

  set(arr) {
    for (let i = 0; i < this.arr.length; ++i) this.arr[i].set(arr[i])
    }

  destroy() { for (const o of this.obj) o.destroy() }
  
  map(f) { for (const o of this.obj) f(o) }

  getElement(i) { return this.obj[i] }
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
    if (this.tag === tag) this.val.set(v.contents)
    else {
      if (this.val !== null) this.val.destroy()
      const el = this.mk[tag]()
      el.set(v.contents)
      this.tag = tag
      this.val = el
    }
  }

  map(f) { if (this.val !== nul) f(this.val) } 

  getElement(tag) {
    if (this.tag === tag) return this.val
    return null
  }
}


class Toggle {
  constructor(dom, hidden) {
    this.visible = false
    this.hidden = hidden
    this.dom = dom
    dom.classList.add(hidden)
  }

  set(vis) {
    if (this.visible === vis) return
    if (vis) { this.dom.classList.remove(this.hidden) }
    else this.dom.classList.add(this.hidden)
    this.visible = vis
  }
}

