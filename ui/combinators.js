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

  map(f) { if (this.el !== null) f(this.el) }

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
        return true
      } else
        return false
    } else {
      if (x === null) {
        this.el.destroy()
        this.el = null
        return true
      } else
        return this.el.set(x)
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
    if (this.val === x) return false
    this.val = x
    this.dom.textContent = this.val
    return true
  }
  getValue() { return this.val }
  get() { return this.val }

}

// Assumes the fields of the object are not changing
class Record {
  constructor(obj,ord) {
    this.obj = obj
    this.ord = ord === undefined? Object.keys(obj) : ord
  }
  set(obj) {

    for (const i of this.ord) {
      this.obj[i].set(obj[i])
    }
  }
  destroy() {
    for (const i of this.ord) {
      this.obj[i].destroy()
    }
  }
  
  map(f) { for (const i of this.ord) f(this.obj[i]) }

  getElement(i) { return this.obj[i] }
}

// Assumes the lenght of the array are not changing
class Tuple {
  constructor(arr) { this.arr = arr }

  set(arr) {
    for (let i = 0; i < this.arr.length; ++i) this.arr[i].set(arr[i])
    }

  destroy() { for (const o of this.arr) o.destroy() }
  
  map(f) { for (const o of this.arr) f(o) }

  getElement(i) { return this.arr[i] }
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

  map(f) { if (this.val !== null) f(this.val) } 

  getElement(tag) {
    if (this.tag === tag) return this.val
    return null
  }
}



class DomClass {
  constructor(dom) {
    this.tag = null
    this.dom = dom
  }

  set(newTag) {
    if (this.tag === newTag) return
    if (this.tag !== null) { this.dom.classList.remove(this.tag) }
    this.tag = newTag
    if (this.tag !== null) { this.dom.classList.add(this.tag) }
  }

  get() { return this.tag }
}

class Toggle {
  constructor(dom, hidden) {
    this.domClass = new DomClass(dom)
    this.hidden = hidden
    this.domClass.set(hidden)
  }

  set(vis) { this.domClass.set(vis? null : this.hidden) }
  toggle() { this.set(!this.isVisible()) }
  isVisible() { return this.domClass.get() === null }
}
