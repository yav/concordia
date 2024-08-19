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
  }
}
