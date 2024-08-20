class Tagged {
  constructor(mk) {
    this.mk = mk
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
      const v = this.mk[tag]()
      v.set(v.contents)
      
    }
  }
}
