
function uiGet(id) {
  const d = document.getElementById(id)
  if (d === null) { throw new Error("Missing id: " + id) }
  return d
}

function uiFromTemplate(id) {
  const dom = uiGet(id).cloneNode(true)
  dom.removeAttribute("id")
  return dom
}

function uiFromTemplateNested(id) {
  const dom = uiFromTemplate(id)
  const els = {}
  function search(it) {
    for (const el of it.children) {
      const a = el.getAttribute("id")
      if (a !== null) {
        els[a] = el
        el.removeAttribute("id")
      }
      if (el instanceof HTMLElement) { search(el) }
    }
  }
  search(dom)
  return [ dom, els ]
}

