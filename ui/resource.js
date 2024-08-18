
function resourceIcon(ty) {
  const dom = document.createElement("div")
  dom.classList.add("resource-icon")
  dom.classList.add(ty.toLowerCase())
  dom.setAttribute("title", ty)
  return dom
}
