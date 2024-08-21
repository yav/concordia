let conn = null
let gui = null

function main() {
  conn = srvConnect()
}

async function uiRedraw(s) {
  if (gui !== null) gui.destroy()
  gui = new GUI()
  await gui.set(s.game)
}

function uiSetQuestion(q) {
  console.log("uiSetQuestion",q)
}

function uiQuestion(q) {
  console.log("uiQuestion",q)
}

function uiUpdate(x) {
  console.log("uiUpdate",x)
}
