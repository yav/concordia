let conn = null

function main() {
  conn = srvConnect()
}

function uiRedraw(s) {
  console.log("uiRedraw",s)
  for (const c of s.game.hand) {
    const ca = new Card(c)
    document.body.appendChild(ca.getDom())
  }

  for (const c of s.game.market) {
    const ca = new Card(c)
    document.body.appendChild(ca.getDom())
  }
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
