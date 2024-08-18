let conn = null

function main() {
  conn = srvConnect()
}

function uiRedraw(s) {
  console.log("uiRedraw",s)
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
