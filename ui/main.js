let conn = null
let gui = null

function main() {
  gui = new GUI()
  conn = srvConnect()
}

async function uiRedraw(s) {
  gui.quest.reset()
  await gui.set(s.game)
  uiQuestions(s.questions)
}

function uiSetQuestion(q) {
  gui.setQuestion(q)
}

function uiQuestion(q) {
  gui.ask(q)
}

function uiUpdate(s) {
  gui.set(s)
}
