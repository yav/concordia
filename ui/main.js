let conn = null
let gui = null
const quest = new Question()

function main() {
  conn = srvConnect()
}

async function uiRedraw(s) {
  if (gui === null) gui = new GUI()
  quest.reset()
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
