let getUser = userId =>
  (_req: Request.t) => {
    `User: ${userId}`->Min.text(~status=200)
  }

let handler = req => {
  switch Min.pathSegments(req) {
  | [] => Min.text("Hello from Min! 🌱", ~status=304, ~headers=[("my-header", "test")])
  | ["json"] => Min.json(JSON.parseOrThrow(`{"foo":"bar","hello":"world"}`))
  | ["user", userId] => Min.get(req, getUser(userId))
  | ["status"] => Min.status(401)
  | _ => Min.notFound()
  }
}

let _server = Min.serve(~port=3000, handler)
