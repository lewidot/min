type handler = Request.t => promise<Response.t>
type middleware = handler => handler

let pathSegments = (req: Request.t) => {
  req
  ->Request.url
  ->URL.make
  ->URL.pathname
  ->String.split("/")
  ->Array.filter(s => s != "")
}

let text = (body, ~status=200, ~headers=?) => {
  let options: Response.responseInit = switch headers {
  | Some(h) => {
      status,
      headers: HeadersInit.FromArray(h),
    }
  | None => {status: status}
  }

  Promise.resolve(Response.make(body, ~options))
}

let status = (status, ~headers=?) => {
  text("", ~status, ~headers?)
}

let json = (data, ~status=200, ~headers=?) => {
  let options: Response.responseInitWithHeaders = switch headers {
  | Some(h) => {
      status,
      headers: Headers.make(~init=HeadersInit.FromArray(h)),
    }
  | None => {status: status}
  }

  Promise.resolve(Response.makeWithJson(data, ~options))
}

let notFound = () => status(404)
let methodNotAllowed = () => status(405)

let get = (req, handler) => {
  switch req->Request.method {
  | GET | HEAD => handler(req)
  | _ => methodNotAllowed()
  }
}

let post = (req, handler) => {
  switch req->Request.method {
  | POST => handler(req)
  | _ => methodNotAllowed()
  }
}

let put = (req, handler) => {
  switch req->Request.method {
  | PUT => handler(req)
  | _ => methodNotAllowed()
  }
}

let patch = (req, handler) => {
  switch req->Request.method {
  | PATCH => handler(req)
  | _ => methodNotAllowed()
  }
}

let delete = (req, handler) => {
  switch req->Request.method {
  | DELETE => handler(req)
  | _ => methodNotAllowed()
  }
}

let serve = (~port=3000, handler: handler) => {
  let server = Bun.serve({
    port,
    fetch: (request, _server) => {
      handler(request)
    },
  })

  let hostname = server->Bun.Server.hostname
  let portStr = server->Bun.Server.port->Int.toString
  Console.log(`server listening on http://${hostname}:${portStr}`)

  server
}

let methodToString = (method: method) => {
  switch method {
  | GET => "GET"
  | POST => "POST"
  | PUT => "PUT"
  | PATCH => "PATCH"
  | DELETE => "DELETE"
  | HEAD => "HEAD"
  | CONNECT => "CONNECT"
  | OPTIONS => "OPTIONS"
  | TRACE => "TRACE"
  }
}

let logger = handler => {
  req => {
    // Log the request
    let start = Date.now()
    let method = req->Request.method->methodToString
    let path = req->Request.url->URL.make->URL.pathname

    // Call the handler and log the response
    handler(req)->Promise.thenResolve(response => {
      let duration = Date.now() - start
      let status = response->Response.status->Int.toString
      Console.log(`${method} ${path} ${status} ${Float.toString(duration)}ms`)
      response
    })
  }
}

let html = (body, ~status=200, ~headers=?) => {
  let contentHeader = ("content-type", "text/html; charset=utf-8")

  let options: Response.responseInit = {
    status,
    headers: HeadersInit.FromArray(
      switch headers {
      | Some(h) => [contentHeader, ...h]
      | None => [contentHeader]
      },
    ),
  }

  Promise.resolve(Response.make(body, ~options))
}
