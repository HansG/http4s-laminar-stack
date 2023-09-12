package example.backend

import scala.concurrent.duration.*
import cats.*
import cats.effect.*
import org.http4s.{EntityDecoder, HttpRoutes, StaticFile, multipart}
import org.http4s.circe.CirceEntityDecoder
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.dsl.Http4sDsl
import example.shared.Protocol.*
import fs2.io.file.{Files, Path => FsPath}

class Routes(
    service: Service,
    frontendJS: String
) extends Http4sDsl[IO]:
  def routes = HttpRoutes.of[IO] {
    case request @ POST -> Root / "get-suggestions" =>
      for
        req <- circeEntityDecoder[IO, GetSuggestions.Request]
          .decode(request, strict = true)
          .value
        result <- service.getSuggestions(
          req.getOrElse(throw new RuntimeException("what"))
        )
        // introduce a fake delay here to showcase the amazing
        // loader gif
        resp <- Ok(result) <* IO.sleep(50.millis)
      yield resp


    //https://app.gitter.im/#/room/#http4s_http4s:gitter.im
    case request @ POST -> Root / "upload" =>
      for
        req <- implicitly[EntityDecoder[IO, multipart.Multipart[IO]]]
          .decode(request, strict = true)
          .value
        result <-  req match {
          case Left(_) => BadRequest()
          case Right(mp) =>
            Traverse[Vector].traverse(mp.parts)   { part =>
              Files[IO].writeAll(FsPath( s"d://storage//${part.filename}"))(part.body)
                .compile.drain
            }.flatMap( Ok(_) )
        }
      yield result


    case request @ GET -> Root / "frontend" / "app.js" =>
      StaticFile
        .fromResource[IO](frontendJS, Some(request))
        .getOrElseF(NotFound())

    case request @ GET -> Root / "frontend" =>
      StaticFile
        .fromResource[IO]("index.html", Some(request))
        .getOrElseF(NotFound())

    case request @ GET -> Root / "assets" / path if staticFileAllowed(path) =>
      StaticFile
        .fromResource("/assets/" + path, Some(request))
        .getOrElseF(NotFound())
  }

  private def staticFileAllowed(path: String) =
    List(".gif", ".js", ".css", ".map", ".html", ".webm").exists(path.endsWith)
end Routes
