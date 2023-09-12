package trys

import cats.effect.{IO, IOApp}
import com.comcast.ip4s.{Host, Port}
import org.http4s.*
import org.http4s.MediaType.*
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.headers.*
import org.http4s.multipart.{Multipart, Part}


//https://stackoverflow.com/questions/47368919/processing-multipart-content-in-http4s
object HelloWorldServer extends IOApp with Http4sDsl[IO] {
  val service = HttpRoutes.of[IO] {

    case GET -> Root =>
      Ok(
        """|<html>
           |<body>
           |<form method="post" action="/post" enctype="multipart/form-data">
           | <input type="date" name="birthDate" placeholder="birthDate">
           | <input type="file" name="dataFile">
           | <input type="submit">
           |</form>
        """.stripMargin).
        withContentType(Some(`Content-Type`(`text/html`)))

    case req @ POST -> Root / "post" => {
      println(s"POST request: $req")
      req.decode[Multipart[IO]] { m =>
        Ok(
          s"""Multipart Data\nParts:${m.parts.length}
             |${m.parts.map { case f: Part[IO] => { f.name + ", headers: " + f.headers.mkString(",")} }.mkString("\n")}""".stripMargin)
      }
    }

    case req@POST -> Root / "post" => {
      req.decode[Multipart[IO]] { m => {
        m.parts.find(_.name == Some("dataFile")) match {
          case None => BadRequest(s"Not file")
          case Some(part) => for {
            contents <- part.body.through(utf8Decode).runFoldMonoid
            response <- Ok(
              s"""Multipart Data\nParts:${m.parts.length}
                 |File contents: ${contents}""".stripMargin)
          } yield response
        }
      }
      }
    }
  }

  }

  def stream(args: List[String], requestShutdown: IO[Unit]) =
    EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("0.0.0.0").get)
      .withHttpApp(service.orNotFound)
      .build

}