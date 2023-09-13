package trys

import cats.effect.{ExitCode, IO, IOApp}
import com.comcast.ip4s.{Host, Port}
import org.http4s.*
import org.http4s.MediaType.*
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.headers.*
import org.http4s.multipart.{Multipart, Part}
import fs2.text


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
        """.stripMargin)
        .map(resp => resp.withContentType(Some(`Content-Type`(MediaType.text))))

 /*   case req@POST -> Root / "post" =>
      println(s"POST request: $req")
      req.decode[Multipart[IO]] { m =>
        Ok(
          s"""Multipart Data\nParts:${m.parts.length}
             |${
            m.parts.map { case f: Part[IO] => {
              f.name + ", headers: " + f.headers.mkString(",")
            }
            }.mkString("\n")
          }""".stripMargin)
      }*/

    case req@POST -> Root / "post" =>
      req.decode[Multipart[IO]] { m =>
        val dataParts = m.parts.filter(_.name == Some("dataFile"))
        val fileContents = dataParts.map { part  =>
          for {
            contents <- part.body.through(text.utf8.decode).through(text.lines)
            resp <-
              s"""File contents:
                 |${contents}""".stripMargin
          } yield  part.name +  part.headers.mkString("\nheaders: ", ", ", "\n") + resp
        }
        Ok(fileContents.mkString(s"""Multipart File-Data of ${dataParts.length}Parts:))""", "\nNext File", "End of Multipart Data"))
      }



  def resource(args: List[String]) =//, requestShutdown: IO[Unit]) =
    EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("0.0.0.0").get)
      .withHttpApp(service.orNotFound)
      .build


  def run(args: List[String]): IO[ExitCode] =
        resource(args)
          .use(_ => status *> IO.never)
          .as(ExitCode.Success)


}