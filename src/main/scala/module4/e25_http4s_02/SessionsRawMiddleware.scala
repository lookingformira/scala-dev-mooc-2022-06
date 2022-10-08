package module4.e25_http4s_02

import cats.data.{Kleisli, OptionT}
import cats.effect._
import com.comcast.ip4s.{Host, Port}
import org.http4s.HttpRoutes
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.server.{HttpMiddleware, Router}
import org.typelevel.ci.CIStringSyntax
import cats.implicits._
object SessionsRawMiddleware {

  type Sessions[F[_]] = Ref[F, List[String]]

  val helloService: HttpRoutes[IO] =
    HttpRoutes.of { case r @ GET -> Root / "hello" =>
      r.headers.get(ci"X-User-Name") match {
        case Some(nel) =>
          val name = nel.head.value
          Ok(s"Hello, $name")
        case None => Forbidden("No header")
      }
    }

  def loginService(sessions: Sessions[IO]): HttpRoutes[IO] = HttpRoutes.of {
    case PUT -> Root / "register" / name =>
      sessions
        .update(list => name :: list)
        .flatMap(_ => Ok(s"Welcome, $name!"))
  }

  def router(sessions: Sessions[IO]) =
    AddResponseHeader.addResponseHeaderMiddleware(
      Router(
        "/" -> (loginService(sessions) <+> sessionAuth(sessions)(helloService))
      )
    )

  def sessionAuth(sessions: Sessions[IO]): HttpMiddleware[IO] =
    routes =>
      Kleisli { req =>
        req.headers.get(ci"X-User-Name") match {
          case Some(nel) =>
            val name = nel.head.value
            for {
              users <- OptionT.liftF(sessions.get)
              result <-
                if (users.contains(name)) routes(req)
                else OptionT.liftF(Forbidden("You shall not pass"))
            } yield result
          case None => OptionT.liftF(Forbidden("No header"))
        }
      }

  val server = for {
    sessions <- Resource.eval(Ref.of[IO, List[String]](List.empty))
    s <- EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(router(sessions).orNotFound)
      .build
  } yield s

  // curl localhost:8080/hello
  // curl -H "X-User-Name: vladimir" localhost:8080/hello
  // curl -XPUT localhost:8080/register/vladimir
  // curl -H "X-User-Name: vladimir" localhost:8080/hello
}
