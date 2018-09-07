import java.io.{File, PrintWriter}

import scala.io.Source
import scala.util.{Failure, Try}

object Stuff {
  val delimiter = "\t"
}

sealed trait Direction
case object Buy extends Direction
case object Sell extends Direction
object Direction {
  def from(str: String): Option[Direction] = str match {
    case "b" => Option(Buy)
    case "s" => Option(Sell)
    case _ =>
      println(s"wrong direction $str")
      None
  }
}

sealed trait Active
case object A extends Active
case object B extends Active
case object C extends Active
case object D extends Active
object Active {
  def from(str: String): Option[Active] = str match {
    case "A" => Option(A)
    case "B" => Option(B)
    case "C" => Option(C)
    case "D" => Option(D)
    case _ =>
      println(s"wrong active $str")
      None
  }
}

case class Data(active: Active, price: Int, amount: Int)

case class Order(client: String, t: Direction, data: Data)

object Order {
  def from(str: String): Option[Order] =
    str.split(Stuff.delimiter).toSeq match {
      case Seq(c, t, g, p, a) =>
        Direction
          .from(t)
          .flatMap(d => Active.from(g).map(d -> _))
          .flatMap {
            case (d, aa) =>
              Try(Order(c, d, Data(aa, p.toInt, a.toInt))).recoverWith {
                case e =>
                  println(s"wrong format $str $e")
                  Failure(e)
              }.toOption
          }
      case _ =>
        println(s"wrong format $str")
        None
    }
}

case class Client(name: String, fiat: Int, aсtives: Map[Active, Int]) {
  def to: String =
    (Seq(name, fiat.toString) ++ Seq(A, B, C, D).map(aсtives(_).toString))
      .mkString(Stuff.delimiter)
}

object Client {
  def from(str: String): Option[Client] =
    str.split(Stuff.delimiter).toSeq match {
      case Seq(n, f, a, b, c, d) =>
        Try(
          Client(
            n,
            f.toInt,
            Map(A -> a.toInt, B -> b.toInt, C -> c.toInt, D -> d.toInt))).recoverWith {
          case e =>
            println(s"wrong format $str $e")
            Failure(e)
        }.toOption
      case _ =>
        println(s"wrong format $str")
        None
    }
}

object Matching extends App {

  def findFirstAndRemove[T](s: List[T],
                            p: T => Boolean): (List[T], Option[T]) = {
    val i = s.indexWhere(p)
    if (i < 0) s -> None
    else s.patch(i, Nil, 1) -> s.lift(i)
  }

  val clients = Source
    .fromFile("clients.txt")
    .getLines
    .flatMap(Client.from)
    .toList
    .map(c => c.name -> c)
    .toMap

  val orders = Source.fromFile("orders.txt").getLines.flatMap(Order.from).toList
  val (buys, sells) = orders.partition(_.t == Buy)

  val updatedClients = buys
    .foldLeft(clients -> sells) { (accu, b) =>
      accu match {
        case (clientsAccu, sellAccu) =>
          val (sellAccu1, sell) = findFirstAndRemove[Order](
            sellAccu,
            s => s.client != b.client && s.data == b.data)
          val clients1 = sell match {
            case Some(s) =>
              val sellClient = clientsAccu(s.client)
              val buyClient = clientsAccu(b.client)
              val data = s.data
              val a = data.active
              clientsAccu
                .updated(s.client,
                         sellClient.copy(
                           fiat = sellClient.fiat + data.amount * data.price,
                           aсtives = sellClient.aсtives
                             .updated(a, sellClient.aсtives(a) - data.amount)))
                .updated(b.client,
                         buyClient.copy(
                           fiat = buyClient.fiat - data.amount * data.price,
                           aсtives = buyClient.aсtives
                             .updated(a, buyClient.aсtives(a) + s.data.amount)))
            case _ => clientsAccu
          }
          clients1 -> sellAccu1
      }
    }
    ._1
    .values
    .toList
    .sortBy(_.name)

  val pw = new PrintWriter(new File("result.txt"))
  Try(updatedClients.foreach(c => pw.println(c.to)))
    .recover {
      case e =>
        println(s"Error writing to file $e")
    }
  pw.close
}
