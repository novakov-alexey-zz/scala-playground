import scala.annotation.tailrec

trait List1[+T]

case object Nil1 extends List1[Nothing]

case class Cons[+T](head: T, tail: List1[T]) extends List1[T]

object tests {
  def reverse[T](input: List[T]): List[T] = {
    @tailrec
    def go(l: List[T], acc: List[T]): List[T] = {
      l match {
        case Nil => acc
        case h :: t => go(t, h +: acc)
      }
    }

    go(input, List.empty[T])
  }
}


object Main extends App {

  import tests._

  val l = List("h", "e", "l", "l", "o")
  println(reverse(l))
  println(reverse(List(1, 2, 3, 4)))

  val anotherList = Cons(1, Cons(2, Cons(3, Nil1)))
}