package scala.lysu.scala.misc

/**
  * Created by suli on 4/29/16.
  */
object Coffee {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(n: Int) = {
    val msg = "The abs of %d is %d"
    msg.format(n, abs(n))
  }

  def fac(n: Int): Int = {

    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  def fib(n: Int): Int = {

    @annotation.tailrec
    def go(n: Int, acc1: Int, acc2: Int): Int =
      if (n < 2) acc1
      else go(n - 1, acc2, acc1 + acc2)

    go(n, 1, 1)

  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {

    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)

  }

  def isSorted[A](as: Array[A], p: (A, A) => Boolean): Boolean = {

    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n <= 1) true
      else if (!p(as(n - 1), as(n))) false
      else loop(n - 1)
    }

    loop(as.length - 1)

  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def main(args: Array[String]) {
    val x = formatAbs(-100)
    println(x)
    println(fac(3))
    println(fib(4))
    println(isSorted(Array(1, 2, 3), (x: Int, y: Int) => x > y))
    println(isSorted(Array(1, 2, 3), (x: Int, y: Int) => x < y))

    val ab = (a: Int, b: Int) => a + b
    val a = curry(ab)
    val c = a(1)(2)
    println(c)
    println(compose((x: Int) => 1 + x, (y: Int) => 2 + y)(8))
  }

}

