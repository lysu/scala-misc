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
      else go(n-1, n*acc)

    go(n, 1)
  }

  def fb(n: Int): Int = {

    @annotation.tailrec
    def go(n: Int, acc1: Int, acc2: Int): Int =
      if (n < 2) acc1
      else go(n-1, acc2, acc1 + acc2)

    go(n, 1, 1)

  }


  def main(args: Array[String]) {
    val x = formatAbs(-100)
    println(x)
    println(fac(3))
    println(fb(4))
  }

}

