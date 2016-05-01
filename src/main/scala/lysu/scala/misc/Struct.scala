package scala.lysu.scala.misc

sealed trait List2[+A]

case object Nil2 extends List2[Nothing]

case class Cons[+A](head: A, tail: List2[A]) extends List2[A]

object List2 {

  def sum(ints: List2[Int]): Int =
    foldLeft(ints, 0)(_ + _)

  def product(ds: List2[Double]): Double =
    foldLeft(ds, 0.0)(_ * _)

  def len1[A](l: List2[A]): Int =
    foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List2[A]): List2[A] =
    foldLeft(l, List2[A]())((acc, h) => Cons(h, acc))

  def tail[A](ls: List2[A]): List2[A] = ls match {
    case Nil2 => Nil2
    case Cons(_, xs) => xs
  }

  def drop[A](ls: List2[A], n: Int): List2[A] = {
    if (n <= 0) ls
    else ls match {
      case Nil2 => Nil2
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  def dropWhile[A](l: List2[A], f: A => Boolean): List2[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }

  def append[A](as: List2[A], bs: List2[A]): List2[A] =
    foldLeft(bs, as)((as: List2[A], b: A) => Cons(b, as))

  def setHead[A](ls: List2[A], a: A): List2[A] = ls match {
    case Nil2 => Nil2
    case Cons(_, xs) => Cons(a, xs)
  }

  def init[A](l: List2[A]): List2[A] = {
    l match {
      case Nil2 => Nil2
      case Cons(_, Nil2) => Nil2
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def foldRight[A, B](as: List2[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil2 => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def foldLeft[A, B](as: List2[A], z: B)(f: (B, A) => B): B = as match {
    case Nil2 => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def concat[A](l: List2[List2[A]]): List2[A] =
    foldRight(l, Nil2: List2[A])(append)

  def map[A,B](as: List2[A])(f: A => B): List2[B] =
    foldRight(as, Nil2: List2[B])((x, y) => Cons(f(x), y))

  def plus1(ints: List2[Int]): List2[Int] =
    map(ints)((x) => x + 1)

  def toStrings(ds: List2[Double]): List2[String] =
    map(ds)(_.toString)

  def filter[A](as: List2[A])(f: A => Boolean): List2[A] =
    foldRight(as, Nil2: List2[A])((a, acc) => if (f(a)) Cons(a, acc) else acc)

  def flatMap[A,B](as: List2[A])(f: A => List2[B]): List2[B] =
    concat(map(as)(f))

  def filter2[A](l: List2[A])(f: A=> Boolean): List2[A] =
    flatMap(l)((a) => if(f(a)) List2(a) else Nil2)

  def zipWith[A,B,C](a: List2[A], b: List2[B])(f: (A, B) => C): List2[C] = (a, b) match {
    case (Nil2, _) => Nil2
    case (_, Nil2) => Nil2
    case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1, x2), zipWith(a, b)(f))
  }

  def apply[A](as: A*): List2[A] =
    if (as.isEmpty) Nil2
    else Cons(as.head, apply(as.tail: _*))

  def hasSub[A](sup: List2[A], sub: List2[A]): Boolean = sup match {
    case Nil2 => sub == Nil2
    case _ if startWith(sup, sub) => true
    case Cons(x, xs) => hasSub(xs, sub)
  }

  def startWith[A](l: List2[A], prefix: List2[A]): Boolean = (l, prefix) match {
    case (_, Nil2) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startWith(t, t2)
    case _ => false
  }

}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

object Struct {

  def main(args: Array[String]) {

    val x = List2(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil2 => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List2.sum(t)
      case _ => 101
    }
    println(x)

  }

}
