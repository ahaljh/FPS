package chap03

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(value) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value: Int) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => depth(left)+1 max depth(right)+1
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A,B](t: Tree[A], g: A => B)(f: (B,B) => B): B =
    t match {
      case Leaf(value) => g(value)
      case Branch(left, right) => f(fold(left, g)(f), fold(right, g)(f))
    }

  def size2[A](t: Tree[A]): Int =
    fold(t, (x:A) => 1)((x,y) => x+y+1)

  def maximum2(t: Tree[Int]): Int =
    fold(t, (x:Int) => x)((x,y) => x max y)

  def depth2[A](t: Tree[A]): Int =
    fold(t, (x:A) => 1)((x,y) => (x+1) max (y+1))

  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t, (x:A) => Leaf(f(x)): Tree[B])((x,y) => Branch(x, y): Tree[B])
}
