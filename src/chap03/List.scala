package chap03

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  def setHead[A](l: List[A], newHead: A) = l match {
    case Nil => Cons(newHead, Nil)
    case Cons(h, t) => Cons(newHead, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(h, t) => drop(t, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if (f(h) == true) => dropWhile(t, f)
    case _ => l  // l이 Nil일 경우 포함
  }

//  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] =
//    as match {
//      case Cons(h, t) if f(h) => dropWhile(t)(f)
//      case _ => as
//    }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(h, t) if (t != Nil) => Cons(h, init(t))
    case _ => Nil
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x+y)

  def product2(ds: List[Double]) =
    foldRight(ds, 1.0)(_ * _)

  // 함수에서 (x,y) => y+1 이 나은가? 아니면 (_,acc) => acc+1 이 나은가? 후자가 낫겠지?
  def length[A](as: List[A]) =
    foldRight(as, 0)((x,y) => y+1)

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ds: List[Double]) =
    foldLeft(ds, 1.0)(_ * _)

  // foldLeft의 두번째 함수..여기서 파라미터를 x,y로 주는가.. 뭔가 분간 잘 되게 줄 방법은?(일단은 type을 명시하긴 했다.)
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil:List[A])((x: List[A], y: A) => Cons(y, x))

  // 이것이 맞는가..물론 제대로 동작하긴 한다... reverse후 foldLeft
  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(foldLeft(as, Nil:List[A])((x: List[A], y: A) => Cons(y, x)), z)((b: B, a: A) => f(a, b))

  def append[A](as: List[A], a: A): List[A] =
    foldRight(as, Cons(a, Nil))(Cons(_, _))

  def appendList[A](as1: List[A], as2: List[A]): List[A] =
    foldRight(as1, as2)(Cons(_, _))

  def plusOne(ns: List[Int]): List[Int] = ns match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x+1, plusOne(xs))
  }

  def doubleToString(ds: List[Double]): List[String] = ds match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, doubleToString(xs))
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
    case Cons(x, xs) => filter(xs)(f)
  }

  // foldRight가 적절할까? append가 적절할까? 성능상 foldRight가 나을꺼 같긴 한데, 이해가 바로 될까?
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    //case Cons(x, xs) => appendList(f(x), flatMap(xs)(f))
    case Cons(x, xs) => foldRight(f(x), flatMap(xs)(f))(Cons(_, _))
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) Cons(x,Nil) else Nil)

  // 이렇게 case문 중첩시 indentation은 어떻게 하는가?
  def plusList(ns1: List[Int], ns2: List[Int]): List[Int] = {
    ns1 match {
      case Nil => ns2
      case Cons(x1, xs1) => ns2 match {
        case Nil => ns1
        case Cons(x2, xs2) => Cons((x1 + x2), plusList(xs1, xs2))
      }
    }
  }

  // def zipWith[A,B](as1: List[A], as2: List[A])(f: (A, A) => B): List[B]  이게 맞지 않을까?
  def zipWith[A](as1: List[A], as2: List[A])(f: (A, A) => A): List[A] = {
    as1 match {
      case Nil => as2
      case Cons(x1, xs1) => as2 match {
        case Nil => as1
        case Cons(x2, xs2) => Cons(f(x1,x2), zipWith(xs1, xs2)(f))
      }
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def hasItem(item: A, l: List[A]): Boolean = l match {
      case Nil => false
      case Cons(x, xs) if (x == item) => true
      case Cons(x, xs) => hasItem(item, xs)
    }

    def process(subList: List[A]): Boolean = subList match {
      case Nil => true  // 여기서 이렇게 처리를 해도 맞는가... 처음부터 Nil일 경우..
      case Cons(x, xs) if (hasItem(x, sup) == false) => false
      case Cons(x, xs) => process(xs)
    }

    process(sub)
  }

}
