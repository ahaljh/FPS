import chap03.Cons
import chap03.Nil
import chap03.List
import chap03.List._

// Example 3.1
// 질문: 도대체 import 를 어떻게 해야 아래 식을 평가할 수 있는가?
// import chap03.List._
// import chap03.{Cons, List}

val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + sum(t)
  case _ => 101
}

// Example 3.2
tail(List(1,2,3,4,5))

// Example 3.3
setHead(List(1,2,3,4,5), 6)

// Example 3.4
drop(List(1,2,3,4,5), 1)
drop(List(1,2,3,4,5), 2)
drop(List(1,2,3,4,5), 3)

// Example 3.5
dropWhile(List(1,2,3,4,5), (x:Int) => (x < 3))

// Example 3.6
init(List(1,2,3,4,5))

// Example 3.8
foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

// Example 3.9
length(List(1,2,3,4,5))

// Example 3.11
sum3(List(1,2,3,4,5))
product3(List(1,2,3,4,5))

// Example 3.12
reverse(List(1,2,3))

// Example 3.13
foldRight2(List(1,2,3), Nil:List[Int])(Cons(_,_))

// Example 3.14
append(List(1,2,3,4), 5)

// Example 3.15
appendList(List(1,2,3), List(4,5))

// Example 3.16
plusOne(List(1,2,3,4,5))

// Example 3.17
doubleToString(List(1.0, 2.1, 3.2, 4.3))

// Example 3.18
map(List(1,2,3,4,5))(x => x + 3)

// Example 3.19
filter(List(1,2,3,4,5))(x => x%2==0)

// Example 3.20
flatMap(List(1,2,3))(i => List(i,i))

// Example 3.21
filter2(List(1,2,3,4,5))(x => x%2==0)

// Example 3.22
plusList(List(1,2,3), List(4,5,6))

// Example 3.23
zipWith(List(1,2,3), List(4,5,6))(_ + _)

// Example 3.24
hasSubsequence(List(1,2,3,4), List(1,2))
hasSubsequence(List(1,2,3,4), List(2,3))
hasSubsequence(List(1,2,3,4), List(4))
hasSubsequence(List(1,2,3,4), List(1,5))