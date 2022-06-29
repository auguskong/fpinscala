import fpinscala.exercises.datastructures.List
import fpinscala.exercises.datastructures.List.{Cons, Nil}

def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
  as match
    case Nil => acc
    case Cons(x, xs) => f(x, foldRight(xs, acc, f))

def foldRight2[A,B](as: List[A], acc: B)(f: (A, B) => B): B = // Utility functions
  as match
    case Nil => acc
    case Cons(x, xs) => f(x, foldRight2(xs, acc)(f))


foldRight(List(1,2,3), Nil:List[Int], (Cons(_, _)))

def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
  l match
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l

def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] =
  l match
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l

val xs: List[Int] = List(1,2,3,4,5)
val ex1 = dropWhile(xs, (x: Int) => x < 4)
// group the arguments this way can assist with type inference
val ex2 = dropWhile2(xs)(x => x < 4)

def sum(ints: List[Int]): Int = ints match {
  case Nil => 0
  case Cons(x,xs) => x + sum(xs)
}

def product(doubles: List[Double]): Double = doubles match {
  case Nil => 1.0
  case Cons(x,xs) => x * product(xs)
}

def sum2(ns: List[Int]) = foldRight2(ns, 0)((x,y) => x + y)
def product2(ns: List[Double]) = foldRight2(ns, 1.0)(_ * _)

product2(List(1.0, 4.0, 5.0))