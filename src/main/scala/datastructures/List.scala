package datastructures

/**
  * Created by kunal on 31/7/17.
  */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](ds: List[A]): List[A] = ds match {
    case Cons(_,t) => t
    case _ => sys.error("ohhh")
  }

  def head[A](ds: List[A]): A = ds match {
    case Nil => sys.error("blah blah blah")
    case Cons(h,_) => h
  }
  def drop[A](l: List[A], n: Int): List[A] = {
    if(n == 0)
      l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h,t) => if(f(h)) dropWhile(t,f) else l
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def length[A](l : List[A]): Int = {
    foldRight(l,0)((l,len) => len+1)
  }

  def append[A](l : List[A],elem: List[A]): List[A] = {
    foldRight(l,elem)((x,y) => Cons(x,y))
  }

  def reverse[A](l : List[A]): List[A] = {
    foldRight(l,List[A]())((x,y) => List.append(y,List(x)))
  }

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h,t) => foldLeft(t,f(z,h))(f)
  }

  def flatten[A](l : List[List[A]]): List[A] = {
    foldLeft(l,Nil: List[A])((x,y) => List.append(x,y))
  }

  def addOne(l : List[Int]) : List[Int] = {
    foldRight(l,Nil: List[Int])((x,y) => Cons(x+1,y))
  }

  def convertToString[A](l : List[A]) : List[String] = {
    foldRight(l,Nil: List[String])((x,y) => Cons(x.toString,y))
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRight(as,Nil: List[B])((x,y) => Cons(f(x),y))
  }

  def mapViaFoldLeft[A,B](as: List[A])(f: A => B): List[B] = {
    foldLeft(as,Nil: List[B])((x,y) => Cons(f(y),x))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] ={
    foldRight(as,Nil: List[A])((x,y) => if(f(x)) Cons(x,y) else y )
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    flatten(map(as)(f))
  }

}

object Test {
  def main(args: Array[String]): Unit = {
    println(List.tail(List(1,2,3,4,5)))
    def f(x : Int): Boolean = x<4
    def test(y: Int): Double = y.toDouble
    def makeCopy[A](y : A): List[A] = Cons(y,Cons(y,Cons(y,Nil)))
    println("Drop While =>"+List.dropWhile(List(1,1,4,5,6),f))
    println(List.length(List(1,2,3,4,5,6)))
    println(List.append(List(1,3,4,5,6),List(7)))
    println(List.reverse(List(1,2,3,4,5,6)))
    println(List.flatten(List(List(1,2,3,4,5),List(1,2,3,4,5,6),List(2,3,4,5,6))))
    println(List.addOne(List(1,2,3,4,5)))
    println(List.convertToString(List(1,2,3,4,5)))
    println(List.map(List(1,2,3,4,5))(test))
    println(List.mapViaFoldLeft(List(1,2,3,4,5))(test))
    println(List.filter(List(1,2,3,4,5))(f))
    println(List.flatMap(List(1,2,3,4,5))(makeCopy))
  }
}