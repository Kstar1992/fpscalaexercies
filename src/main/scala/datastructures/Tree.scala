package datastructures

/**
  * Created by kunal on 1/8/17.
  */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => size(l)+size(r)+1
  }


  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(x,y) => maximum(x).max(maximum(y))
  }

  def maxDepth[A](t: Tree[A],dep : Int):Int = t match {
    case Leaf(_) => dep+1
    case Branch(x,y) => maxDepth(x,dep+1).max(maxDepth(y,dep+1))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(x,y) => Branch(map(x)(f),map(y)(f))
  }
}

object Output {
  def main(args: Array[String]): Unit = {
    def tes(x: Int): String = {
      if(x < 5)
        "here"
      else
        "there"
    }
    val t = Branch(Branch(Branch(Branch(Leaf(7),Branch(Leaf(1),Leaf(0))),Leaf(6)),Branch(Leaf(3),Leaf(2))),Branch(Branch(Leaf(9),Leaf(10)),Branch(Leaf(44),Leaf(33))))
    println(Tree.size(t))
    println(Tree.maximum(t))
    println(Tree.maxDepth(t,0))
    println(Tree.map(t)(tes))

  }
}
