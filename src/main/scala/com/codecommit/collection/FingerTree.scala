package com.codecommit.collection

object FingerTree {
  sealed trait FingerTree[+A] {
    val isEmpty: Boolean
    
    def headLeft: A
    def tailLeft: FingerTree[A]
    
    def headRight: A
    def tailRight: FingerTree[A]
    
    def +:[B >: A](b: B): FingerTree[B]
    def +[B >: A](b: B): FingerTree[B]
    
    def viewLeft: FTViewLeft[FingerTree, A]
    def viewRight: FTViewRight[FingerTree, A]
    
    def iterator: Iterator[A]
  }
  
  case class Single[+A](a: A) extends FingerTree[A] {
    val headLeft = a
    val tailLeft = Empty
    
    val headRight = a
    val tailRight = Empty
    
    val isEmpty = false
    
    def +:[B >: A](b: B) = Deep(One(b), Empty, One(a))
    
    def +[B >: A](b: B) = Deep(One(a), Empty, One(b))
    
    def viewLeft = FTConsLeft[FingerTree, A](a, Empty)
    def viewRight = FTConsRight[FingerTree, A](Empty, a)
    
    def iterator = new Iterator[A] {
      var hasNext = true
      
      def next = {
        hasNext = false
        a
      }
    }
    
    override def toString = "FingerTree(Single(%s))".format(a)
  }
  
  case class Deep[+A](prefix: Digit[A], tree: FingerTree[Node[A]], suffix: Digit[A]) extends FingerTree[A] {
    val isEmpty = false
    
    val headLeft = prefix.headLeft
    val headRight = suffix.headRight
    
    def tailLeft = viewLeft.tail
    def tailRight = viewRight.tail
    
    def +:[B >: A](b: B) = prefix match {
      case Four(d, e, f, g) => Deep(Two(b, d), Node3(d, e, f) +: tree, suffix)
      case partial => Deep(b :: partial, tree, suffix)
    }
    
    def +[B >: A](b: B) = suffix match {
      case Four(g, f, e, d) => Deep(prefix, tree + Node3(g, f, e), Two(d, b))
      case partial => Deep(prefix, tree, partial + b)
    }
    
    def viewLeft = {
      def deep(prefix: Digit[A], tree: FingerTree[Node[A]], suffix: Digit[A]) = prefix match {
        case One(_) => {
          tree.viewLeft match {
            case FTConsLeft(a, newTree) => Deep(a.toDigit, newTree, suffix)
            case FTNilLeft() => suffix.toTree
          }
        }
        
        case prefix => Deep(prefix.tailLeft, tree, suffix)
      }
      
      FTConsLeft(prefix.headLeft, deep(prefix, tree, suffix))
    }
    
    def viewRight = {
      def deep(prefix: Digit[A], tree: FingerTree[Node[A]], suffix: Digit[A]) = suffix match {
        case One(_) => {
          tree.viewRight match {
            case FTConsRight(newTree, a) => Deep(prefix, newTree, a.toDigit)
            case FTNilRight() => prefix.toTree
          }
        }
        
        case suffix => Deep(prefix, tree, suffix.tailRight)
      }
      
      FTConsRight(deep(prefix, tree, suffix.tailRight), suffix.headRight)
    }
    
    def iterator = prefix.iterator ++ (tree.iterator flatMap { _.toList.iterator }) ++ suffix.iterator
    
    override def toString = "FingerTree(%s, %s, %s)".format(prefix, tree, suffix)
  }
  
  case object Empty extends FingerTree[Nothing] {
    val isEmpty = true
    
    def headLeft = throw new NoSuchElementException("headLeft on empty finger tree")
    def tailLeft = throw new NoSuchElementException("tailLeft on empty finger tree")
    
    def headRight = throw new NoSuchElementException("headRight on empty finger tree")
    def tailRight = throw new NoSuchElementException("tailRight on empty finger tree")
    
    def +:[A](a: A) = Single(a)
    
    def +[A](a: A) = Single(a)
    
    def viewLeft = FTNilLeft[FingerTree]()
    def viewRight = FTNilRight[FingerTree]()
    
    def iterator = new Iterator[Nothing] {
      val hasNext = false
      
      def next = throw new NoSuchElementException
    }
    
    override def toString = "FingerTree(Empty)"
  }
  
 
  sealed trait Node[+A] {
    def toDigit: Digit[A]
    def toList: List[A]
  }
  
  case class Node2[+A](a1: A, a2: A) extends Node[A] {
    def toDigit = Two(a1, a2)
    
    def toList = List(a1, a2)
    
    override def toString = "Node2(%s, %s)".format(a1, a2)
  }
  
  case class Node3[+A](a1: A, a2: A, a3: A) extends Node[A] {
    def toDigit = Three(a1, a2, a3)
    
    def toList = List(a1, a2, a3)
    
    override def toString = "Node3(%s, %s, %s)".format(a1, a2, a3)
  }
  
  
  sealed trait FTViewLeft[+S[+_], +A] {
    def head: A
    def tail: S[A]
  }
  
  case class FTConsLeft[+S[+_], +A](head: A, tail: S[A]) extends FTViewLeft[S, A]
  
  case class FTNilLeft[+S[+_]]() extends FTViewLeft[S, Nothing] {
    def head = throw new NoSuchElementException("head on empty view")
    def tail = throw new NoSuchElementException("tail on empty view")
  }
  
  
  sealed trait FTViewRight[+S[+_], +A] {
    def tail: S[A]
    def head: A
  }
  
  case class FTConsRight[+S[+_], +A](tail: S[A], head: A) extends FTViewRight[S, A]
  
  case class FTNilRight[+S[+_]]() extends FTViewRight[S, Nothing] {
    def tail = throw new NoSuchElementException("tail on empty view")
    def head = throw new NoSuchElementException("head on empty view")
  }
  
  
  sealed trait Digit[+A] {
    val headLeft: A
    def tailLeft: Digit[A]
    
    val headRight: A
    def tailRight: Digit[A]
    
    def ::[B >: A](b: B): Digit[B]
    def +[B >: A](b: B): Digit[B]
    
    def toTree: FingerTree[A]
    
    def iterator: Iterator[A]
  }
  
  case class One[+A](a1: A) extends Digit[A] {
    val headLeft = a1
    def tailLeft = throw new NoSuchElementException("tail on digit: one")
    
    val headRight = a1
    def tailRight = throw new NoSuchElementException("tail on digit: one")
    
    def ::[B >: A](b: B) = Two(b, a1)
    def +[B >: A](b: B) = Two(a1, b)
    
    def toTree = Single(a1)
    
    def iterator = new Iterator[A] {
      var hasNext = true
      
      def next = {
        hasNext = false
        a1
      }
    }
  }
  
  case class Two[+A](a1: A, a2: A) extends Digit[A] {
    val headLeft = a1
    def tailLeft = One(a2)
    
    val headRight = a2
    def tailRight = One(a1)
    
    def ::[B >: A](b: B) = Three(b, a1, a2)
    def +[B >: A](b: B) = Three(a1, a2, b)
    
    def toTree = a1 +: Single(a2)
    
    def iterator = (a1 :: a2 :: Nil).iterator
  }
  
  case class Three[+A](a1: A, a2: A, a3: A) extends Digit[A] {
    val headLeft = a1
    def tailLeft = Two(a2, a3)
    
    val headRight = a3
    def tailRight = Two(a1, a2)
    
    def ::[B >: A](b: B) = Four(b, a1, a2, a3)
    def +[B >: A](b: B) = Four(a1, a2, a3, b)
    
    def toTree = a1 +: a2 +: Single(a3)
    
    def iterator = (a1 :: a2 :: a3 :: Nil).iterator
  }
  
  case class Four[+A](a1: A, a2: A, a3: A, a4: A) extends Digit[A] {
    val headLeft = a1
    def tailLeft = Three(a2, a3, a4)
    
    val headRight = a4
    def tailRight = Three(a1, a2, a3)
    
    def ::[B >: A](b: B) = throw new UnsupportedOperationException(":: on Four")
    def +[B >: A](b: B) = throw new UnsupportedOperationException("+ on Four")
    
    def toTree = a1 +: a2 +: a3 +: Single(a4)
    
    def iterator = (a1 :: a2 :: a3 :: a4 :: Nil).iterator
  }
}
