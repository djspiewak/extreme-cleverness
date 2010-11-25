package com.codecommit.collection

object RedBlack {
  def makeBlack[K : Ordering, V](tree: Tree[K, V]) = tree match {
    case Node(false, left, key, value, right) => Node(true, left, key, value, right)
    case t => t
  }
  
  def balance[K : Ordering, V](isBlack: Boolean, left: Tree[K, V], key: K, value: V, right: Tree[K, V]) = {
    (isBlack, left, key, value, right) match {
      case (true, Node(false, Node(false, a, xk, xv, b), yk, yv, c), zk, zv, d) =>
        Node(false, Node(true, a, xk, xv, b), yk, yv, Node(true, c, zk, zv, d))
      
      case (true, Node(false, a, xk, xv, Node(false, b, yk, yv, c)), zk, zv, d) =>
        Node(false, Node(true, a, xk, xv, b), yk, yv, Node(true, c, zk, zv, d))
      
      case (true, a, xk, xv, Node(false, Node(false, b, yk, yv, c), zk, zv, d)) =>
        Node(false, Node(true, a, xk, xv, b), yk, yv, Node(true, c, zk, zv, d))
      
      case (true, a, xk, xv, Node(false, b, yk, yv, Node(false, c, zk, zv, d))) =>
        Node(false, Node(true, a, xk, xv, b), yk, yv, Node(true, c, zk, zv, d))
      
      case (isBlack, a, xk, xv, b) => Node(isBlack, a, xk, xv, b)
    }
  }
  
  sealed abstract class Tree[K : Ordering, +V] {
    val isBlack: Boolean
    
    def left: Tree[K, V]
    
    def key: K
    def value: V
    
    def right: Tree[K, V]
    
    def apply(key: K): Option[V]
    
    def updated[A >: V](key: K, value: A): Tree[K, A]
    
    def -(key: K): Tree[K, V]
    
    def removeMin: (Tree[K, V], K, V)
    
    def toStream: Stream[(K, V)]
  }
  
  case class Node[K : Ordering, +V](isBlack: Boolean, left: Tree[K, V], key: K, value: V, right: Tree[K, V]) extends Tree[K, V] {
    import Ordered._
    
    def apply(key: K) = {
      if (this.key > key)
        left(key)
      else if (this.key == key)
        Some(value)
      else       // (this.key < key)
        right(key)
    }
    
    def updated[A >: V](key: K, value: A) = {
      if (this.key > key)
        balance(isBlack, left.updated(key, value), this.key, this.value, right)
      else if (this.key == key)
        Node(isBlack, left, key, value, right)
      else       //  (this.key < key)
        balance(isBlack, left, this.key, this.value, right.updated(key, value))
    }
    
    def -(key: K) = {
      if (this.key > key) {
        balance(isBlack, left - key, this.key, this.value, right)
      } else if (this.key == key) {
        (left, right) match {
          case (Node(_, _, _, _, _), Node(_, _, _, _, _)) => {
            val (right2, key2, value2) = right.removeMin
            balance(isBlack, left, key2, value2, right2)
          }
          
          case (Leaf(), Node(_, _, _, _, _)) => right
          
          case (Node(_, _, _, _, _), Leaf()) => left
          
          case (Leaf(), Leaf()) => Leaf()
        }
      } else {
        balance(isBlack, left, this.key, this.value, right - key)
      }
    }
  
    def removeMin = left match {
      case Node(_, _, _, _, _) => {
        val (left2, key2, value2) = left.removeMin
        (balance(isBlack, left2, key, value, right), key2, value2)
      }
      
      case Leaf() => (right, key, value)
    }
    
    def toStream = left.toStream ++ ((key, value) #:: right.toStream) 
  }
  
  case class Leaf[K : Ordering]() extends Tree[K, Nothing] {
    val isBlack = true
    
    def left = error("left of an empty tree")
    
    def key = error("key of an empty tree")
    def value = error("value of an empty tree")
    
    def right = error("right of an empty tree")
    
    def apply(key: K) = None
    
    def updated[V](key: K, value: V) = Node(false, Leaf(), key, value, Leaf())
    
    def -(key: K) = this
    
    def removeMin = error("cannot removeMin of an empty tree")
    
    val toStream = Stream()
  }
}
