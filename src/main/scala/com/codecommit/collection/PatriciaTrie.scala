package com.codecommit.collection

object PatriciaTrie {
  def join[A](prefix1: Int, first: Trie[A], prefix2: Int, second: Trie[A]) = {
    val mask = branchingBit(prefix1, prefix2)
    
    if (zeroBit(prefix1, mask))
      Branch(offsetMask(prefix1, mask), mask, first, second)
    else
      Branch(offsetMask(prefix1, mask), mask, second, first)
  }
  
  def branchingBit(prefix1: Int, prefix2: Int) = lowestBit(prefix1 ^ prefix2)
  
  def lowestBit(x: Int) = x & -x
  
  def zeroBit(key: Int, mask: Int) = (key & mask) == 0
  
  def matchPrefix(key: Int, prefix: Int, mask: Int) = offsetMask(key, mask) == prefix
  
  def offsetMask(prefix: Int, mask: Int) = prefix & (mask - 1)
  
  
  sealed trait Trie[+A] {
    def +[B >: A](pair: (Int, B)): Trie[B]
    
    def ++[B >: A](that: Trie[B]): Trie[B]
    
    def apply(key: Int): Option[A]
    
    def toStream: Stream[A]
  }
  
  case class Leaf[+A](key: Int, a: A) extends Trie[A] {
    def +[B >: A](pair: (Int, B)) = {
      val (key, value) = pair
      
      if (this.key == key)
        Leaf(key, value)
      else
        join(key, Leaf(key, value), this.key, this)
    }
    
    def ++[B >: A](that: Trie[B]) =
      if (that(key).isDefined) that else that + (key -> a)
    
    def apply(key: Int) = if (this.key == key) Some(a) else None
  
    def toStream = Stream(a) 
  }
  
  case class Branch[+A](prefix: Int, mask: Int, left: Trie[A], right: Trie[A]) extends Trie[A] {
    def +[B >: A](pair: (Int, B)) = {
      val (key, value) = pair
      
      if (matchPrefix(key, prefix, mask)) {
        if (zeroBit(key, mask))
          Branch(prefix, mask, left + (key, value), right)
        else
          Branch(prefix, mask, left, right + (key, value))
      } else {
        join(key, Leaf(key, value), prefix, this)
      }
    }
    
    def ++[B >: A](that: Trie[B]) = that match {
      case Leaf(key, value) => this + (key -> value)
      
      case Branch(prefix2, mask2, left2, right2) => {
        if (prefix == prefix2 && mask == mask2) {
          Branch(prefix, mask, left ++ left2, right ++ right2)
        } else if (mask < mask2 && matchPrefix(prefix2, prefix, mask)) {
          if (zeroBit(prefix2, mask))
            Branch(prefix, mask, left ++ that, right)
          else
            Branch(prefix, mask, left, right ++ that)
        } else if (mask > mask2 && matchPrefix(prefix, prefix2, mask)) {
          if (zeroBit(prefix, mask))
            Branch(prefix2, mask2, this ++ left, right2)
          else
            Branch(prefix2, mask2, left, this ++ right2)
        } else {
          join(prefix, this, prefix2, that)
        }
      }
        
      case Empty => this
    }
    
    def apply(key: Int) = {
      if (zeroBit(key, mask))
        left(key)
      else
        right(key)
    }
    
    def toStream = left.toStream append right.toStream
  }
  
  case object Empty extends Trie[Nothing] {
    def +[A](pair: (Int, A)) = Leaf(pair._1, pair._2)
    
    def ++[A](that: Trie[A]) = that
    
    def apply(key: Int) = None
    
    def toStream = Stream()
  }
}
