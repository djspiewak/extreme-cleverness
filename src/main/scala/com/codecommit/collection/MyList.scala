package com.codecommit.collection

import scala.collection.{generic, immutable, mutable, LinearSeqLike}
import generic.{CanBuildFrom, GenericCompanion, GenericTraversableTemplate, SeqFactory}
import immutable.LinearSeq
import mutable.{Builder, ListBuffer}

object MyList {
  import scala.{List => _}        // just to be extra explicit about shadowing
  
  sealed trait List[+A] extends LinearSeq[A]
      with GenericTraversableTemplate[A, List]
      with LinearSeqLike[A, List[A]] {
    
    override val companion = List
    
    def ::[B >: A](b: B): List[B] = new ::(b, this)
  }
  
  object List extends SeqFactory[List] {
    implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, List[A]] = new GenericCanBuildFrom[A]
    
    def newBuilder[A]: Builder[A, List[A]] =
      new ListBuffer[A] mapResult { x => (x :\ (Nil: List[A])) { _ :: _ } }
    
    override def empty[A]: List[A] = Nil
  }
  
  
  case class ::[+A](override val head: A, override val tail: List[A]) extends List[A] {
    override val isEmpty = false
    
    override lazy val length = 1 + tail.length
    
    def apply(i: Int) = {
      if (i < 0)
        throw new IndexOutOfBoundsException(i.toString)
      else if (i == 0)
        head
      else
        tail(i - 1)
    }
  }
  
  case object Nil extends List[Nothing] {
    override val isEmpty = true
    
    override val length = 0
    
    def apply(i: Int) = throw new IndexOutOfBoundsException(i.toString)
  }
}
