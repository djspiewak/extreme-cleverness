package com.codecommit.collection

import scala.collection.{generic, immutable, mutable, LinearSeqLike}
import generic.{CanBuildFrom, GenericCompanion, GenericTraversableTemplate, SeqFactory}
import immutable.LinearSeq
import mutable.{Builder, ListBuffer}

import FingerTree._

class FingerQueue[+A] private (deque: FingerTree[A])
    extends LinearSeq[A]
    with GenericTraversableTemplate[A, FingerQueue]
    with LinearSeqLike[A, FingerQueue[A]] {
  
  override val companion = FingerQueue
  
  override val isEmpty = deque.isEmpty
  
  override def head = deque.headLeft
  
  override def tail = new FingerQueue(deque.tailLeft)
  
  def +[B >: A](b: B) = enqueue(b)
  
  def enqueue[B >: A](b: B) = new FingerQueue(deque + b)
  
  def dequeue: (A, FingerQueue[A]) = (head, tail)
  
  def length = iterator.length
  
  def apply(i: Int) = iterator.toStream(i)
  
  override def iterator = deque.iterator
}

object FingerQueue extends SeqFactory[FingerQueue] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, FingerQueue[A]] = new GenericCanBuildFrom[A]
  
  def newBuilder[A]: Builder[A, FingerQueue[A]] =
    new ListBuffer[A] mapResult { x => new FingerQueue((x :\ (Empty: FingerTree[A])) { _ +: _ }) }
  
  override def empty[A]: FingerQueue[A] = new FingerQueue[A](Empty)
  
  override def apply[A](xs: A*): FingerQueue[A] = new FingerQueue((xs :\ (Empty: FingerTree[A])) { _ +: _ })
}
