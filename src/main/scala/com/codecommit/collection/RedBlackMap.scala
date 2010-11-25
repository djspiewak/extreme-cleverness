package com.codecommit.collection

import scala.collection.{generic, immutable, mutable, SortedMapLike}
import generic.{CanBuildFrom, ImmutableSortedMapFactory}
import immutable.{Map, SortedMap, MapLike}
import mutable.{Builder, ListBuffer}

import RedBlack._

class RedBlackMap[K : Ordering, +V] private (tree: Tree[K, V])
    extends SortedMap[K, V]
    with SortedMapLike[K, V, RedBlackMap[K, V]]
    with MapLike[K, V, RedBlackMap[K, V]] {
      
  override protected[this] def newBuilder: Builder[(K, V), RedBlackMap[K, V]] =
    RedBlackMap.newBuilder[K, V]
  
  val ordering = implicitly[Ordering[K]]
  
  def get(key: K) = tree(key)
  
  override def updated[A >: V](key: K, value: A) =
    new RedBlackMap(makeBlack(tree.updated(key, value)))
  
  override def +[A >: V](pair: (K, A)) = 
    new RedBlackMap(makeBlack(tree.updated(pair._1, pair._2)))
  
  def -(key: K) = new RedBlackMap(makeBlack(tree - key))
  
  // well, it is *lazy* functional programming...
  override def rangeImpl(from: Option[K], to: Option[K]) = {
    import Ordered._
    
    val base = toStream
    val headed = from map { k => base dropWhile { _._1 < k } } getOrElse base
    val tailed = to map { k => headed takeWhile { _._1 < k } } getOrElse headed
    
    RedBlackMap(tailed: _*)
  }
  
  def iterator = toStream.iterator
  
  override def toStream = tree.toStream
  
  override def empty = new RedBlackMap[K, V](Leaf())
}

object RedBlackMap extends ImmutableSortedMapFactory[RedBlackMap] {
  implicit def canBuildFrom[K : Ordering, V]: CanBuildFrom[Coll, (K, V), RedBlackMap[K, V]] =
    new SortedMapCanBuildFrom[K, V]
  
  def empty[K, V](implicit ord: Ordering[K]) = new RedBlackMap(Leaf())
}
