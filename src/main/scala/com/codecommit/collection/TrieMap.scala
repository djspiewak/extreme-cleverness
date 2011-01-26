package com.codecommit.collection

import scala.collection.{generic, immutable, mutable}
import generic.{CanBuildFrom, ImmutableMapFactory}
import immutable.{Map, MapLike}
import mutable.{Builder, ListBuffer}

import PatriciaTrie._

class TrieMap[K, +V] private (trie: Trie[List[(K, V)]])
    extends Map[K, V]
    with MapLike[K, V, TrieMap[K, V]] {
      
  override def empty: TrieMap[K, V] = new TrieMap(Empty)
  
  override def get(key: K) = for {
    bucket <- trie(key.hashCode)
    (_, value) <- bucket find { case (key2, _) => key == key2 }
  } yield value
  
  override def +[V2 >: V](pair: (K, V2)) = {
    val (key, value) = pair
    val hash = key.hashCode
    val bucket = trie(hash) map replaceInBucket(pair) getOrElse List(pair)
    new TrieMap(trie + (hash -> bucket))
  }
  
  override def -(key: K) = (empty /: iterator) { _ + _ }
  
  override def iterator = trie.toStream flatMap identity iterator
  
  private def replaceInBucket[V2 >: V](pair: (K, V2))(bucket: List[(K, V2)]): List[(K, V2)] = {
    val (key, value) = pair
    bucket match {
      case (`key`, _) :: tail => pair :: tail
      case (key2, value2) :: tail => (key2, value2) :: replaceInBucket(pair)(tail)
      case Nil => pair :: Nil
    }
  }
}

object TrieMap extends ImmutableMapFactory[TrieMap] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), TrieMap[A, B]] =
    new MapCanBuildFrom[A, B]
  
  def empty[A, B]: TrieMap[A, B] = new TrieMap(Empty)
}
