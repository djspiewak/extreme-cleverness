/**
 Copyright (c) 2007-2008, Rich Hickey
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:

 * Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above
   copyright notice, this list of conditions and the following
   disclaimer in the documentation and/or other materials provided
   with the distribution.

 * Neither the name of Clojure nor the names of its contributors
   may be used to endorse or promote products derived from this
   software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 POSSIBILITY OF SUCH DAMAGE.
 **/

package com.codecommit.collection

import scala.collection.IndexedSeqLike
import scala.collection.generic._
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.{ArrayBuffer, Builder}

import VectorCases._

/**
 * A straight port of Clojure's <code>PersistentVector</code> class (with some
 * additional optimizations which may eventually land in Clojure's mainline).
 * For the record, this implementation is about 30% faster than
 * {@link scala.collection.immutable.Vector} on reads and about 5% slower for
 * "writes".
 *
 * @author Daniel Spiewak
 * @author Rich Hickey
 */
class Vector[+T] private[collection] (val length: Int, trie: Case, tail: Array[AnyRef])
    extends IndexedSeq[T]
    with GenericTraversableTemplate[T, Vector]
    with IndexedSeqLike[T, Vector[T]] { outer =>
      
  private val tailOff = length - tail.length
  
  override def companion = Vector
  
  /*
   * The design of this data structure inherantly requires heterogenous arrays.
   * It is *possible* to design around this, but the result is comparatively
   * quite inefficient.  With respect to this fact, I have left the original
   * (somewhat dynamically-typed) implementation in place.
   */
  
  private[collection] def this() = this(0, Zero, Vector.EmptyArray)
  
  def apply(i: Int): T = {
    if (i >= 0 && i < length) {
      if (i >= tailOff) {
        tail(i & 0x01f).asInstanceOf[T]
      } else {
        var arr = trie(i)
        arr(i & 0x01f).asInstanceOf[T]
      }
    } else throw new IndexOutOfBoundsException(i.toString)
  }
  
  def update[A >: T](i: Int, obj: A): Vector[A] = {
    if (i >= 0 && i < length) {
      if (i >= tailOff) {
        val newTail = new Array[AnyRef](tail.length)
        Array.copy(tail, 0, newTail, 0, tail.length)
        newTail(i & 0x01f) = obj.asInstanceOf[AnyRef]
        
        new Vector[A](length, trie, newTail)
      } else {
        new Vector[A](length, trie(i) = obj.asInstanceOf[AnyRef], tail)
      }
    } else if (i == length) {
      this + obj
    } else throw new IndexOutOfBoundsException(i.toString)
  }
  
  def +[A >: T](obj: A): Vector[A] = {
    if (tail.length < 32) {
      val tail2 = new Array[AnyRef](tail.length + 1)
      Array.copy(tail, 0, tail2, 0, tail.length)
      tail2(tail.length) = obj.asInstanceOf[AnyRef]
      
      new Vector[A](length + 1, trie, tail2)
    } else {
      new Vector[A](length + 1, trie + tail, Vector.array(obj.asInstanceOf[AnyRef]))
    }
  }
  
  /**
   * Removes the <i>tail</i> element of this vector.
   */
  def pop: Vector[T] = {
    if (length == 0) {
      throw new IllegalStateException("Can't pop empty vector")
    } else if (length == 1) {
      Vector.empty
    } else if (tail.length > 1) {
      val tail2 = new Array[AnyRef](tail.length - 1)
      Array.copy(tail, 0, tail2, 0, tail2.length)
      
      new Vector[T](length - 1, trie, tail2)
    } else {
      val (trie2, tail2) = trie.pop
      new Vector[T](length - 1, trie2, tail2)
    }
  }
}

final class VectorBuilder[A] extends Builder[A, Vector[A]] {      // TODO optimize
  private val buffer = new ArrayBuffer[A]
  
  val ZeroThresh = 0
  val OneThresh = 32
  val TwoThresh = 32 << 5
  val ThreeThresh = 32 << 10
  val FourThresh = 32 << 15
  val FiveThresh = 32 << 20
  val SixThresh = 32 << 25
  
  def +=(elem: A) = {
    buffer += elem
    this
  }
  
  def result = {
    import VectorCases._
    
    val tailLength = if (buffer.length % 32 == 0) 32 else buffer.length % 32
    val trieBuffer = buffer.view(0, buffer.length - tailLength)
    val tailBuffer = buffer.view(buffer.length - tailLength, buffer.length)
    
    val trie = if (trieBuffer.length <= ZeroThresh)
      Zero
    else if (trieBuffer.length <= OneThresh)
      One(fillArray1(trieBuffer))
    else if (trieBuffer.length <= TwoThresh)
      Two(fillArray2(trieBuffer))
    else if (trieBuffer.length <= ThreeThresh)
      Three(fillArray3(trieBuffer))
    else if (trieBuffer.length <= FourThresh)
      Four(fillArray4(trieBuffer))
    else if (trieBuffer.length <= FiveThresh)
      Five(fillArray5(trieBuffer))
    else if (trieBuffer.length <= SixThresh)
      Six(fillArray6(trieBuffer))
    else
      throw new IllegalStateException("Cannot build vector with length which exceeds MAX_INT")
    
    new Vector[A](buffer.length, trie, fillArray1(tailBuffer))
  }
  
  private def fillArray6(seq: Seq[_]) = {
    val CellSize = FiveThresh
    val length = if (seq.length % CellSize == 0) seq.length / CellSize else (seq.length / CellSize) + 1
    val back = new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](length)
    
    for (i <- 0 until back.length) {
      val buffer = seq.view(i * CellSize, Math.min((i + 1) * CellSize, seq.length))
      back(i) = fillArray5(buffer)
    }
    
    back
  }
  
  private def fillArray5(seq: Seq[_]) = {
    val CellSize = FourThresh
    val length = if (seq.length % CellSize == 0) seq.length / CellSize else (seq.length / CellSize) + 1
    val back = new Array[Array[Array[Array[Array[AnyRef]]]]](length)
    
    for (i <- 0 until back.length) {
      val buffer = seq.view(i * CellSize, Math.min((i + 1) * CellSize, seq.length))
      back(i) = fillArray4(buffer)
    }
    
    back
  }
  
  private def fillArray4(seq: Seq[_]) = {
    val CellSize = ThreeThresh
    val length = if (seq.length % CellSize == 0) seq.length / CellSize else (seq.length / CellSize) + 1
    val back = new Array[Array[Array[Array[AnyRef]]]](length)
    
    for (i <- 0 until back.length) {
      val buffer = seq.view(i * CellSize, Math.min((i + 1) * CellSize, seq.length))
      back(i) = fillArray3(buffer)
    }
    
    back
  }
  
  private def fillArray3(seq: Seq[_]) = {
    val CellSize = TwoThresh
    val length = if (seq.length % CellSize == 0) seq.length / CellSize else (seq.length / CellSize) + 1
    val back = new Array[Array[Array[AnyRef]]](length)
    
    for (i <- 0 until back.length) {
      val buffer = seq.view(i * CellSize, Math.min((i + 1) * CellSize, seq.length))
      back(i) = fillArray2(buffer)
    }
    
    back
  }
  
  private def fillArray2(seq: Seq[_]) = {
    val CellSize = OneThresh
    val length = if (seq.length % CellSize == 0) seq.length / CellSize else (seq.length / CellSize) + 1
    val back = new Array[Array[AnyRef]](length)
    
    for (i <- 0 until back.length) {
      val buffer = seq.view(i * CellSize, Math.min((i + 1) * CellSize, seq.length))
      back(i) = fillArray1(buffer)
    }
    
    back
  }
  
  private def fillArray1(seq: Seq[_]) = {
    val back = new Array[AnyRef](seq.length)
    
    for ((e, i) <- seq.zipWithIndex) {
      back(i) = e.asInstanceOf[AnyRef]
    }
    
    back
  }
  
  def clear() {
    buffer.clear()
  }
}

object Vector extends SeqFactory[Vector] {

  @inline
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Vector[A]] = new GenericCanBuildFrom[A] {
    override def apply = newBuilder[A]
  }
  
  def newBuilder[A] = new VectorBuilder[A]
  
  private[collection] val EmptyArray = new Array[AnyRef](0)
  
  private[this] val emptyVector = new Vector[Nothing]
  
  @inline
  override def empty[A]: Vector[A] = emptyVector
  
  @inline
  private[collection] def array(elem: AnyRef) = {
    val back = new Array[AnyRef](1)
    back(0) = elem
    back
  }
}

private[collection] object VectorCases {
  @inline
  private[this] def copy1(array1: Array[AnyRef], array2: Array[AnyRef]) = {
    Array.copy(array1, 0, array2, 0, Math.min(array1.length, array2.length))
    array2
  }
  
  @inline
  private[this] def copy2(array1: Array[Array[AnyRef]], array2: Array[Array[AnyRef]]) = {
    Array.copy(array1, 0, array2, 0, Math.min(array1.length, array2.length))
    array2
  }
  
  @inline
  private[this] def copy3(array1: Array[Array[Array[AnyRef]]], array2: Array[Array[Array[AnyRef]]]) = {
    Array.copy(array1, 0, array2, 0, Math.min(array1.length, array2.length))
    array2
  }
  
  @inline
  private[this] def copy4(array1: Array[Array[Array[Array[AnyRef]]]], array2: Array[Array[Array[Array[AnyRef]]]]) = {
    Array.copy(array1, 0, array2, 0, Math.min(array1.length, array2.length))
    array2
  }
  
  @inline
  private[this] def copy5(array1: Array[Array[Array[Array[Array[AnyRef]]]]], array2: Array[Array[Array[Array[Array[AnyRef]]]]]) = {
    Array.copy(array1, 0, array2, 0, Math.min(array1.length, array2.length))
    array2
  }
  
  @inline
  private[this] def copy6(array1: Array[Array[Array[Array[Array[Array[AnyRef]]]]]], array2: Array[Array[Array[Array[Array[Array[AnyRef]]]]]]) = {
    Array.copy(array1, 0, array2, 0, Math.min(array1.length, array2.length))
    array2
  }
  
  sealed trait Case {
    type Self <: Case
    
    val shift: Int
    
    def apply(i: Int): Array[AnyRef]
    def update(i: Int, obj: AnyRef): Self
    
    def +(node: Array[AnyRef]): Case
    def pop: (Case, Array[AnyRef])
  }
  
  case object Zero extends Case {
    type Self = Nothing
    
    val shift = -1
    
    def apply(i: Int) = throw new IndexOutOfBoundsException(i.toString)
    def update(i: Int, obj: AnyRef) = throw new IndexOutOfBoundsException(i.toString)
    
    def +(node: Array[AnyRef]) = One(node)
    def pop = throw new IndexOutOfBoundsException("Cannot pop an empty Vector")
  }
  
  case class One(trie: Array[AnyRef]) extends Case {
    type Self = One
    
    val shift = 0
    
    def apply(i: Int) = trie
    
    def update(i: Int, obj: AnyRef) = {
      val trie2 = copy1(trie, new Array[AnyRef](trie.length))
      trie2(i & 0x01f) = obj
      One(trie2)
    }
    
    def +(tail: Array[AnyRef]) = {
      val trie2 = new Array[Array[AnyRef]](2)
      trie2(0) = trie
      trie2(1) = tail
      Two(trie2)
    }
    
    def pop = (Zero, trie)
  }
  
  case class Two(trie: Array[Array[AnyRef]]) extends Case {
    type Self = Two
    
    val shift = 5
    
    def apply(i: Int) = trie((i >>> 5) & 0x01f)
    
    def update(i: Int, obj: AnyRef) = {
      val trie2a = copy2(trie, new Array[Array[AnyRef]](trie.length))
      
      val trie2b = {
        val target = trie2a((i >>> 5) & 0x01f)
        copy1(target, new Array[AnyRef](target.length))
      }
      trie2a((i >>> 5) & 0x01f) = trie2b
      
      trie2b(i & 0x01f) = obj
      Two(trie2a)
    }
    
    def +(tail: Array[AnyRef]) = {
      if (trie.length >= 32) {
        val trie2 = new Array[Array[Array[AnyRef]]](2)
        trie2(0) = trie
        
        trie2(1) = new Array[Array[AnyRef]](1)
        trie2(1)(0) = tail
        
        Three(trie2)
      } else {
        val trie2 = copy2(trie, new Array[Array[AnyRef]](trie.length + 1))
        trie2(trie.length) = tail
        Two(trie2)
      }
    }
    
    def pop = {
      if (trie.length == 2) {
        (One(trie(0)), trie.last)
      } else {
        val trie2 = copy2(trie, new Array[Array[AnyRef]](trie.length - 1))
        (Two(trie2), trie.last)
      }
    }
  }
  
  case class Three(trie: Array[Array[Array[AnyRef]]]) extends Case {
    type Self = Three
    
    val shift = 10
    
    def apply(i: Int) = {
      val a = trie((i >>> 10) & 0x01f)
      a((i >>> 5) & 0x01f)
    }
    
    def update(i: Int, obj: AnyRef) = {
      val trie2a = copy3(trie, new Array[Array[Array[AnyRef]]](trie.length))
      
      val trie2b = {
        val target = trie2a((i >>> 10) & 0x01f)
        copy2(target, new Array[Array[AnyRef]](target.length))
      }
      trie2a((i >>> 10) & 0x01f) = trie2b
      
      val trie2c = {
        val target = trie2b((i >>> 5) & 0x01f)
        copy1(target, new Array[AnyRef](target.length))
      }
      trie2b((i >>> 5) & 0x01f) = trie2c
      
      trie2c(i & 0x01f) = obj
      Three(trie2a)
    }
    
    def +(tail: Array[AnyRef]) = {
      if (trie.last.length >= 32) {
        if (trie.length >= 32) {
          val trie2 = new Array[Array[Array[Array[AnyRef]]]](2)
          trie2(0) = trie
          
          trie2(1) = new Array[Array[Array[AnyRef]]](1)
          trie2(1)(0) = new Array[Array[AnyRef]](1)
          trie2(1)(0)(0) = tail
          
          Four(trie2)
        } else {
          val trie2 = copy3(trie, new Array[Array[Array[AnyRef]]](trie.length + 1))
          trie2(trie.length) = new Array[Array[AnyRef]](1)
          trie2(trie.length)(0) = tail
          Three(trie2)
        }
      } else {
        val trie2 = copy3(trie, new Array[Array[Array[AnyRef]]](trie.length))
        trie2(trie2.length - 1) = copy2(trie2.last, new Array[Array[AnyRef]](trie2.last.length + 1))
        trie2.last(trie.last.length) = tail
        Three(trie2)
      }
    }
    
    def pop = {
      if (trie.last.length == 1) {
        if (trie.length == 2) {
          (Two(trie(0)), trie.last.last)
        } else {
          val trie2 = copy3(trie, new Array[Array[Array[AnyRef]]](trie.length - 1))
          (Three(trie2), trie.last.last)
        }
      } else {
        val trie2 = copy3(trie, new Array[Array[Array[AnyRef]]](trie.length))
        trie2(trie2.length - 1) = copy2(trie2.last, new Array[Array[AnyRef]](trie2.last.length - 1))
        (Three(trie2), trie.last.last)
      }
    }
  }
  
  case class Four(trie: Array[Array[Array[Array[AnyRef]]]]) extends Case {
    type Self = Four
    
    val shift = 15
    
    def apply(i: Int) = {
      val a = trie((i >>> 15) & 0x01f)
      val b = a((i >>> 10) & 0x01f)
      b((i >>> 5) & 0x01f)
    }
    
    def update(i: Int, obj: AnyRef) = {
      val trie2a = copy4(trie, new Array[Array[Array[Array[AnyRef]]]](trie.length))
      
      val trie2b = {
        val target = trie2a((i >>> 15) & 0x01f)
        copy3(target, new Array[Array[Array[AnyRef]]](target.length))
      }
      trie2a((i >>> 15) & 0x01f) = trie2b
      
      val trie2c = {
        val target = trie2b((i >>> 10) & 0x01f)
        copy2(target, new Array[Array[AnyRef]](target.length))
      }
      trie2b((i >>> 10) & 0x01f) = trie2c
      
      val trie2d = {
        val target = trie2c((i >>> 5) & 0x01f)
        copy1(target, new Array[AnyRef](target.length))
      }
      trie2c((i >>> 5) & 0x01f) = trie2d
      
      trie2d(i & 0x01f) = obj
      Four(trie2a)
    }
    
    def +(tail: Array[AnyRef]) = {
      if (trie.last.last.length >= 32) {
        if (trie.last.length >= 32) {
          if (trie.length >= 32) {
            val trie2 = new Array[Array[Array[Array[Array[AnyRef]]]]](2)
            trie2(0) = trie
            
            trie2(1) = new Array[Array[Array[Array[AnyRef]]]](1)
            trie2(1)(0) = new Array[Array[Array[AnyRef]]](1)
            trie2(1)(0)(0) = new Array[Array[AnyRef]](1)
            trie2(1)(0)(0)(0) = tail
            
            Five(trie2)
          } else {
            val trie2 = copy4(trie, new Array[Array[Array[Array[AnyRef]]]](trie.length + 1))
            trie2(trie.length) = new Array[Array[Array[AnyRef]]](1)
            trie2(trie.length)(0) = new Array[Array[AnyRef]](1)
            trie2(trie.length)(0)(0) = tail
            Four(trie2)
          }
        } else {
          val trie2 = copy4(trie, new Array[Array[Array[Array[AnyRef]]]](trie.length))
          trie2(trie2.length - 1) = copy3(trie2.last, new Array[Array[Array[AnyRef]]](trie2.last.length + 1))
          trie2.last(trie.last.length) = new Array[Array[AnyRef]](1)
          trie2.last.last(0) = tail
          Four(trie2)
        }
      } else {
        val trie2 = copy4(trie, new Array[Array[Array[Array[AnyRef]]]](trie.length))
        trie2(trie2.length - 1) = copy3(trie2.last, new Array[Array[Array[AnyRef]]](trie2.last.length))
        trie2.last(trie2.last.length - 1) = copy2(trie2.last.last, new Array[Array[AnyRef]](trie2.last.last.length + 1))
        trie2.last.last(trie.last.last.length) = tail
        Four(trie2)
      }
    }
    
    def pop = {
      if (trie.last.last.length == 1) {
        if (trie.last.length == 1) {
          if (trie.length == 2) {
            (Three(trie(0)), trie.last.last.last)
          } else {
            val trie2 = copy4(trie, new Array[Array[Array[Array[AnyRef]]]](trie.length - 1))
            (Four(trie2), trie.last.last.last)
          }
        } else {
          val trie2 = copy4(trie, new Array[Array[Array[Array[AnyRef]]]](trie.length))
          trie2(trie2.length - 1) = copy3(trie2.last, new Array[Array[Array[AnyRef]]](trie2.last.length - 1))
          (Four(trie2), trie.last.last.last)
        }
      } else {
        val trie2 = copy4(trie, new Array[Array[Array[Array[AnyRef]]]](trie.length))
        trie2(trie2.length - 1) = copy3(trie2.last, new Array[Array[Array[AnyRef]]](trie2.last.length - 1))
        trie2.last(trie2.last.length - 1) = copy2(trie2.last.last, new Array[Array[AnyRef]](trie2.last.last.length - 1))
        (Four(trie2), trie.last.last.last)
      }
    }
  }
  
  case class Five(trie: Array[Array[Array[Array[Array[AnyRef]]]]]) extends Case {
    type Self = Five
    
    val shift = 20
    
    def apply(i: Int) = {
      val a = trie((i >>> 20) & 0x01f)
      val b = a((i >>> 15) & 0x01f)
      val c = b((i >>> 10) & 0x01f)
      c((i >>> 5) & 0x01f)
    }
    
    def update(i: Int, obj: AnyRef) = {
      val trie2a = copy5(trie, new Array[Array[Array[Array[Array[AnyRef]]]]](trie.length))
      
      val trie2b = {
        val target = trie2a((i >>> 20) & 0x01f)
        copy4(target, new Array[Array[Array[Array[AnyRef]]]](target.length))
      }
      trie2a((i >>> 20) & 0x01f) = trie2b
      
      val trie2c = {
        val target = trie2b((i >>> 15) & 0x01f)
        copy3(target, new Array[Array[Array[AnyRef]]](target.length))
      }
      trie2b((i >>> 15) & 0x01f) = trie2c
      
      val trie2d = {
        val target = trie2c((i >>> 10) & 0x01f)
        copy2(target, new Array[Array[AnyRef]](target.length))
      }
      trie2c((i >>> 10) & 0x01f) = trie2d
      
      val trie2e = {
        val target = trie2d((i >>> 5) & 0x01f)
        copy1(target, new Array[AnyRef](target.length))
      }
      trie2d((i >>> 5) & 0x01f) = trie2e
      
      trie2e(i & 0x01f) = obj
      Five(trie2a)
    }
    
    def +(tail: Array[AnyRef]) = {
      if (trie.last.last.last.length >= 32) {
        if (trie.last.last.length >= 32) {
          if (trie.last.length >= 32) {
            if (trie.length >= 32) {
              val trie2 = new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](2)
              trie2(0) = trie
              
              trie2(1) = new Array[Array[Array[Array[Array[AnyRef]]]]](1)
              trie2(1)(0) = new Array[Array[Array[Array[AnyRef]]]](1)
              trie2(1)(0)(0) = new Array[Array[Array[AnyRef]]](1)
              trie2(1)(0)(0)(0) = new Array[Array[AnyRef]](1)
              trie2(1)(0)(0)(0)(0) = tail
              
              Six(trie2)
            } else {
              val trie2 = copy5(trie, new Array[Array[Array[Array[Array[AnyRef]]]]](trie.length + 1))
              trie2(trie.length) = new Array[Array[Array[Array[AnyRef]]]](1)
              trie2(trie.length)(0) = new Array[Array[Array[AnyRef]]](1)
              trie2(trie.length)(0)(0) = new Array[Array[AnyRef]](1)
              trie2(trie.length)(0)(0)(0) = tail
              Five(trie2)
            }
          } else {
            val trie2 = copy5(trie, new Array[Array[Array[Array[Array[AnyRef]]]]](trie.length))
            trie2(trie2.length - 1) = copy4(trie2.last, new Array[Array[Array[Array[AnyRef]]]](trie2.last.length + 1))
            trie2.last(trie.last.length) = new Array[Array[Array[AnyRef]]](1)
            trie2.last.last(0) = new Array[Array[AnyRef]](1)
            trie2.last.last.last(0) = tail
            Five(trie2)
          }
        } else {
          val trie2 = copy5(trie, new Array[Array[Array[Array[Array[AnyRef]]]]](trie.length))
          trie2(trie2.length - 1) = copy4(trie2.last, new Array[Array[Array[Array[AnyRef]]]](trie2.last.length))
          trie2.last(trie2.last.length - 1) = copy3(trie2.last.last, new Array[Array[Array[AnyRef]]](trie2.last.last.length + 1))
          trie2.last.last(trie.last.last.length) = new Array[Array[AnyRef]](1)
          trie2.last.last.last(0) = tail
          Five(trie2)
        }
      } else {
        val trie2 = copy5(trie, new Array[Array[Array[Array[Array[AnyRef]]]]](trie.length))
        trie2(trie2.length - 1) = copy4(trie2.last, new Array[Array[Array[Array[AnyRef]]]](trie2.last.length))
        trie2.last(trie2.last.length - 1) = copy3(trie2.last.last, new Array[Array[Array[AnyRef]]](trie2.last.last.length))
        trie2.last.last(trie2.last.last.length - 1) = copy2(trie2.last.last.last, new Array[Array[AnyRef]](trie2.last.last.last.length + 1))
        trie2.last.last.last(trie.last.last.last.length) = tail
        Five(trie2)
      }
    }
    
    def pop = {
      if (trie.last.last.last.length == 1) {
        if (trie.last.last.length == 1) {
          if (trie.last.length == 1) {
            if (trie.length == 2) {
              (Four(trie(0)), trie.last.last.last.last)
            } else {
              val trie2 = copy5(trie, new Array[Array[Array[Array[Array[AnyRef]]]]](trie.length - 1))
              (Five(trie2), trie.last.last.last.last)
            }
          } else {
            val trie2 = copy5(trie, new Array[Array[Array[Array[Array[AnyRef]]]]](trie.length))
            trie2(trie2.length - 1) = copy4(trie2.last, new Array[Array[Array[Array[AnyRef]]]](trie2.last.length - 1))
            (Five(trie2), trie.last.last.last.last)
          }
        } else {
          val trie2 = copy5(trie, new Array[Array[Array[Array[Array[AnyRef]]]]](trie.length))
          trie2(trie2.length - 1) = copy4(trie2.last, new Array[Array[Array[Array[AnyRef]]]](trie2.last.length - 1))
          trie2.last(trie2.last.length - 1) = copy3(trie2.last.last, new Array[Array[Array[AnyRef]]](trie2.last.last.length - 1))
          (Five(trie2), trie.last.last.last.last)
        }
      } else {
        val trie2 = copy5(trie, new Array[Array[Array[Array[Array[AnyRef]]]]](trie.length))
        trie2(trie2.length - 1) = copy4(trie2.last, new Array[Array[Array[Array[AnyRef]]]](trie2.last.length - 1))
        trie2.last(trie2.last.length - 1) = copy3(trie2.last.last, new Array[Array[Array[AnyRef]]](trie2.last.last.length - 1))
        trie2.last.last(trie2.last.last.length - 1) = copy2(trie2.last.last.last, new Array[Array[AnyRef]](trie2.last.last.last.length - 1))
        (Five(trie2), trie.last.last.last.last)
      }
    }
  }
  
  case class Six(trie: Array[Array[Array[Array[Array[Array[AnyRef]]]]]]) extends Case {
    type Self = Six
    
    val shift = 25
    
    def apply(i: Int) = {
      val a = trie((i >>> 25) & 0x01f)
      val b = a((i >>> 20) & 0x01f)
      val c = b((i >>> 15) & 0x01f)
      val d = c((i >>> 10) & 0x01f)
      d((i >>> 5) & 0x01f)
    }
    
    def update(i: Int, obj: AnyRef) = {
      val trie2a = copy6(trie, new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](trie.length))
      
      val trie2b = {
        val target = trie2a((i >>> 25) & 0x01f)
        copy5(target, new Array[Array[Array[Array[Array[AnyRef]]]]](target.length))
      }
      trie2a((i >>> 25) & 0x01f) = trie2b
      
      val trie2c = {
        val target = trie2b((i >>> 20) & 0x01f)
        copy4(target, new Array[Array[Array[Array[AnyRef]]]](target.length))
      }
      trie2b((i >>> 20) & 0x01f) = trie2c
      
      val trie2d = {
        val target = trie2c((i >>> 15) & 0x01f)
        copy3(target, new Array[Array[Array[AnyRef]]](target.length))
      }
      trie2c((i >>> 15) & 0x01f) = trie2d
      
      val trie2e = {
        val target = trie2d((i >>> 10) & 0x01f)
        copy2(target, new Array[Array[AnyRef]](target.length))
      }
      trie2d((i >>> 10) & 0x01f) = trie2e
      
      val trie2f = {
        val target = trie2e((i >>> 5) & 0x01f)
        copy1(target, new Array[AnyRef](target.length))
      }
      trie2e((i >>> 5) & 0x01f) = trie2f
      
      trie2f(i & 0x01f) = obj
      Six(trie2a)
    }
    
    def +(tail: Array[AnyRef]) = {
      if (trie.last.last.last.last.length >= 32) {
        if (trie.last.last.last.length >= 32) {
          if (trie.last.last.length >= 32) {
            if (trie.last.length >= 32) {
              if (trie.length >= 32) {
                throw new IndexOutOfBoundsException("Cannot grow vector beyond integer bounds")
              } else {
                val trie2 = copy6(trie, new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](trie.length + 1))
                trie2(trie.length) = new Array[Array[Array[Array[Array[AnyRef]]]]](1)
                trie2(trie.length)(0) = new Array[Array[Array[Array[AnyRef]]]](1)
                trie2(trie.length)(0)(0) = new Array[Array[Array[AnyRef]]](1)
                trie2(trie.length)(0)(0)(0) = new Array[Array[AnyRef]](1)
                trie2(trie.length)(0)(0)(0)(0) = tail
                Six(trie2)
              }
            } else {
              val trie2 = copy6(trie, new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](trie.length))
              trie2(trie2.length - 1) = copy5(trie2.last, new Array[Array[Array[Array[Array[AnyRef]]]]](trie2.last.length + 1))
              trie2.last(trie.last.length) = new Array[Array[Array[Array[AnyRef]]]](1)
              trie2.last.last(0) = new Array[Array[Array[AnyRef]]](1)
              trie2.last.last.last(0) = new Array[Array[AnyRef]](1)
              trie2.last.last.last.last(0) = tail
              Six(trie2)
            }
          } else {
            val trie2 = copy6(trie, new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](trie.length))
            trie2(trie2.length - 1) = copy5(trie2.last, new Array[Array[Array[Array[Array[AnyRef]]]]](trie2.last.length))
            trie2.last(trie2.last.length - 1) = copy4(trie2.last.last, new Array[Array[Array[Array[AnyRef]]]](trie2.last.last.length + 1))
            trie2.last.last(trie.last.last.length) = new Array[Array[Array[AnyRef]]](1)
            trie2.last.last.last(0) = new Array[Array[AnyRef]](1)
            trie2.last.last.last.last(0) = tail
            Six(trie2)
          }
        } else {
          val trie2 = copy6(trie, new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](trie.length))
          trie2(trie2.length - 1) = copy5(trie2.last, new Array[Array[Array[Array[Array[AnyRef]]]]](trie2.last.length))
          trie2.last(trie2.last.length - 1) = copy4(trie2.last.last, new Array[Array[Array[Array[AnyRef]]]](trie2.last.last.length))
          trie2.last.last(trie2.last.last.length - 1) = copy3(trie2.last.last.last, new Array[Array[Array[AnyRef]]](trie2.last.last.last.length + 1))
          trie2.last.last.last(trie.last.last.last.length) = new Array[Array[AnyRef]](1)
          trie2.last.last.last.last(0) = tail
          Six(trie2)
        }
      } else {
        val trie2 = copy6(trie, new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](trie.length))
        trie2(trie2.length - 1) = copy5(trie2.last, new Array[Array[Array[Array[Array[AnyRef]]]]](trie2.last.length))
        trie2.last(trie2.last.length - 1) = copy4(trie2.last.last, new Array[Array[Array[Array[AnyRef]]]](trie2.last.last.length))
        trie2.last.last(trie2.last.last.length - 1) = copy3(trie2.last.last.last, new Array[Array[Array[AnyRef]]](trie2.last.last.last.length))
        trie2.last.last.last(trie.last.last.last.length - 1) = copy2(trie2.last.last.last.last, new Array[Array[AnyRef]](trie2.last.last.last.last.length + 1))
        trie2.last.last.last.last(trie.last.last.last.last.length) = tail
        Six(trie2)
      }
    }
    
    def pop = {
      if (trie.last.last.last.last.length == 1) {
        if (trie.last.last.last.length == 1) {
          if (trie.last.last.length == 1) {
            if (trie.last.length == 1) {
              if (trie.length == 2) {
                (Five(trie(0)), trie.last.last.last.last.last)
              } else {
                val trie2 = copy6(trie, new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](trie.length - 1))
                (Six(trie2), trie.last.last.last.last.last)
              }
            } else {
              val trie2 = copy6(trie, new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](trie.length))
              trie2(trie2.length - 1) = copy5(trie2.last, new Array[Array[Array[Array[Array[AnyRef]]]]](trie2.last.length - 1))
              (Six(trie2), trie.last.last.last.last.last)
            }
          } else {
            val trie2 = copy6(trie, new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](trie.length))
            trie2(trie2.length - 1) = copy5(trie2.last, new Array[Array[Array[Array[Array[AnyRef]]]]](trie2.last.length - 1))
            trie2.last(trie2.last.length - 1) = copy4(trie2.last.last, new Array[Array[Array[Array[AnyRef]]]](trie2.last.last.length - 1))
            (Six(trie2), trie.last.last.last.last.last)
          }
        } else {
          val trie2 = copy6(trie, new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](trie.length))
          trie2(trie2.length - 1) = copy5(trie2.last, new Array[Array[Array[Array[Array[AnyRef]]]]](trie2.last.length - 1))
          trie2.last(trie2.last.length - 1) = copy4(trie2.last.last, new Array[Array[Array[Array[AnyRef]]]](trie2.last.last.length - 1))
          trie2.last.last(trie2.last.last.length - 1) = copy3(trie2.last.last.last, new Array[Array[Array[AnyRef]]](trie2.last.last.last.length - 1))
          (Six(trie2), trie.last.last.last.last.last)
        }
      } else {
        val trie2 = copy6(trie, new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](trie.length))
        trie2(trie2.length - 1) = copy5(trie2.last, new Array[Array[Array[Array[Array[AnyRef]]]]](trie2.last.length - 1))
        trie2.last(trie2.last.length - 1) = copy4(trie2.last.last, new Array[Array[Array[Array[AnyRef]]]](trie2.last.last.length - 1))
        trie2.last.last(trie2.last.last.length - 1) = copy3(trie2.last.last.last, new Array[Array[Array[AnyRef]]](trie2.last.last.last.length - 1))
        trie2.last.last.last(trie2.last.last.last.length - 1) = copy2(trie2.last.last.last.last, new Array[Array[AnyRef]](trie2.last.last.last.last.length - 1))
        (Six(trie2), trie.last.last.last.last.last)
      }
    }
  }
}
