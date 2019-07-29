/*
Copyright (c) 2017, Qvantel
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
 * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
 * Neither the name of the Qvantel nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL Qvantel BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.qvantel.jsonapi

import _root_.spray.json._

object JsonOption {
  def apply[A](x: A): JsonOption[A] = if (x == null) JsonNull else JsonSome(x)
  def apply[A](x: Option[A]): JsonOption[A] = x match {
    case Some(y) => JsonSome(y)
    case None    => JsonAbsent
  }
  def empty[A]: JsonOption[A] = JsonAbsent

  private[jsonapi] type JF[T] = JsonFormat[T] // simple alias for reduced verbosity

  implicit def jsonOptionFormat[T: JF]: JF[JsonOption[T]] = new JsonOptionFormat[T]

  class JsonOptionFormat[T: JF] extends JF[JsonOption[T]] {
    def write(option: JsonOption[T]) = option match {
      case JsonSome(x) => x.toJson
      case JsonAbsent  => JsNull
      case JsonNull    => JsNull
    }
    def read(value: JsValue) = value match {
      case JsNull => JsonNull
      case x      => JsonSome(x.convertTo[T])
    }
    // allows reading the JSON as a Some (useful in container formats)
    def readSome(value: JsValue): JsonSome[T] = JsonSome(value.convertTo[T])
  }
}

/** Represents optional value with a special case for null.
  * Used for parsing data out of json where null and value
  * not being present at all are different things (for example
  * in the case of wanting to unset a value)
  *
  * @note For the most part tries to implement scala options api.
  *       Also provides .toOption to turn the maybe into a scala Option
  * @define none [[JsonAbsent]]
  * @define some [[JsonSome]]
  * @define option [[JsonOption]]
  * @define p `p`
  * @define f `f`
  */
sealed abstract class JsonOption[+A] { self =>

  /** Helper function to .orElse JsonSome. Result is None for JsonNull
    * but for JsonAbsent x param is returned
    */
  def someOrElse[B >: A](x: Option[B]): Option[B] = this match {
    case JsonSome(v) => Some(v)
    case JsonNull    => None
    case JsonAbsent  => x
  }

  /** Turns this JsonOption into scala Option
    */
  def toOption: Option[A]

  /** Returns true if maybe is nothing or null
    */
  def isEmpty: Boolean

  /** Returns true if maybe is nothing
    */
  def isAbsent: Boolean

  /** Returns true if maybe is instance of just
    */
  def isDefined: Boolean = !isEmpty

  /** Returns the maybe's value.
    *
    * @note The maybe must be nonEmpty
    * @throws java.util.NoSuchElementException if the maybe is empty
    */
  def get: A

  /** Returns the maybe's value if the maybe is nonempty, otherwise
    * return the result of evaluating `default`.
    *
    *  @param default  the default expression.
    */
  @inline final def getOrElse[B >: A](default: => B): B =
    if (isEmpty) default else this.get

  /** Returns the option's value if it is nonempty,
    * or `null` if it is empty.
    * Although the use of null is discouraged, code written to use
    * $option must often interface with code that expects and returns nulls.
    *
    * @example {{{
    * val initialText: Option[String] = getInitialText
    * val textField = new JComponent(initialText.orNull,20)
    * }}}
    */
  @inline final def orNull[A1 >: A](implicit ev: Null <:< A1): A1 = this getOrElse ev(null)

  /** Returns a $some containing the result of applying $f to this $option's
    * value if this $option is nonempty.
    * Otherwise return $none.
    *
    *  @note This is similar to `flatMap` except here,
    *  $f does not need to wrap its result in an $option.
    *  @param  f   the function to apply
    *  @see flatMap
    *  @see foreach
    */
  @inline final def map[B](f: A => B): JsonOption[B] =
    this match {
      case JsonSome(x) => JsonSome(f(x))
      case JsonAbsent  => JsonAbsent
      case JsonNull    => JsonNull
    }

  /** Returns the result of applying $f to this $option's
    *  value if the $option is nonempty.  Otherwise, evaluates
    *  expression `ifEmpty`.
    *
    *  @note This is equivalent to `$option map f getOrElse ifEmpty`.
    *  @param  ifEmpty the expression to evaluate if empty.
    *  @param  f       the function to apply if nonempty.
    */
  @inline final def fold[B](ifEmpty: => B)(f: A => B): B =
    this match {
      case JsonSome(x) => f(x)
      case JsonAbsent  => ifEmpty
      case JsonNull    => ifEmpty
    }

  /** Returns the result of applying $f to this $option's value if
    * this $option is nonempty.
    * Returns $none if this $option is empty.
    * Slightly different from `map` in that $f is expected to
    * return an $option (which could be $none).
    *
    *  @param  f   the function to apply
    *  @see map
    *  @see foreach
    */
  @inline final def flatMap[B](f: A => JsonOption[B]): JsonOption[B] =
    this match {
      case JsonSome(x) => f(x)
      case JsonAbsent  => JsonAbsent
      case JsonNull    => JsonNull
    }

  def flatten[B](implicit ev: A <:< JsonOption[B]): JsonOption[B] =
    this match {
      case JsonSome(x) => ev(x)
      case JsonAbsent  => JsonAbsent
      case JsonNull    => JsonNull
    }

  /** Returns this $option if it is nonempty '''and''' applying the predicate $p to
    * this $option's value returns true. Otherwise, return $none.
    *
    *  @param  p   the predicate used for testing.
    */
  @inline final def filter(p: A => Boolean): JsonOption[A] =
    this match {
      case JsonSome(x) if p(x)      => this
      case JsonAbsent | JsonSome(_) => JsonAbsent
      case JsonNull                 => JsonNull
    }

  /** Returns this $option if it is nonempty '''and''' applying the predicate $p to
    * this $option's value returns false. Otherwise, return $none.
    *
    *  @param  p   the predicate used for testing.
    */
  @inline final def filterNot(p: A => Boolean): JsonOption[A] =
    this match {
      case JsonSome(x) if !p(x)     => this
      case JsonAbsent | JsonSome(_) => JsonAbsent
      case JsonNull                 => JsonNull
    }

  /** Necessary to keep $option from being implicitly converted to
    *  [[scala.collection.Iterable]] in `for` comprehensions.
    */
  @inline final def withFilter(p: A => Boolean): WithFilter = new WithFilter(p)

  /** We need a whole WithFilter class to honor the "doesn't create a new
    *  collection" contract even though it seems unlikely to matter much in a
    *  collection with max size 1.
    */
  class WithFilter(p: A => Boolean) {
    def map[B](f: A => B): JsonOption[B]                 = self filter p map f
    def flatMap[B](f: A => JsonOption[B]): JsonOption[B] = self filter p flatMap f
    def foreach[U](f: A => U): Unit                      = self filter p foreach f

    def withFilter(q: A => Boolean): WithFilter = new WithFilter(x => p(x) && q(x))
  }

  /** Returns false if the option is $none, true otherwise.
    *
    *  @note   Implemented here to avoid the implicit conversion to Iterable.
    */
  final def nonEmpty = isDefined

  /** Tests whether the option contains a given value as an element.
    *
    *  @example {{{
    *  // Returns true because Some instance contains string "something" which equals "something".
    *  Some("something") contains "something"
    *
    *  // Returns false because "something" != "anything".
    *  Some("something") contains "anything"
    *
    *  // Returns false when method called on None.
    *  None contains "anything"
    *  }}}
    *  @param elem the element to test.
    *  @return `true` if the option has an element that is equal (as
    *  determined by `==`) to `elem`, `false` otherwise.
    */
  final def contains[A1 >: A](elem: A1): Boolean =
    !isEmpty && this.get == elem

  /** Returns true if this option is nonempty '''and''' the predicate
    * $p returns true when applied to this $option's value.
    * Otherwise, returns false.
    *
    *  @param  p   the predicate to test
    */
  @inline final def exists(p: A => Boolean): Boolean =
    !isEmpty && p(this.get)

  /** Returns true if this option is empty '''or''' the predicate
    * $p returns true when applied to this $option's value.
    *
    *  @param  p   the predicate to test
    */
  @inline final def forall(p: A => Boolean): Boolean = isEmpty || p(this.get)

  /** Apply the given procedure $f to the option's value,
    *  if it is nonempty. Otherwise, do nothing.
    *
    *  @param  f   the procedure to apply.
    *  @see map
    *  @see flatMap
    */
  @inline final def foreach[U](f: A => U): Unit = {
    if (!isEmpty) f(this.get)
    ()
  }

  /** Returns a $some containing the result of
    * applying `pf` to this $option's contained
    * value, '''if''' this option is
    * nonempty '''and''' `pf` is defined for that value.
    * Returns $none otherwise.
    *
    *  @example {{{
    *  // Returns Some(HTTP) because the partial function covers the case.
    *  Some("http") collect {case "http" => "HTTP"}
    *
    *  // Returns None because the partial function doesn't cover the case.
    *  Some("ftp") collect {case "http" => "HTTP"}
    *
    *  // Returns None because None is passed to the collect method.
    *  None collect {case value => value}
    *  }}}
    *  @param  pf   the partial function.
    *  @return the result of applying `pf` to this $option's
    *  value (if possible), or $none.
    */
  @inline final def collect[B](pf: PartialFunction[A, B]): JsonOption[B] =
    this match {
      case JsonSome(x) => JsonOption(pf.lift(x))
      case JsonAbsent  => JsonAbsent
      case JsonNull    => JsonNull
    }

  /** Returns this $option if it is nonempty,
    *  otherwise return the result of evaluating `alternative`.
    *
    *  @param alternative the alternative expression.
    */
  @inline final def orElse[B >: A](alternative: => JsonOption[B]): JsonOption[B] =
    if (isEmpty) alternative else this

  /** Returns a singleton iterator returning the $option's value
    * if it is nonempty, or an empty iterator if the option is empty.
    */
  def iterator: Iterator[A] =
    if (isEmpty) collection.Iterator.empty else collection.Iterator.single(this.get)

  /** Returns a singleton list containing the $option's value
    * if it is nonempty, or the empty list if the $option is empty.
    */
  def toList: List[A] =
    if (isEmpty) List() else new ::(this.get, Nil)

  /** Returns a [[scala.util.Left]] containing the given
    * argument `left` if this $option is empty, or
    * a [[scala.util.Right]] containing this $option's value if
    * this is nonempty.
    *
    * @param left the expression to evaluate and return if this is empty
    * @see toLeft
    */
  @SuppressWarnings(Array("org.wartremover.warts.Product", "org.wartremover.warts.Serializable"))
  @inline final def toRight[X](left: => X) =
    if (isEmpty) Left(left) else Right(this.get)

  /** Returns a [[scala.util.Right]] containing the given
    * argument `right` if this is empty, or
    * a [[scala.util.Left]] containing this $option's value
    * if this $option is nonempty.
    *
    * @param right the expression to evaluate and return if this is empty
    * @see toRight
    */
  @SuppressWarnings(Array("org.wartremover.warts.Product", "org.wartremover.warts.Serializable"))
  @inline final def toLeft[X](right: => X) =
    if (isEmpty) Right(right) else Left(this.get)
}

/** Class `Some[A]` represents existing values of type `A`.
  */
final case class JsonSome[+A](x: A) extends JsonOption[A] {
  override def isEmpty  = false
  override def isAbsent = false
  override def get: A   = x

  /** Turns this JsonOption into scala Option
    */
  override def toOption: Option[A] = Some(x)
}

sealed trait JsonEmpty extends JsonOption[Nothing] {
  def isEmpty                   = true
  def get                       = throw new NoSuchElementException("JsonEmpty.get")
  def toOption: Option[Nothing] = None
}

/** This case object represents non-existent values.
  */
case object JsonAbsent extends JsonEmpty {
  override def get      = throw new NoSuchElementException("JsonAbsent.get")
  override def isAbsent = true
}

/** This case object represents null values.
  */
case object JsonNull extends JsonEmpty {
  override def get      = throw new NoSuchElementException("JsonNull.get")
  override def isAbsent = false
}
