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

import scala.annotation.compileTimeOnly
import com.qvantel.jsonapi.macrosupport.{JsonApiReaders, JsonApiWriters}

import scala.reflect.macros.blackbox

/**
  * Contains macros used to automatically create JsonApiWriter and JsonApiFormat instances from a case class for http://jsonapi.org format.
  *
  * The JsonApiFormat it self is not yet complete. The read side returns Nothing (???) and thus will crash with [[scala.NotImplementedError]]
  *
  * In the case class you have to provide 2 implicits after of which you can create an instance of the JsonApiFormat or JsonApiWriter.
  * Example for a case class called Individual.
  * {{{
  * implicit val individualResourceType: ResourceType[Individual] = "individuals"
  * implicit val individualPathTo: PathTo[Individual] = new PathTo[Individual] {
  *   private[this] val root = Path("/individuals")
  *   override final def self(id: String): Uri = root / id
  * }
  * // After these you can generate the format like this
  * implicit val individualJsonFormat: JsonApiFormat[Individual] = jsonApiFormat[Individual]
  * }}}
  */
@compileTimeOnly("Macros can only be used at compile-time")
final class Macros(val c: blackbox.Context) extends JsonApiWriters with JsonApiReaders {
  import c.universe._

  private[this] def createFormat(t: c.Type): c.Tree = {
    val rootParamName     = TermName(c.freshName("root"))
    val includedParamName = TermName(c.freshName("included"))
    val includePaths      = TermName(c.freshName("includePaths"))
    val includePath       = TermName(c.freshName("includePath"))
    q"""new _root_.com.qvantel.jsonapi.JsonApiFormat[$t] with _root_.spray.json.RootJsonFormat[$t] {
          import _root_.com.qvantel.jsonapi.PathJsonFormat
          override final def write($rootParamName: $t): _root_.spray.json.JsValue = ${primaryDataWriter(t,
                                                                                                        rootParamName)}
          override final def included($rootParamName: $t): _root_.scala.collection.immutable.Set[_root_.spray.json.JsObject] = ${includedWriter(
      t,
      rootParamName)}
          override final def read(
            $rootParamName: _root_.spray.json.JsValue,
            $includedParamName: _root_.scala.collection.immutable.Map[(String, String), _root_.spray.json.JsObject],
            $includePaths: _root_.scala.collection.immutable.Set[String],
            $includePath: String
          ): $t = ${reader(t, rootParamName, includedParamName, includePaths, includePath)}
        }"""
  }

  def createIncludes[A: c.WeakTypeTag]: c.Tree = {
    val t = weakTypeOf[A]

    val includeParamName    = TermName(c.freshName())
    val includeMapParamName = TermName(c.freshName())
    q"""
      new _root_.com.qvantel.jsonapi.Includes[$t] {
        private[this] final lazy val $includeMapParamName: Map[String, _root_.com.qvantel.jsonapi.Includes[_]] = ${includesMap(
      t)}
        override def includeAllowed($includeParamName: String): Boolean = ${includeAllowed(t,
                                                                                           includeParamName,
                                                                                           includeMapParamName)}
      }
      """
  }

  def jsonApiWriterWithNoNameManglingImpl[A: c.WeakTypeTag]: c.Tree = createWriter(weakTypeOf[A])
  def jsonApiFormatWithNoNameManglingImpl[A: c.WeakTypeTag]: c.Tree = createFormat(weakTypeOf[A])
}
