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
 * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.qvantel.jsonapi

import scala.annotation.compileTimeOnly
import scala.reflect.macros.whitebox.{Context => WhiteboxContext}
import mojolly.inflector.Inflector

import com.qvantel.jsonapi.JsonApiResourceMacro.{Mode, NoId, Normal}
import com.qvantel.jsonapi.macrosupport.JsonApiCommon

@compileTimeOnly("‘jsonApiResource’ is a compile-time annotation")
final class JsonApiResourceMacro(val c: WhiteboxContext) extends JsonApiCommon {
  import c.universe._

  private[this] def createIdentifiable(name: TypeName): c.Tree =
    q"""implicit final val ${TermName(s"${name}Identifiable")}: _root_.com.qvantel.jsonapi.Identifiable[$name] = _root_.com.qvantel.jsonapi.Identifiable.by(_.id.toString)"""

  private[this] def createResourceType(name: TypeName, resourceTypeName: String): c.Tree =
    q"""implicit final val ${TermName(s"${name}ResourceType")}: _root_.com.qvantel.jsonapi.ResourceType[$name] = _root_.com.qvantel.jsonapi.ResourceType($resourceTypeName)"""

  private[this] def createPathTo(name: TypeName, resourceTypeName: String): c.Tree = {
    val path = s"/$resourceTypeName"
    q"""implicit final val ${TermName(s"${name}PathTo")}: _root_.com.qvantel.jsonapi.PathTo[$name] = new _root_.com.qvantel.jsonapi.PathTo[$name] {
          override final def self(id: String): _root_.spray.http.Uri.Path = _root_.spray.http.Uri.Path($path) / id
        }"""
  }

  private[this] def createIncludes(name: TypeName): c.Tree =
    q"""implicit final val ${TermName(s"${name}Includes")}: _root_.com.qvantel.jsonapi.Includes[$name] = _root_.com.qvantel.jsonapi.includes[$name]"""

  private[this] def createJsonApiFormat(name: TypeName): c.Tree =
    q"""implicit final val ${TermName(s"${name}JsonApiFormat")}: _root_.com.qvantel.jsonapi.JsonApiFormat[$name] = _root_.com.qvantel.jsonapi.jsonApiFormat[$name]"""

  private[this] def modifiedDeclaration(classDecl: ClassDef, maybeCompanionDecl: Option[ModuleDef]): Tree = {
    val className = classDecl.name

    val (maybeResourceTypeName, mode) = getResourceTypeAndMode(c.prefix.tree)

    val noId = mode == NoId

    val resourceTypeName =
      maybeResourceTypeName.getOrElse(Inflector.pluralize(NameManglers.dasherized(className.toString)))

    val companionDecl = maybeCompanionDecl getOrElse q"""object ${className.toTermName} {}""" match {
      case m @ q"$mods object $name extends { ..$earlydefns } with ..$parents { $self => ..$body }" =>
        val defaultParts = List(createResourceType(className, resourceTypeName),
                                createIncludes(className),
                                createJsonApiFormat(className))

        val withIdRequiringParts = if (noId) {
          defaultParts
        } else {
          defaultParts ++ List(createIdentifiable(className), createPathTo(className, resourceTypeName))
        }

        q"""object $name {
             ..$withIdRequiringParts
             ..$body }"""
    }

    val result = q"""
      $classDecl
      $companionDecl
    """
    result
  }

  private[this] def getResourceTypeAndMode(tree: Tree): (Option[String], Mode) = {
    def resourceTypeToOpt(res: String) =
      res match {
        case "normal" => None
        case _        => Some(res)
      }

    tree match {
      case q"new $name( ..$params )" =>
        params match {
          case resourceType :: mode :: Nil =>
            (resourceTypeToOpt(c.eval[String](c.Expr(resourceType))), Mode(c.eval[String](c.Expr(mode))))
          case resourceType :: Nil =>
            (resourceTypeToOpt(c.eval[String](c.Expr(resourceType))), Normal)
          case _ => (None, Normal)
        }
      case _ => (None, Normal)
    }
  }

  def impl(annottees: c.Tree*): c.Tree = {
    import c.universe._

    annottees match {
      case (classDecl: ClassDef) :: Nil =>
        modifiedDeclaration(classDecl, None)
      case (classDecl: ClassDef) :: (compDecl: ModuleDef) :: Nil =>
        modifiedDeclaration(classDecl, Some(compDecl))
      case _ =>
        c.abort(c.enclosingPosition, "Invalid annottee")
    }
  }
}

object JsonApiResourceMacro {
  sealed trait Mode
  case object NoId   extends Mode
  case object Normal extends Mode

  object Mode {
    def apply(str: String): Mode = str.toLowerCase match {
      case "normal" => Normal
      case "no-id"  => NoId
      case _        => Normal
    }
  }
}
