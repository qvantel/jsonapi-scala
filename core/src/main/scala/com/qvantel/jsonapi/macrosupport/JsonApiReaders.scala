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
package com.qvantel.jsonapi.macrosupport

import scala.annotation.compileTimeOnly

@compileTimeOnly("Macros can only be used at compile-time")
trait JsonApiReaders extends JsonApiCommon {
  import c.universe._
  def reader(t: c.Type,
             primaryJsonTerm: TermName,
             includedJsonTerm: TermName,
             includePaths: TermName,
             includePath: TermName): c.Tree = {
    val ts = t.typeSymbol

    if (!ts.isClass) {
      c.abort(c.enclosingPosition, "Can only handle case classes")
    } else if (ts.asClass.isSealed) {
      c.abort(c.enclosingPosition, "Can only handle case simple classes")
    } else {
      simpleCaseClass(t, primaryJsonTerm, includedJsonTerm, includePaths, includePath)
    }
  }

  private[this] def simpleCaseClass(t: c.Type,
                                    primaryJsonTerm: TermName,
                                    includedByIdTypeTerm: TermName,
                                    includePaths: TermName,
                                    includePath: TermName): c.Tree = {
    val (relationships, attributes) = partitionedCaseClassFields(t)

    // term names
    val primaryJsObj     = TermName(c.freshName("primaryJsObj"))
    val attributesObj    = TermName(c.freshName("attributesObj"))
    val relationshipsObj = TermName(c.freshName("relationshipsObj"))

    val attributeBits = attributes.filterNot(_.name.toString == "meta").map { field =>
      val name      = field.name
      val jsonName  = camelToDashes(field.name.toString)
      val fieldType = field.infoIn(t)

      // id is not in attributes but on the json object root
      if (name.toString == "id") {
        q"""$name = $primaryJsObj.fields.get($jsonName).getOrElse(throw new _root_.spray.json.DeserializationException("expected field '" + $jsonName + "' in json: " + $primaryJsObj.compactPrint)).convertTo[$fieldType]"""
      } else {
        if (fieldType <:< jsonOptionType) {
          val containedType = resolveContainedAttributeType(fieldType)
          q"""$name = $attributesObj.fields.get($jsonName).map {
               case _root_.spray.json.JsNull => _root_.com.qvantel.jsonapi.JsonNull
               case v => v.convertTo[_root_.com.qvantel.jsonapi.JsonOption[$containedType]]
             }.getOrElse(_root_.com.qvantel.jsonapi.JsonAbsent)
           """
        } else if (fieldType <:< optionType) {
          val containedType = resolveContainedAttributeType(fieldType)
          q"""$name = $attributesObj.fields.get($jsonName).filterNot(_.isInstanceOf[_root_.spray.json.JsNull.type]).map(_.convertTo[$containedType])"""
        } else if (fieldType <:< typeOf[Iterable[_]]) {
          q"""$name = $attributesObj.fields.get($jsonName).filterNot(_.isInstanceOf[_root_.spray.json.JsNull.type]).map(_.convertTo[$fieldType]).getOrElse(${emptyForIterableType(
            fieldType)})"""
        } else {
          q"""$name = $attributesObj.fields.get($jsonName).getOrElse(throw new _root_.spray.json.DeserializationException("expected field '" + $jsonName + "' in attributes json: " + $attributesObj.compactPrint)).convertTo[$fieldType]"""
        }
      }
    }

    // relationships
    val relationBits = relationships.filterNot(_.name.toString == "meta").map { field =>
      val name      = field.name
      val jsonName  = camelToDashes(field.name.toString)
      val fieldType = field.infoIn(t)

      val containedType = resolveContainedType(fieldType)

      val relIdTerm    = TermName(c.freshName("relationshipId"))
      val relTypeTerm  = TermName(c.freshName("relationshipType"))
      val jsObjectTerm = TermName(c.freshName("jsObject"))

      def loadInclude = q""" $includePaths.contains($newIncludePath) """

      def newIncludePath =
        q"""
           $includePath match {
             case "" => $jsonName
             case x => x + "." + $jsonName
           } """

      def errorHandledToOneFields =
        q"""
          $relationshipsObj.getOrElse(throw new _root_.spray.json.DeserializationException("'relationships' object missing in json"))
          .fields.get($jsonName).getOrElse(throw new _root_.spray.json.DeserializationException("expected '" + $jsonName + "' in relationships json")).asJsObject
          .fields.get("data").getOrElse(throw new _root_.spray.json.DeserializationException("expected 'data' in '" + $jsonName + "' in relationships json")).asJsObject
          .getFields("id", "type")
        """
      def errorHandledOptionToOneFields =
        q"""
          $relationshipsObj.flatMap(_.fields.get($jsonName).flatMap { rel =>
            // A "relationship object" must contain at least one of following: "data", "links" or "meta"
            val fields = rel.asJsObject.fields
            val x = fields.get("data")
            x match {
              case Some(_root_.spray.json.JsNull) => None
              case None => {
                if (!fields.contains("links") && !fields.contains("meta"))
                  throw new _root_.spray.json.DeserializationException("expected 'data', 'links' or 'meta' in '" + $jsonName + "' in relationships json")
                x
              }
              case _ => x.map(_.asJsObject.getFields("id", "type"))
            }
          })
        """

      def errorHandledJsonOptionToOneFields =
        q"""
          _root_.com.qvantel.jsonapi.JsonOption($relationshipsObj).flatMap(_.fields.get($jsonName) match {
            case None => _root_.com.qvantel.jsonapi.JsonAbsent
            case Some(_root_.spray.json.JsNull) => _root_.com.qvantel.jsonapi.JsonNull
            case Some(rel) =>
              // A "relationship object" must contain at least one of following: "data", "links" or "meta"
              val fields = rel.asJsObject.fields
              val x = fields.get("data")
              x match {
                case Some(_root_.spray.json.JsNull) => _root_.com.qvantel.jsonapi.JsonNull
                case None if !fields.contains("links") && !fields.contains("meta") =>
                  throw new _root_.spray.json.DeserializationException("expected 'data', 'links' or 'meta' in '" + $jsonName + "' in relationships json")
                case _ => _root_.com.qvantel.jsonapi.JsonOption(x.map(_.asJsObject.getFields("id", "type")))
              }
          })
        """
      def toOneCases =
        List(
          cq"""Seq(_root_.spray.json.JsString($relIdTerm), _root_.spray.json.JsString($relTypeTerm)) =>
          if ($relIdTerm == "") {
            throw new _root_.spray.json.DeserializationException("illegal id 'empty string' found in Resource Identifier Object")
          } else {
            $includedByIdTypeTerm.get(($relIdTerm, $relTypeTerm)) match {
              case Some($jsObjectTerm) if $loadInclude =>
                _root_.com.qvantel.jsonapi.ToOne.loaded[$containedType](implicitly[_root_.com.qvantel.jsonapi.JsonApiFormat[$containedType]].read($jsObjectTerm, $includedByIdTypeTerm, $includePaths, $newIncludePath))
              case _ => _root_.com.qvantel.jsonapi.ToOne.reference[$containedType]($relIdTerm)
            }
          }
        """,
          cq"""_ => throw new _root_.spray.json.DeserializationException("id and type expected")"""
        )

      def polyToOneCases =
        coproductTypes(containedType).map { cType =>
          cq"""
            Seq(_root_.spray.json.JsString($relIdTerm), _root_.spray.json.JsString($relTypeTerm)) if $relTypeTerm == implicitly[_root_.com.qvantel.jsonapi.ResourceType[$cType]].resourceType =>
              if ($relIdTerm == "") {
                throw new _root_.spray.json.DeserializationException("illegal id 'empty string' found  n Resource Identifier Object")
              } else {
                $includedByIdTypeTerm.get(($relIdTerm, $relTypeTerm)) match {
                  case Some($jsObjectTerm) if $loadInclude =>
                    _root_.com.qvantel.jsonapi.PolyToOne.loaded[$containedType, $cType](implicitly[_root_.com.qvantel.jsonapi.JsonApiFormat[$cType]].read($jsObjectTerm, $includedByIdTypeTerm, $includePaths, $newIncludePath))
                  case _ =>
                    _root_.com.qvantel.jsonapi.PolyToOne.reference[$containedType, $cType]($relIdTerm)
                }
              }
          """
        } :+ cq"""Seq(_root_.spray.json.JsString($relIdTerm), _root_.spray.json.JsString($relTypeTerm)) => throw new _root_.spray.json.DeserializationException("relationship of type '" + $relTypeTerm + "' is not part of coproduct '" + ${containedType.toString} + "'")"""


      // to one types
      if (fieldType <:< toOneType) {
        q""" $name = $errorHandledToOneFields match { case ..$toOneCases } """
      } else if (fieldType <:< polyToOneType) {
        q""" $name = $errorHandledToOneFields match { case ..$polyToOneCases } """
      } /* optional to one types  */
      else if (fieldType <:< optionalToOneType) {
        q""" $name = $errorHandledOptionToOneFields.map(_ match { case ..$toOneCases }) """
      } /* json optional to one types  */
      else if (fieldType <:< jsonOptionalToOneType) {
        q""" $name = $errorHandledJsonOptionToOneFields.map(_ match { case ..$toOneCases }) """
      } else if (fieldType <:< optionalPolyToOneType) {
        q""" $name = $errorHandledOptionToOneFields.map(_ match { case ..$polyToOneCases }) """
      } else if (fieldType <:< jsonOptionalPolyToOneType) {
        q""" $name = $errorHandledJsonOptionToOneFields.map(_ match { case ..$polyToOneCases }) """
      } else if (fieldType <:< toManyType) {
        val relationship =
          q"""
             $relationshipsObj.flatMap(_.fields.get($jsonName).filterNot(_.isInstanceOf[_root_.spray.json.JsNull.type]).map { rel =>
                val fields = rel.asJsObject.fields
                (fields.get("data"), fields.get("links")) match {
                  case (Some(_root_.spray.json.JsNull), _) => _root_.com.qvantel.jsonapi.ToMany.reference[$containedType]
                  case (Some(dataJson), _ ) =>
                    val data = dataJson.convertTo[Seq[_root_.spray.json.JsObject]]
                    if ($loadInclude) {
                     val entities = data.map { x =>
                       val id = x.fields.get("id").map(_.convertTo[String]).getOrElse(throw new _root_.spray.json.DeserializationException("'id' not found in " + x.compactPrint))

                       if (id == "") {
                         throw new _root_.spray.json.DeserializationException("illegal id 'empty string' found in Resource Identifier Object: " + x.compactPrint)
                       }

                       val tpe = x.fields.get("type").map(_.convertTo[String]).getOrElse(throw new _root_.spray.json.DeserializationException("'type' not found in " + x.compactPrint))

                       val item = $includedByIdTypeTerm.getOrElse((id, tpe), throw new _root_.spray.json.DeserializationException("expected entity to be found in include list for id and tpe"))

                       implicitly[_root_.com.qvantel.jsonapi.JsonApiFormat[$containedType]].read(item, $includedByIdTypeTerm, $includePaths, $newIncludePath)
                     }

                     _root_.com.qvantel.jsonapi.ToMany.loaded[$containedType](entities)
                   } else {
                     _root_.com.qvantel.jsonapi.ToMany.reference[$containedType](data.map { x =>
                       val tpe = x.fields.get("type").map(_.convertTo[String]).getOrElse(throw new _root_.spray.json.DeserializationException("'type' not found in " + x.compactPrint))
                       if (tpe != implicitly[_root_.com.qvantel.jsonapi.ResourceType[$containedType]].resourceType) {
                         throw new _root_.spray.json.DeserializationException("wrong type")
                       }
                       val id = x.fields.get("id").map(_.convertTo[String]).getOrElse(throw new _root_.spray.json.DeserializationException("'id' not found in " + x.compactPrint))

                       if (id == "") {
                         throw new _root_.spray.json.DeserializationException("illegal id 'empty string' found in Resource Identifier Object: " + x.compactPrint)
                       }

                       id
                     }.toSet)
                   }
                  case (_, Some(links)) =>
                    val related = links.asJsObject.fields.get("related")

                    related match {
                      case Some(_root_.spray.json.JsString(r)) => _root_.com.qvantel.jsonapi.ToMany.reference[$containedType](r)
                      case _ => throw new _root_.spray.json.DeserializationException("related link expected in: " + links.compactPrint)
                    }
                  case _ =>
                    throw new _root_.spray.json.DeserializationException("invalid resource object: " + rel.compactPrint)
                }
              }).getOrElse(_root_.com.qvantel.jsonapi.ToMany.reference[$containedType])

          """
        q""" $name = $relationship """
      } else if (fieldType <:< polyToManyType) {
        val coproductTypeList = coproductTypes(containedType).map { cType =>
          q""" implicitly[_root_.com.qvantel.jsonapi.ResourceType[$cType]].resourceType """
        }

        val coproductLoader =
          coproductTypes(containedType).map { cType =>
            cq"""
              tpe if tpe == implicitly[_root_.com.qvantel.jsonapi.ResourceType[$cType]].resourceType =>
                _root_.shapeless.Coproduct[$containedType](implicitly[_root_.com.qvantel.jsonapi.JsonApiFormat[$cType]].read($jsObjectTerm, $includedByIdTypeTerm, $includePaths, $newIncludePath))
            """
          } :+ cq"""tpe => throw new _root_.spray.json.DeserializationException("relationship of type '" + tpe + "' is not one of [" + $coproductTypeList.mkString(",") + "]")"""

        val coproductTypeChecker =
          coproductTypes(containedType).map { cType =>
            cq"""
              tpe if tpe == implicitly[_root_.com.qvantel.jsonapi.ResourceType[$cType]].resourceType => Unit
            """
          } :+ cq"""tpe => throw new _root_.spray.json.DeserializationException("relationship of type '" + tpe + "' is not one of [" + $coproductTypeList.mkString(",") + "]")"""

        val relationship =
          q"""
             $relationshipsObj.flatMap(_.fields.get($jsonName).filterNot(_.isInstanceOf[_root_.spray.json.JsNull.type]).map { rel =>
                val fields = rel.asJsObject.fields
                (fields.get("data"), fields.get("links")) match {
                  case (Some(_root_.spray.json.JsNull), _) => _root_.com.qvantel.jsonapi.PolyToMany.reference[$containedType]
                  case (Some(dataJson), _ ) =>
                    val data = dataJson.convertTo[Seq[_root_.spray.json.JsObject]]
                    if ($loadInclude) {
                     val entities = data.map { x =>
                       val id = x.fields.get("id").map(_.convertTo[String]).getOrElse(throw new _root_.spray.json.DeserializationException("'id' not found in " + x.compactPrint))

                       if (id == "") {
                         throw new _root_.spray.json.DeserializationException("illegal id 'empty string' found in Resource Identifier Object: " + x.compactPrint)
                       }

                       val tpe = x.fields.get("type").map(_.convertTo[String]).getOrElse(throw new _root_.spray.json.DeserializationException("'type' not found in " + x.compactPrint))

                       val $jsObjectTerm = $includedByIdTypeTerm.getOrElse((id, tpe), throw new _root_.spray.json.DeserializationException("expected entity to be found in include list for id and tpe"))


                       tpe match { case ..$coproductLoader }
                     }

                     _root_.com.qvantel.jsonapi.PolyToMany.loaded[$containedType](entities)
                   } else {
                     _root_.com.qvantel.jsonapi.PolyToMany.reference[$containedType](data.map { x =>
                       val tpe = x.fields.get("type").map(_.convertTo[String]).getOrElse(throw new _root_.spray.json.DeserializationException("'type' not found in " + x.compactPrint))
                       val id = x.fields.get("id").map(_.convertTo[String]).getOrElse(throw new _root_.spray.json.DeserializationException("'id' not found in " + x.compactPrint))

                       if (id == "") {
                         throw new _root_.spray.json.DeserializationException("illegal id 'empty string' found in Resource Identifier Object: " + x.compactPrint)
                       }

                       tpe match { case ..$coproductTypeChecker }

                       (id, tpe)
                     }.toMap)
                   }
                  case (_, Some(links)) =>
                    val related = links.asJsObject.fields.get("related")

                    related match {
                      case Some(_root_.spray.json.JsString(r)) => _root_.com.qvantel.jsonapi.PolyToMany.reference[$containedType](r)
                      case _ => throw new _root_.spray.json.DeserializationException("related link expected in: " + links.compactPrint)
                    }
                  case _ =>
                    throw new _root_.spray.json.DeserializationException("invalid resource object: " + rel.compactPrint)
                }
              }).getOrElse(_root_.com.qvantel.jsonapi.PolyToMany.reference[$containedType])

          """
        q""" $name = $relationship """
      } else {
        c.abort(c.enclosingPosition, s"cannot handle $fieldType type of relationship yet")
      }
    }

    val attributesObjVal =
      if (attributeBits.isEmpty || (attributes.size == 1 && attributes.exists(_.name.toString == "id"))) {
        q""
      } else {
        q""" val $attributesObj = $primaryJsObj.fields.get("attributes").filterNot(_.isInstanceOf[_root_.spray.json.JsNull.type]).map(x => x.asJsObject("json object expected but got: " + x.compactPrint)).getOrElse(_root_.spray.json.JsObject.empty) """
      }

    val relationshipsObjVal = if (relationBits.isEmpty) {
      q""
    } else {
      q""" val $relationshipsObj = $primaryJsObj.fields.get("relationships").filterNot(_.isInstanceOf[_root_.spray.json.JsNull.type]).map(x => x.asJsObject("json object expected but got: " + x.compactPrint)) """
    }

    val metaBits = attributes.find(_.name.toString == "meta") match {
      case Some(meta) =>
        List(q"""
          meta = $primaryJsObj.fields.get("meta").map { j =>
            j.asJsObject("expected meta to be a json object but got: " + j.compactPrint).fields.map { case (key, value) =>
              (key, _root_.com.qvantel.jsonapi.UntypedMeta(value))
            }.toMap
          }.getOrElse(Map.empty)
        """)
      case _ =>
        List.empty
    }

    // check type if true set terms and generate new instance
    q"""
       val $primaryJsObj = $primaryJsonTerm.asJsObject("json object expected but got: " + $primaryJsonTerm.compactPrint)
       val tpe = $primaryJsObj.fields("type").convertTo[String]
       val expectedType = implicitly[_root_.com.qvantel.jsonapi.ResourceType[$t]].resourceType
       if(tpe == expectedType) {
         $attributesObjVal
         $relationshipsObjVal

         new $t(..${attributeBits ++ relationBits ++ metaBits})
       } else {
         throw new _root_.spray.json.DeserializationException("wrong type '" + expectedType + "' expected but got '" + tpe + "'")
       }
     """
  }
}
