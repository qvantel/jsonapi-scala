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
trait JsonApiWriters extends JsonApiCommon {
  import c.universe._

  private[this] def resourceTypeJson(t: c.Type): c.Tree =
    q"_root_.spray.json.JsString(${resourceType(t)})"

  private[this] def idJson(t: c.Type, objName: TermName): c.Tree =
    q"_root_.spray.json.JsString(_root_.scala.Predef.implicitly[_root_.com.qvantel.jsonapi.Identifiable[$t]].identify($objName))"

  private[this] def selfPathJson(t: c.Type, objName: TermName): c.Tree =
    q"""
       _root_.scala.Predef.implicitly[_root_.spray.json.JsonWriter[_root_.com.netaporter.uri.Uri]].write(_root_.scala.Predef.implicitly[_root_.com.qvantel.jsonapi.PathTo[$t]].entity($objName))
      """

  private[this] val emptyJsObjectSet: c.Tree =
    q"_root_.scala.collection.immutable.Set.empty[_root_.spray.json.JsObject]"

  private[this] def entityJson(t: c.Type, objName: TermName): c.Tree =
    q"_root_.scala.Predef.implicitly[_root_.com.qvantel.jsonapi.JsonApiWriter[$t]].write($objName, sparseFields).asJsObject"

  private[this] def entityJsonSingleton(t: c.Type, objName: TermName): c.Tree =
    q"_root_.scala.collection.immutable.Set[_root_.spray.json.JsObject](${entityJson(t, objName)})"

  private[this] def fieldJson(t: c.Type, objName: TermName)(field: c.universe.TermSymbol): c.Tree = {
    val fieldType = field.infoIn(t)
    val fieldName = field.name.toString

    if (fieldType <:< typeOf[com.qvantel.jsonapi.JsonOption[_]]) {
      c.abort(c.enclosingPosition, s"JsonOption[_] not allowed. $fieldName: $fieldType give")
    } else {
      q"""(${camelToDashes(fieldName)}, _root_.scala.Predef.implicitly[_root_.spray.json.JsonWriter[$fieldType]].write($objName.${field.name}))"""
    }
  }

  private[this] def jsonOptionFieldJson(t: c.Type, objName: TermName)(field: c.universe.TermSymbol): c.Tree = {
    val fieldType = field.infoIn(t)
    val fieldName = field.name.toString

    if (fieldType <:< typeOf[com.qvantel.jsonapi.JsonOption[_]]) {
      q"""if($objName.${field.name} == _root_.com.qvantel.jsonapi.JsonAbsent)
            None
          else
            Some((${camelToDashes(fieldName)}, _root_.scala.Predef.implicitly[_root_.spray.json.JsonWriter[$fieldType]].write($objName.${field.name})))
      """
    } else {
      c.abort(c.enclosingPosition, s"only JsonOption[_] allowed. $fieldName: $fieldType give")
    }
  }

  private[this] def relationJson(t: c.Type, st: c.Type, objName: TermName)(field: c.universe.TermSymbol): c.Tree = {
    val fieldType     = field.infoIn(t)
    val fieldName     = field.name.toString
    val containedType = resolveContainedType(fieldType)
    val json =
      q"""_root_.com.qvantel.jsonapi.Link.to[$containedType, $st]($objName, $objName.${field.name}, ${camelToDashes(
        fieldName)})"""
    q"""(${camelToDashes(fieldName)}, $json)"""
  }

  private[this] def relationJsonNoParentPath(t: c.Type, st: c.Type, objName: TermName)(
      field: c.universe.TermSymbol): c.Tree = {
    val fieldType     = field.infoIn(t)
    val fieldName     = field.name.toString
    val containedType = resolveContainedType(fieldType)
    val json =
      q"""_root_.com.qvantel.jsonapi.Link.toNoParentPath[$containedType, $st]($objName, $objName.${field.name}, ${camelToDashes(
        fieldName)})"""
    if (fieldType <:< typeOf[com.qvantel.jsonapi.JsonOption[_]]) {
      q"""if ($objName.${field.name} == _root_.com.qvantel.jsonapi.JsonAbsent) None else Some((${camelToDashes(
        fieldName)}, $json))"""
    } else {
      q"""Some((${camelToDashes(fieldName)}, $json))"""
    }
  }

  private[this] def simpleCaseClassPrimaryDataWriter(t: c.Type, st: c.Type, objName: TermName): c.Tree = {
    val (relationFields, allAttributeFields) = partitionedCaseClassFields(t)

    val attributeFields = allAttributeFields.filter(x => x.name.toString != "id" && x.name.toString != "meta")

    val hasId   = allAttributeFields.exists(_.name.toString == "id")
    val hasMeta = allAttributeFields.exists(_.name.toString == "meta")

    val idRequiredFields = if (hasId) {
      val selfFieldJson      = q"""("self", ${selfPathJson(st, objName)})"""
      val idFieldJson        = q"""("id", ${idJson(st, objName)})"""
      val linksJson          = q"""("links" -> _root_.spray.json.JsObject(..$selfFieldJson))"""
      val relationFieldsJson = relationFields map relationJson(t, st, objName)

      val baseFields = Seq(idFieldJson, linksJson)

      if (relationFieldsJson.isEmpty) {
        baseFields
      } else {
        val chosenRelationships =
          q"""if (sparseFields.contains(${resourceType(st)}))
                  $relationFieldsJson.asInstanceOf[List[(String, _root_.spray.json.JsValue)]].filter{ case(relationshipName, _) =>
                    sparseFields(${resourceType(st)}).contains(relationshipName)
                  }
                else
                  $relationFieldsJson
              """

        baseFields :+ q"""("relationships" -> _root_.spray.json.JsObject($chosenRelationships:_*))"""
      }
    } else {
      val relationFieldsJson = relationFields map relationJsonNoParentPath(t, st, objName)

      if (relationFieldsJson.isEmpty) {
        Seq.empty
      } else {
        val chosenRelationships =
          q"""if (sparseFields.contains(${resourceType(st)}))
                  $relationFieldsJson.flatten.asInstanceOf[List[(String, _root_.spray.json.JsValue)]].filter{ case(relationshipName, _) =>
                    sparseFields(${resourceType(st)}).contains(relationshipName)
                  }
                else
                  $relationFieldsJson.flatten
              """

        Seq(q"""("relationships" -> _root_.spray.json.JsObject($chosenRelationships:_*))""")
      }
    }

    val typeFieldJson = q"""("type", ${resourceTypeJson(st)})"""
    val jsonOptionAttributes = attributeFields.filter { field =>
      field.infoIn(t) <:< typeOf[com.qvantel.jsonapi.JsonOption[_]]
    }
    val nonJsonOptionAttribtes = attributeFields.filterNot { field =>
      field.infoIn(t) <:< typeOf[com.qvantel.jsonapi.JsonOption[_]]
    }
    val nonJsonOptionAttributeFieldsJson = nonJsonOptionAttribtes map fieldJson(t, objName)
    val jsonOptionAttributeFieldsJson    = jsonOptionAttributes map jsonOptionFieldJson(t, objName)

    val chosenAttributes = {
      val attributes = q"""$nonJsonOptionAttributeFieldsJson ++ $jsonOptionAttributeFieldsJson.flatten"""
      q"""if (sparseFields.contains(${resourceType(st)}))
              $attributes.asInstanceOf[List[(String, _root_.spray.json.JsValue)]].filter { case(attributeName, _) =>
                sparseFields(${resourceType(st)}).contains(attributeName)
              }
            else
              $attributes
        """
    }

    val attributesJson =
      q"""("attributes" -> _root_.spray.json.JsObject($chosenAttributes:_*))"""

    val metaFields = allAttributeFields.find(_.name.toString == "meta") match {
      case _root_.scala.Some(meta) if meta.infoIn(t) <:< typeOf[Map[String, com.qvantel.jsonapi.Meta]] =>
        Seq(q"""
            if($objName.meta.isEmpty) {
              Map.empty[String, _root_.spray.json.JsObject]
            } else {
              $objName.meta.map { case(key, value) =>
                key -> value.asJson
              }.toMap
            }
           """)
      case _root_.scala.Some(_) =>
        c.abort(c.enclosingPosition, "can only have meta of type Map[String, Meta]")
      case _root_.scala.None => Seq.empty
    }

    val metaJson = q"""("meta" -> _root_.spray.json.JsObject(..$metaFields))"""

    val allFields = if (hasMeta) {
      Seq(typeFieldJson, attributesJson, metaJson) ++ idRequiredFields
    } else {
      Seq(typeFieldJson, attributesJson) ++ idRequiredFields
    }

    q"""_root_.spray.json.JsObject(
        List(..$allFields).filter { case(_, fieldValue) => fieldValue != _root_.spray.json.JsObject.empty }:_*
    )"""
  }

  private[this] def sumTypePrimaryDataWriter(t: c.Type, objName: TermName): c.Tree = {
    val children    = t.typeSymbol.asClass.knownDirectSubclasses
    val caseObjName = TermName(c.freshName())
    val cases = children map { (child: c.universe.Symbol) =>
      cq"$caseObjName: $child => ${simpleCaseClassPrimaryDataWriter(child.asClass.toType, t, caseObjName)}"
    }
    q"""$objName match { case ..$cases }"""
  }

  private[this] def simpleCaseClassRelationWriter(t: c.Type, depth: Int, objName: TermName): c.Tree = {
    val caseObjName = TermName(c.freshName())
    val relationFieldsJson = for {
      field <- relationFields(t)
      fieldType     = field.infoIn(t)
      containedType = resolveContainedType(fieldType)
    } yield {
      if (fieldType <:< toOneType) {
        q"""$objName.${field.name} match {
              case _root_.com.qvantel.jsonapi.ToOne.Loaded($caseObjName) =>
                ${entityJsonSingleton(containedType, caseObjName)} ++ _root_.scala.Predef.implicitly[_root_.com.qvantel.jsonapi.JsonApiWriter[$containedType]].included($caseObjName, sparseFields)
              case _ => $emptyJsObjectSet
            }"""
      } else if (fieldType <:< polyToOneType) {
        val referenceCase           = cq"_root_.com.qvantel.jsonapi.PolyToOne.Reference(_, _) => $emptyJsObjectSet"
        val containedCoproductTypes = coproductTypes(containedType)
        val loadedCases = containedCoproductTypes.zip(coproductPatterns(containedCoproductTypes.size, caseObjName)) map {
          case (cType, pattern) =>
            cq"""_root_.com.qvantel.jsonapi.PolyToOne.Loaded($pattern, _, _) =>
                 ${entityJsonSingleton(cType, caseObjName)} ++ _root_.scala.Predef.implicitly[_root_.com.qvantel.jsonapi.JsonApiWriter[$cType]].included($caseObjName, sparseFields)"""
        }
        val defaultCase = cq"""_ => throw new Exception("Internal error in Coproduct handling")"""
        val cases       = referenceCase +: loadedCases :+ defaultCase
        q"$objName.${field.name} match { case ..$cases }"
      } else if (fieldType <:< optionalToOneType) {
        q"""$objName.${field.name} match {
              case _root_.scala.Some(_root_.com.qvantel.jsonapi.ToOne.Loaded($caseObjName)) =>
                ${entityJsonSingleton(containedType, caseObjName)} ++ _root_.scala.Predef.implicitly[_root_.com.qvantel.jsonapi.JsonApiWriter[$containedType]].included($caseObjName, sparseFields)
              case _ => $emptyJsObjectSet
            }"""
      } else if (fieldType <:< jsonOptionalToOneType) {
        q"""$objName.${field.name} match {
                  case _root_.com.qvantel.jsonapi.JsonSome(_root_.com.qvantel.jsonapi.ToOne.Loaded($caseObjName)) =>
                    ${entityJsonSingleton(containedType, caseObjName)} ++ _root_.scala.Predef.implicitly[_root_.com.qvantel.jsonapi.JsonApiWriter[$containedType]].included($caseObjName, sparseFields)
                  case _ => $emptyJsObjectSet
                }"""
      } else if (fieldType <:< optionalPolyToOneType) {
        val referenceCase =
          cq"_root_.scala.None | _root_.scala.Some(_root_.com.qvantel.jsonapi.PolyToOne.Reference(_, _)) => $emptyJsObjectSet"
        val containedCoproductTypes = coproductTypes(containedType)
        val loadedCases = containedCoproductTypes.zip(coproductPatterns(containedCoproductTypes.size, caseObjName)) map {
          case (cType, pattern) =>
            cq"""_root_.scala.Some(_root_.com.qvantel.jsonapi.PolyToOne.Loaded($pattern, _, _)) =>
                   ${entityJsonSingleton(cType, caseObjName)} ++ _root_.scala.Predef.implicitly[_root_.com.qvantel.jsonapi.JsonApiWriter[$cType]].included($caseObjName, sparseFields)"""
        }
        val defaultCase = cq"""_ => throw new Exception("Internal error in Coproduct handling")"""
        val cases       = referenceCase +: loadedCases :+ defaultCase
        q"$objName.${field.name} match { case ..$cases }"
      } else if (fieldType <:< jsonOptionalPolyToOneType) {
        val referenceCase =
          cq"""(_root_.com.qvantel.jsonapi.JsonAbsent |
                  _root_.com.qvantel.jsonapi.JsonNull |
                  _root_.com.qvantel.jsonapi.JsonSome(_root_.com.qvantel.jsonapi.PolyToOne.Reference(_, _))) => $emptyJsObjectSet"""
        val containedCoproductTypes = coproductTypes(containedType)
        val loadedCases = containedCoproductTypes.zip(coproductPatterns(containedCoproductTypes.size, caseObjName)) map {
          case (cType, pattern) =>
            cq"""_root_.com.qvantel.jsonapi.JsonSome(_root_.com.qvantel.jsonapi.PolyToOne.Loaded($pattern, _, _)) =>
                   ${entityJsonSingleton(cType, caseObjName)} ++ _root_.scala.Predef.implicitly[_root_.com.qvantel.jsonapi.JsonApiWriter[$cType]].included($caseObjName, sparseFields)"""
        }
        val defaultCase = cq"""_ => throw new Exception("Internal error in Coproduct handling")"""
        val cases       = referenceCase +: loadedCases :+ defaultCase
        q"$objName.${field.name} match { case ..$cases }"
      } else if (fieldType <:< toManyType) {
        val itemName = TermName(c.freshName())
        val itemSym  = q"$itemName: $containedType"
        q"""$objName.${field.name} match {
              case _root_.com.qvantel.jsonapi.ToMany.Loaded($caseObjName) =>
                $caseObjName.map($itemSym => ${entityJson(containedType, itemName)}).toSet ++
                  $caseObjName.flatMap($itemSym => _root_.scala.Predef.implicitly[_root_.com.qvantel.jsonapi.JsonApiWriter[$containedType]].included($itemSym, sparseFields)).toSet
              case _ => $emptyJsObjectSet
            }"""
      } else if (fieldType <:< jsonOptionalToManyType) {
        val itemName = TermName(c.freshName())
        val itemSym  = q"$itemName: $containedType"
        q"""$objName.${field.name} match {
                  case _root_.com.qvantel.jsonapi.JsonSome(_root_.com.qvantel.jsonapi.ToMany.Loaded($caseObjName)) =>
                    $caseObjName.map($itemSym => ${entityJson(containedType, itemName)}).toSet ++
                      $caseObjName.flatMap($itemSym => _root_.scala.Predef.implicitly[_root_.com.qvantel.jsonapi.JsonApiWriter[$containedType]].included($itemSym, sparseFields)).toSet
                  case _ => $emptyJsObjectSet
                }"""
      } else if (fieldType <:< polyToManyType) {
        val itemName                = TermName(c.freshName())
        val containedCoproductTypes = coproductTypes(containedType)
        val casePatternsAndTypes =
          containedCoproductTypes.zip(coproductPatterns(containedCoproductTypes.size, itemName))
        val defaultCase = cq"""_ => throw new Exception("Internal error in Coproduct handling")"""
        val entityElementCases = casePatternsAndTypes map {
          case (cType, pattern) =>
            cq"$pattern => ${entityJson(cType, itemName)}"
        }

        val entityRelationCases = casePatternsAndTypes map {
          case (cType, pattern) =>
            cq"$pattern =>_root_.scala.Predef.implicitly[_root_.com.qvantel.jsonapi.JsonApiWriter[$cType]].included($itemName, sparseFields)"
        }

        q"""$objName.${field.name} match {
                case _root_.com.qvantel.jsonapi.PolyToMany.Loaded($caseObjName) =>
                  $caseObjName.map{case ..${entityElementCases :+ defaultCase}}.toSet ++
                    $caseObjName.flatMap{case ..${entityRelationCases :+ defaultCase}}.toSet
                case _ => $emptyJsObjectSet
              }"""
      } else if (fieldType <:< jsonOptionalPolyToManyType) {
        val itemName                = TermName(c.freshName())
        val containedCoproductTypes = coproductTypes(containedType)
        val casePatternsAndTypes =
          containedCoproductTypes.zip(coproductPatterns(containedCoproductTypes.size, itemName))
        val entityElementCases = casePatternsAndTypes map {
          case (cType, pattern) =>
            cq"$pattern => ${entityJson(cType, itemName)}"
        }
        val entityRelationCases = casePatternsAndTypes map {
          case (cType, pattern) =>
            cq"$pattern =>_root_.scala.Predef.implicitly[_root_.com.qvantel.jsonapi.JsonApiWriter[$cType]].included($itemName, sparseFields)"
        }

        val defaultCase = cq"""_ => throw new Exception("Internal error in Coproduct handling")"""
        val loadedCases = containedCoproductTypes.zip(coproductPatterns(containedCoproductTypes.size, caseObjName)) map {
          case (_, _) =>
            cq"""_root_.com.qvantel.jsonapi.JsonSome(_root_.com.qvantel.jsonapi.PolyToMany.Loaded($caseObjName)) =>
                  $caseObjName.map{case ..${entityElementCases :+ defaultCase}}.toSet ++
                    $caseObjName.flatMap{case ..${entityRelationCases :+ defaultCase}}.toSet"""
        }
        val referenceCase =
          cq"""(_root_.com.qvantel.jsonapi.JsonAbsent |
                  _root_.com.qvantel.jsonapi.JsonNull |
                  _root_.com.qvantel.jsonapi.JsonSome(_)) => $emptyJsObjectSet"""
        val cases = loadedCases :+ referenceCase

        q"$objName.${field.name} match { case ..$cases }"
      } else {
        c.abort(c.enclosingPosition, s"Unexpected relation type $fieldType")
      }
    }

    if (relationFieldsJson.isEmpty) {
      q"_root_.scala.collection.immutable.Set.empty[_root_.spray.json.JsObject]"
    } else {
      q"""_root_.scala.collection.immutable.Set(..$relationFieldsJson).flatten"""
    }
  }

  private[this] def sumTypeRelationWriter(t: c.Type, depth: Int, objName: TermName): c.Tree = {
    val children    = t.typeSymbol.asClass.knownDirectSubclasses
    val caseObjName = TermName(c.freshName())
    val okCases = children map { (child: c.universe.Symbol) =>
      cq"$caseObjName: $child => ${simpleCaseClassRelationWriter(child.asClass.toType, depth, caseObjName)}"
    }
    val failCase = cq"""_ => throw new Exception("Internal error in Coproduct handling")"""

    val cases = okCases + failCase
    q"""$objName match { case ..$cases }"""
  }

  private[this] def relationWriter(t: c.Type, depth: Int, objName: TermName): c.Tree = {
    val ts = t.typeSymbol
    if (!ts.isClass) {
      c.abort(c.enclosingPosition, "Can only handle classes")
    } else if (ts.asClass.isSealed) {
      sumTypeRelationWriter(t, depth, objName)
    } else {
      simpleCaseClassRelationWriter(t, depth, objName)
    }
  }

  private[this] def simpleCaseClassIncludedWriter(t: c.Type, st: c.Type, objName: TermName): c.Tree = {
    val caseObjName = TermName(c.freshName())
    val relationFieldsJson = for {
      field <- relationFields(t)
      fieldType     = field.infoIn(t)
      containedType = resolveContainedType(fieldType)
    } yield {
      if (fieldType <:< toOneType) {
        q"""$objName.${field.name} match {
                case _root_.com.qvantel.jsonapi.ToOne.Loaded($caseObjName) =>
                  ${entityJsonSingleton(containedType, caseObjName)} ++ ${relationWriter(containedType, 0, caseObjName)}
                case _ => $emptyJsObjectSet
              }"""
      } else if (fieldType <:< optionalToOneType) {
        q"""$objName.${field.name} match {
                case _root_.scala.Some(_root_.com.qvantel.jsonapi.ToOne.Loaded($caseObjName)) =>
                  ${entityJsonSingleton(containedType, caseObjName)} ++ ${relationWriter(containedType, 0, caseObjName)}
                case _ => $emptyJsObjectSet
              }"""
      } else if (fieldType <:< jsonOptionalToOneType) {
        q"""$objName.${field.name} match {
                case _root_.com.qvantel.jsonapi.JsonSome(_root_.com.qvantel.jsonapi.ToOne.Loaded($caseObjName)) =>
                  ${entityJsonSingleton(containedType, caseObjName)} ++ ${relationWriter(containedType, 0, caseObjName)}
                case _ => $emptyJsObjectSet
              }"""
      } else if (fieldType <:< polyToOneType) {
        val containedCoproductTypes = coproductTypes(containedType)
        val referenceCase           = cq"_root_.com.qvantel.jsonapi.PolyToOne.Reference(_, _) => $emptyJsObjectSet"
        val loadedCases = containedCoproductTypes.zip(coproductPatterns(containedCoproductTypes.size, caseObjName)) map {
          case (cType, pattern) =>
            cq"""_root_.com.qvantel.jsonapi.PolyToOne.Loaded($pattern, _, _) =>
                   ${entityJsonSingleton(cType, caseObjName)} ++ ${relationWriter(cType, 0, caseObjName)}"""
        }
        val defaultCase = cq"""_ => throw new Exception("Internal error in Coproduct handling")"""
        val cases       = referenceCase +: loadedCases :+ defaultCase
        q"$objName.${field.name} match { case ..$cases }"
      } else if (fieldType <:< optionalPolyToOneType) {
        val containedCoproductTypes = coproductTypes(containedType)
        val referenceCase =
          cq"_root_.scala.None | _root_.scala.Some(_root_.com.qvantel.jsonapi.PolyToOne.Reference(_, _)) => $emptyJsObjectSet"
        val loadedCases = containedCoproductTypes.zip(coproductPatterns(containedCoproductTypes.size, caseObjName)) map {
          case (cType, pattern) =>
            cq"""_root_.scala.Some(_root_.com.qvantel.jsonapi.PolyToOne.Loaded($pattern, _, _)) =>
                   ${entityJsonSingleton(cType, caseObjName)} ++ ${relationWriter(cType, 0, caseObjName)}"""
        }
        val defaultCase = cq"""_ => throw new Exception("Internal error in Coproduct handling")"""
        val cases       = referenceCase +: loadedCases :+ defaultCase
        q"$objName.${field.name} match { case ..$cases }"
      } else if (fieldType <:< jsonOptionalPolyToOneType) {
        val containedCoproductTypes = coproductTypes(containedType)
        val referenceCase =
          cq"""(_root_.com.qvantel.jsonapi.JsonAbsent |
                  _root_.com.qvantel.jsonapi.JsonNull |
                  _root_.com.qvantel.jsonapi.JsonSome(_root_.com.qvantel.jsonapi.PolyToOne.Reference(_, _))) => $emptyJsObjectSet"""
        val loadedCases = containedCoproductTypes.zip(coproductPatterns(containedCoproductTypes.size, caseObjName)) map {
          case (cType, pattern) =>
            cq"""_root_.com.qvantel.jsonapi.JsonSome(_root_.com.qvantel.jsonapi.PolyToOne.Loaded($pattern, _, _)) =>
                   ${entityJsonSingleton(cType, caseObjName)} ++ ${relationWriter(cType, 0, caseObjName)}"""
        }
        val defaultCase = cq"""_ => throw new Exception("Internal error in Coproduct handling")"""
        val cases       = referenceCase +: loadedCases :+ defaultCase
        q"$objName.${field.name} match { case ..$cases }"
      } else if (fieldType <:< toManyType) {
        val itemName = TermName(c.freshName())
        val itemSym  = q"$itemName: $containedType"
        q"""$objName.${field.name} match {
                case _root_.com.qvantel.jsonapi.ToMany.Loaded($caseObjName) =>
                  $caseObjName.map($itemSym => ${entityJson(containedType, itemName)}).toSet ++
                    $caseObjName.flatMap($itemSym => ${relationWriter(containedType, 0, itemName)}).toSet
                case _ => $emptyJsObjectSet
              }"""
      } else if (fieldType <:< jsonOptionalToManyType) {
        val itemName = TermName(c.freshName())
        val itemSym  = q"$itemName: $containedType"
        q"""$objName.${field.name} match {
                case _root_.com.qvantel.jsonapi.JsonSome(_root_.com.qvantel.jsonapi.ToMany.Loaded($caseObjName)) =>
                  $caseObjName.map($itemSym => ${entityJson(containedType, itemName)}).toSet ++
                    $caseObjName.flatMap($itemSym => ${relationWriter(containedType, 0, itemName)}).toSet
                case _ => $emptyJsObjectSet
              }"""
      } else if (fieldType <:< polyToManyType) {
        val itemName                = TermName(c.freshName())
        val containedCoproductTypes = coproductTypes(containedType)
        val casePatternsAndTypes =
          containedCoproductTypes.zip(coproductPatterns(containedCoproductTypes.size, itemName))
        val defaultCase = cq"""_ => throw new Exception("Internal error in Coproduct handling")"""
        val entityElementCases = casePatternsAndTypes map {
          case (cType, pattern) =>
            cq"$pattern => ${entityJson(cType, itemName)}"
        }
        val entityRelationCases = casePatternsAndTypes map {
          case (cType, pattern) =>
            cq"$pattern => ${relationWriter(cType, 0, itemName)}"
        }
        q"""$objName.${field.name} match {
                case _root_.com.qvantel.jsonapi.PolyToMany.Loaded($caseObjName) =>
                  $caseObjName.map{case ..${entityElementCases :+ defaultCase}}.toSet ++
                    $caseObjName.flatMap{case ..${entityRelationCases :+ defaultCase}}.toSet
                case _ => $emptyJsObjectSet
              }"""
      } else if (fieldType <:< jsonOptionalPolyToManyType) {
        val itemName                = TermName(c.freshName())
        val containedCoproductTypes = coproductTypes(containedType)
        val casePatternsAndTypes =
          containedCoproductTypes.zip(coproductPatterns(containedCoproductTypes.size, itemName))
        val entityElementCases = casePatternsAndTypes map {
          case (cType, pattern) =>
            cq"$pattern => ${entityJson(cType, itemName)}"
        }
        val entityRelationCases = casePatternsAndTypes map {
          case (cType, pattern) =>
            cq"$pattern => ${relationWriter(cType, 0, itemName)}"
        }

        val defaultCase = cq"""_ => throw new Exception("Internal error in Coproduct handling")"""
        val loadedCases = containedCoproductTypes.zip(coproductPatterns(containedCoproductTypes.size, caseObjName)) map {
          case (_, _) =>
            cq"""_root_.com.qvantel.jsonapi.JsonSome(_root_.com.qvantel.jsonapi.PolyToMany.Loaded($caseObjName)) =>
                  $caseObjName.map{case ..${entityElementCases :+ defaultCase}}.toSet ++
                    $caseObjName.flatMap{case ..${entityRelationCases :+ defaultCase}}.toSet"""
        }
        val referenceCase =
          cq"""(_root_.com.qvantel.jsonapi.JsonAbsent |
                  _root_.com.qvantel.jsonapi.JsonNull |
                  _root_.com.qvantel.jsonapi.JsonSome(_)) => $emptyJsObjectSet"""
        val cases = loadedCases :+ referenceCase

        q"$objName.${field.name} match { case ..$cases }"
      } else {
        c.abort(c.enclosingPosition, s"Unexpected relation type $fieldType")
      }
    }

    if (relationFieldsJson.isEmpty) {
      q"_root_.scala.collection.immutable.Set.empty[_root_.spray.json.JsObject]"
    } else {
      q"""_root_.scala.collection.immutable.Set(..$relationFieldsJson).flatten"""
    }
  }

  private[this] def sumTypeIncludedWriter(t: c.Type, st: c.Type, objName: TermName): c.Tree = {
    val children    = t.typeSymbol.asClass.knownDirectSubclasses
    val caseObjName = TermName(c.freshName())
    val cases = children map { (child: c.universe.Symbol) =>
      cq"$caseObjName: $child => ${simpleCaseClassIncludedWriter(child.asClass.toType, t, caseObjName)}"
    }
    q"""$objName match { case ..$cases }"""
  }

  def includesMap(t: c.Type): c.Tree = {
    val ts = t.typeSymbol

    if (!ts.isClass) {
      c.abort(c.enclosingPosition, "Can only handle case classes")
    } else if (ts.asClass.isSealed) {
      sumTypeIncludesMap(t)
    } else {
      simpleIncludesMap(t)
    }
  }

  private[this] def simpleIncludesMap(t: c.Type): c.Tree = {
    val ts = t.typeSymbol

    if (!ts.isClass) {
      c.abort(c.enclosingPosition, "Can only handle case classes")
    }

    val relTypes = for {
      field <- relationFields(t)
      fieldType     = field.infoIn(t)
      containedType = resolveContainedType(fieldType)
    } yield {
      val fieldName = field.name.toString()
      if (fieldType <:< polyToOneType || fieldType <:< optionalPolyToOneType || fieldType <:< jsonOptionalPolyToOneType || fieldType <:< polyToManyType || fieldType <:< jsonOptionalPolyToManyType) {
        val polyIncludes = coproductTypes(containedType).map { cType =>
          q"_root_.scala.Predef.implicitly[_root_.com.qvantel.jsonapi.Includes[$cType]]"
        }

        (camelToDashes(fieldName),
         q"""
            new _root_.com.qvantel.jsonapi.Includes[$containedType] {
              override def includeAllowed(include: String): Boolean = {
                List(..$polyIncludes).exists(x => x.includeAllowed(include))
              }
            }
          """)
      } else {
        (camelToDashes(fieldName),
         q"_root_.scala.Predef.implicitly[_root_.com.qvantel.jsonapi.Includes[$containedType]]")
      }
    }

    q"${relTypes.toMap}"
  }

  private[this] def sumTypeIncludesMap(t: c.Type): c.Tree =
    q"Map.empty"

  def includeAllowed(t: c.Type, includeParam: TermName, incMap: TermName): c.Tree = {
    val ts = t.typeSymbol

    if (!ts.isClass) {
      c.abort(c.enclosingPosition, "Can only handle case classes")
    }

    val headTermName = TermName(c.freshName)
    val tailTermName = TermName(c.freshName)

    q"""
       $includeParam.split('.').toList match {
         case _root_.scala.::($headTermName, Nil) =>
           $incMap.isDefinedAt($headTermName)
         case _root_.scala.::($headTermName, $tailTermName) =>
           $incMap.get($headTermName).map(_.includeAllowed($tailTermName.mkString("."))).getOrElse(false)
         case _ =>
           false
       }
     """
  }

  def primaryDataWriter(t: c.Type, objName: TermName): c.Tree = {
    val ts = t.typeSymbol
    if (!ts.isClass) {
      c.abort(c.enclosingPosition, "Can only handle case classes")
    } else if (ts.asClass.isSealed) {
      sumTypePrimaryDataWriter(t, objName)
    } else {
      simpleCaseClassPrimaryDataWriter(t, t, objName)
    }
  }

  def includedWriter(t: c.Type, objName: TermName): c.Tree = {
    val ts = t.typeSymbol
    if (!ts.isClass) {
      c.abort(c.enclosingPosition, "Can only handle case classes")
    } else if (ts.asClass.isSealed) {
      sumTypeIncludedWriter(t, t, objName)
    } else {
      simpleCaseClassIncludedWriter(t, t, objName)
    }
  }

  def createWriter(t: c.Type): c.Tree = {
    val rootParamName = TermName(c.freshName())
    q"""new _root_.com.qvantel.jsonapi.JsonApiWriter[$t] {
          import _root_.com.qvantel.jsonapi.PathJsonFormat
          override final def write($rootParamName: $t, sparseFields: Map[String, List[String]]): _root_.spray.json.JsValue = ${primaryDataWriter(
      t,
      rootParamName)}
          override final def included($rootParamName: $t, sparseFields: Map[String, List[String]]): _root_.scala.collection.immutable.Set[_root_.spray.json.JsObject] = ${includedWriter(
      t,
      rootParamName)}
        }"""
  }
}
