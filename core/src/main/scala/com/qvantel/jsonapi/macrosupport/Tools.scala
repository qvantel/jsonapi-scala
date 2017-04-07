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
package com.qvantel.jsonapi.macrosupport

import scala.annotation.compileTimeOnly
import scala.reflect.macros.blackbox.Context
import shapeless.{CNil, Coproduct}


@compileTimeOnly("Macros can only be used at compile-time")
trait Tools {
  val c: Context
  import c.universe._

  private[this] val coproductType = typeOf[Coproduct]
  private[this] val coproductNil = typeOf[CNil]

  private[this] def inl(name: TermName): c.Tree = {
    val bind = pq"$name"
    q"_root_.shapeless.Inl($bind)"
  }

  private[this] def inr(tree: c.Tree): c.Tree = q"_root_.shapeless.Inr($tree)"

  def coproductTypes(t: c.Type): List[c.Type] = {
    def go(ta: List[c.Type], acc: List[c.Type]): List[c.Type] = {
      ta match {
        case Nil => acc
        case l :: r :: Nil if r =:= coproductNil => acc :+ l
        case l :: r :: Nil => go(r.dealias.typeArgs, acc :+ l)
      }
    }

    if (t <:< coproductType) {
      go(t.dealias.typeArgs, Nil)
    } else {
      c.abort(c.enclosingPosition, s"The type $t must be a shapeless.Coproduct")
    }
  }

  def coproductPattern(n: Int, name: TermName): c.Tree = {
    (1 until n).foldLeft(inl(name))((tree, _) => inr(tree))
  }

  def coproductPatterns(nTypes: Int, name: TermName): Seq[c.Tree] = {
    (1 to nTypes).map(coproductPattern(_, name))
  }
}
