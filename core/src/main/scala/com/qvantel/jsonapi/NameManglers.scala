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

object NameManglers {
  private[this] val Acronyms      = "([A-Z]{2,})([A-Z][a-z0-9]+)".r
  private[this] val AcronymsAtEnd = "([A-Z]{2,})$".r
  private[this] val Camels        = "([A-Z])([a-z0-9]+)".r

  private[this] def capitalizeAcronyms(s: String): String = {
    val s1 = Acronyms.replaceAllIn(s, { m =>
      s"${m.group(1).toLowerCase.capitalize}${m.group(2)}"
    })
    AcronymsAtEnd.replaceAllIn(s1, { m =>
      m.group(1).toLowerCase.capitalize
    })
  }

  private[this] def camelToSeparated(s: String, separator: Char): String =
    Camels.replaceAllIn(s, { m =>
      val u = if (m.start == 0 || s(m.start - 1) == separator) "" else s"$separator"
      s"$u${m.group(1).toLowerCase}${m.group(2)}"
    })

  def separated(separator: Char): NameMangler = { (s: String) =>
    if (s.forall(_.isUpper)) {
      s.toLowerCase
    } else {
      val noDots      = s.replace(".", "")
      val acros       = capitalizeAcronyms(noDots)
      val downInitial = s"${acros(0).toLower}${acros.tail}"
      camelToSeparated(downInitial, separator)
    }
  }

  val snakeCase: NameMangler  = separated('_')
  val dasherized: NameMangler = separated('-')

  @inline def noNameMangling(s: String): String = identity(s)
}
