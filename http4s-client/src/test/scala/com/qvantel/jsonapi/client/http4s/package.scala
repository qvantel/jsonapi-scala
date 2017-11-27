package com.qvantel.jsonapi.client

import com.qvantel.jsonapi.ApiRoot

package object http4s {
  implicit val apiroot: ApiRoot = ApiRoot.empty
}
