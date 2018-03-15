package com.qvantel.jsonapi

import cats.effect.IO
import com.netaporter.uri.Uri
import com.qvantel.jsonapi.ApiEndpoint.Config

trait ApiEndpoint {
  def config: IO[Config]
}

object ApiEndpoint {
  final case class Config(uri: Uri, headers: Map[String, String])

  final case class Static(static: Uri, headers: Map[String, String] = Map()) extends ApiEndpoint {
    override val config: IO[Config] = IO.pure(Config(static, headers))
  }
  final case class Dynamic(config: IO[Config]) extends ApiEndpoint
}
