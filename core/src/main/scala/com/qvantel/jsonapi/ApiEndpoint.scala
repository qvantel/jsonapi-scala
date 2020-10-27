package com.qvantel.jsonapi

import cats.effect.IO
import io.lemonlabs.uri.Url
import com.qvantel.jsonapi.ApiEndpoint.Config

trait ApiEndpoint {
  def config: IO[Config]
}

object ApiEndpoint {
  final case class Config(url: Url, headers: Map[String, String])

  final case class Static(static: Url, headers: Map[String, String] = Map()) extends ApiEndpoint {
    override val config: IO[Config] = IO.pure(Config(static, headers))
  }
  final case class Dynamic(config: IO[Config]) extends ApiEndpoint
}
