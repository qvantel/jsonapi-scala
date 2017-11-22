package com.qvantel.jsonapi

import cats.effect.IO
import com.netaporter.uri.Uri

trait ApiEndpoint {
  def uri: IO[Uri]
}

object ApiEndpoint {
  final case class Static(static: Uri) extends ApiEndpoint {
    override val uri: IO[Uri] = IO.pure(static)
  }
  final class Dynamic(f: () => IO[Uri]) extends ApiEndpoint {
    override def uri: IO[Uri] = f()
  }
}
