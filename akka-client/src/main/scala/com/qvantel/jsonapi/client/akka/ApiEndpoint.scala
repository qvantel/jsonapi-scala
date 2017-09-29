package com.qvantel.jsonapi.client.akka

import com.netaporter.uri.Uri
import monix.eval.Task

trait ApiEndpoint {
  def uri: Task[Uri]
}

object ApiEndpoint {
  final case class Static(static: Uri) extends ApiEndpoint {
    override val uri: Task[Uri] = Task.now(static)
  }
  final class Dynamic(f: () => Task[Uri]) extends ApiEndpoint {
    override def uri: Task[Uri] = f()
  }
}
