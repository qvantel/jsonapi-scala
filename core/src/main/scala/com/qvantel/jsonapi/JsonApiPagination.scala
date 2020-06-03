package com.qvantel.jsonapi

import com.netaporter.uri.Uri

/**
  * Case class for handling the top level pagination links in collection responses
  *
  * @param originalUri The URI (relative or absolute) of the original incoming request, complete with all query parameters
  */
case class JsonApiPagination(originalUri: Uri,
                             private val firstParams: Map[String, String] = Map.empty,
                             private val lastParams: Map[String, String] = Map.empty,
                             private val prevParams: Map[String, String] = Map.empty,
                             private val nextParams: Map[String, String] = Map.empty) {

  /**
    * Adds a 'first' link to the response
    * @param pageParams One or more tuples of pagination kind and value, e.g "size" -> "10" or "number" -> "2"
    */
  def withFirst(pageParams: (String, String)*): JsonApiPagination =
    this.copy(firstParams = pageParams.toMap)

  /**
    * Adds a 'last' link to the response
    * @param pageParams One or more tuples of pagination kind and value, e.g "size" -> "10" or "number" -> "2"
    */
  def withLast(pageParams: (String, String)*): JsonApiPagination =
    this.copy(lastParams = pageParams.toMap)

  /**
    * Adds a 'prev' link to the response
    * @param pageParams One or more tuples of pagination kind and value, e.g "size" -> "10" or "number" -> "2"
    */
  def withPrev(pageParams: (String, String)*): JsonApiPagination =
    this.copy(prevParams = pageParams.toMap)

  /**
    * Adds a 'next' link to the response
    * @param pageParams One or more tuples of pagination kind and value, e.g "size" -> "10" or "number" -> "2"
    */
  def withNext(pageParams: (String, String)*): JsonApiPagination =
    this.copy(nextParams = pageParams.toMap)

  /**
    * Returns all non empty pagination links in a map
    */
  def allLinksAsUris: Map[String, Uri] =
    Map(
      "first" -> buildPaginationUri(firstParams),
      "last"  -> buildPaginationUri(lastParams),
      "prev"  -> buildPaginationUri(prevParams),
      "next"  -> buildPaginationUri(nextParams)
    ).flatMap { case (name, maybeUri) => maybeUri.map(uri => name -> uri) }

  private[this] def buildPaginationUri(pageParams: Map[String, String]): Option[Uri] =
    if (pageParams.isEmpty)
      None
    else
      Some(
        originalUri
          .filterQueryNames(!_.startsWith("page["))
          .addParams(pageParams.map { case (k, v) => s"page[$k]" -> v }))
}

object JsonApiPagination {
  val Empty = new JsonApiPagination(Uri.empty)
}
