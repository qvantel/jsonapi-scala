package com.qvantel.jsonapi

import io.lemonlabs.uri.Url
import io.lemonlabs.uri.typesafe.dsl._

/**
  * Case class for handling the top level pagination links in collection responses
  *
  * @param originalUrl The URL (relative or absolute) of the original incoming request, complete with all query parameters
  */
case class JsonApiPagination(originalUrl: Url,
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
  def allLinksAsUrls: Map[String, Url] =
    Map(
      "first" -> buildPaginationUrl(firstParams),
      "last"  -> buildPaginationUrl(lastParams),
      "prev"  -> buildPaginationUrl(prevParams),
      "next"  -> buildPaginationUrl(nextParams)
    ).flatMap { case (name, maybeUrl) => maybeUrl.map(url => name -> url) }

  private[this] def buildPaginationUrl(pageParams: Map[String, String]): Option[Url] =
    if (pageParams.isEmpty)
      None
    else
      Some(
        originalUrl
          .filterQueryNames(!_.startsWith("page["))
          .addParams(pageParams.map { case (k, v) => s"page[$k]" -> v }))
}

object JsonApiPagination {
  type PaginationFunc = Long => JsonApiPagination
  val EmptyFunc: PaginationFunc = _ => Empty

  val Empty = new JsonApiPagination("")
}
