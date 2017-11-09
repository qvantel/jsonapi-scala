package com.qvantel.jsonapi.client.akka

import com.qvantel.jsonapi.{ToMany, ToOne, jsonApiResource}
import spray.json.DefaultJsonProtocol._

@jsonApiResource final case class BillingAccount(id: String,
                                                 accountId: String,
                                                 taxRegion: String,
                                                 name: Option[String],
                                                 customerAccount: ToOne[CustomerAccount])
@jsonApiResource final case class CustomerAccount(id: String, billingAccounts: ToMany[BillingAccount])
