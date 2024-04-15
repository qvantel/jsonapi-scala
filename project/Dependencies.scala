import sbt._

object Dependencies extends AutoPlugin {

  val `pekko-actor`   = "org.apache.pekko" %% "pekko-actor"   % "1.0.2"
  val `pekko-slf4j`   = "org.apache.pekko" %% "pekko-slf4j"   % `pekko-actor`.revision
  val `pekko-stream`  = "org.apache.pekko" %% "pekko-stream"  % `pekko-actor`.revision
  val `pekko-testkit` = "org.apache.pekko" %% "pekko-testkit" % `pekko-actor`.revision
  //
  val `pekko-http`            = "org.apache.pekko" %% "pekko-http"            % "1.0.1"
  val `pekko-http-spray-json` = "org.apache.pekko" %% "pekko-http-spray-json" % `pekko-http`.revision
  val `pekko-http-core`       = "org.apache.pekko" %% "pekko-http-core"       % `pekko-http`.revision
  val `pekko-http-testkit`    = "org.apache.pekko" %% "pekko-http-testkit"    % `pekko-http`.revision

}
