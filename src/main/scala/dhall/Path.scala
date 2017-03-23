package dhall

trait Path {
  val value: String
}

case class Env(value: String) extends Path
case class Url(value: String) extends Path
