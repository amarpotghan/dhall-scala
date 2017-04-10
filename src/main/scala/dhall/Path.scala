package dhall

trait Path {
  def value: String
}

case class Env(value: String) extends Path
case class Url(value: String) extends Path
case class File(value: String) extends Path
