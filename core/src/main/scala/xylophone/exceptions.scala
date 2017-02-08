package xylophone

case class ParseException(source: String, line: Option[Int] = None, column: Option[Int] = None)
  extends Exception("Failed to parse source")
