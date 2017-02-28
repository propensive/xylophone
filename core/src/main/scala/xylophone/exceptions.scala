package xylophone

/** represents a failure during parsing
 *
 *  @param source  the source in which the parsing error occurred
 *  @param line    the line the error was on, if any
 *  @param column  the column the error was on, if any */
case class ParseException(source: String, line: Option[Int] = None, column: Option[Int] = None)
  extends Exception("Failed to parse source")
