package xylophone.test

import rapture.test.TestSuite
import xylophone._, backends.stdlib._

import scala.collection.immutable.ListMap

object SplittingTests extends TestSuite {

  val seq1 = xml"<a/><b/><c/><d/><e/>"

  val `Splitting head` = test {
    seq1 match {
      case head ~: _ => head.toString
    }
  }.returns("<a/>")

  val `Splitting tail` = test {
    seq1 match {
      case _ ~: tail => tail.toString
    }
  }.returns("<b/><c/><d/><e/>")

  val `Splitting init` = test {
    seq1 match {
      case init :~ _ => init.toString
    }
  }.returns("<a/><b/><c/><d/>")

  val `Splitting last` = test {
    seq1 match {
      case _ :~ last => last.toString
    }
  }.returns("<e/>")

}

