package xylophone.test

import xylophone._, backends.stdlib._

import rapture.test._

object AccessTests extends TestSuite {
  val sample = xml"""<a><b>1</b><c>2</c><xs><x id="1"><b>3</b><d>4</d></x><x id="2"><b>5</b><d>6</d></x><x><b>7</b><d>8</d></x></xs></a>"""

  val `Get root node` = test {
    sample().toString
  }.returns(sample.toString)

  val `Get root node element seq` = test {
    sample().b.toString
  }.returns("<b>1</b>")

  val `Get root node element` = test {
    sample().b().toString
  }.returns("<b>1</b>")

  val `Get root node children` = test {
    sample().*.toString
  }.returns("""<b>1</b><c>2</c><xs><x id="1"><b>3</b><d>4</d></x><x id="2"><b>5</b><d>6</d></x><x><b>7</b><d>8</d></x></xs>""")

  val `Get root seq children` = test {
    sample.*.toString
  }.returns("""<b>1</b><c>2</c><xs><x id="1"><b>3</b><d>4</d></x><x id="2"><b>5</b><d>6</d></x><x><b>7</b><d>8</d></x></xs>""")

  val `Get root seq node` = test {
    sample.b.toString
  }.returns("""<b>1</b><c>2</c><xs><x id="1"><b>3</b><d>4</d></x><x id="2"><b>5</b><d>6</d></x><x><b>7</b><d>8</d></x></xs>""")
}
