package xylophone.test

import rapture.test._

class TestProgramme extends Programme {
  include(AccessTests)
  include(SplittingTests)
  include(JoiningTests)
  include(SerializationTests)
  //include(ParsingTests)
}
