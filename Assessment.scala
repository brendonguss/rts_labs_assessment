// Copy the entire contents of this file, go to https://scastie.scala-lang.org/, paste it into the editor and click "Save",
// this will execute the test suite of the assessment.

/**
  * #3 If you could change 1 thing about your favorite framework/language/platform (pick one), what would it be?
  * 
  * I could change one thing about one of my favorite languages, Scala, I would remove some of the implicit conversion functionality
  * so that Scala code that uses it will become easier to read.
  */


object Assessment {
  case class NumResult(numAbove: Int, numBelow: Int)
  
  def numAboveBelow(nums: List[Int], num: Int): NumResult = {
    if (nums == null) {
      throw new Exception("The list of numbers cannot be null!")
    }
    
    var numBelow = 0
    var numAbove = 0
    
    nums.foreach{n =>
      if (n > num) {
        numAbove += 1
      } else if (n < num) {
        numBelow += 1
      }
    }
    NumResult(numAbove, numBelow)
  }

  def printNumAboveBelow(nums: List[Int], num: Int): Unit = {
    val numResult = numAboveBelow(nums, num)
    println(s"above: ${numResult.numAbove}, below: ${numResult.numBelow}")
  }

  def rotateString(str: String, rotation: Int): String = {
    if (str == null) {
      throw new Exception("The string must not be null!")
    }

    if (rotation < 0) {
      throw new Exception("Rotation value cannot be less than 0!")
    }

    val trueRotation = if (str.length > 0) rotation % str.length else 0

    if (trueRotation == 0) {
      str
    } else {
      str.substring(str.length - trueRotation, str.length) ++ str.dropRight(trueRotation)
    }
  }

}

// Tests

import Assessment._

//Rotate String Tests

println(rotateString("", 0) == "")
println(rotateString("MyString", 2) == "ngMyStri")
println(rotateString("MyString", 8) == "MyString")
println(rotateString("MyString", 0) == "MyString")
println(rotateString("MyString", 16) == "MyString")
println(rotateString("MyString", 10) == "ngMyStri")

//Number Above & Below a Number Tests

println(numAboveBelow(List(1, 5, 2, 1, 10), 6) == NumResult(1, 4))
println(numAboveBelow(List(), 0) == NumResult(0, 0))
println(numAboveBelow(List(1), 0) == NumResult(1, 0))
println(numAboveBelow(List(1, 2, 3), 0) == NumResult(3, 0))
