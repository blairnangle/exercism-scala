package helloworld

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class HelloWorldTest extends AnyFunSuite with Matchers {

  test("Say Hi!") {
    HelloWorld.hello() should be("Hello, World!")
  }
}
