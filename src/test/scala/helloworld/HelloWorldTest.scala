package helloworld

import helloworld.HelloWorld

class HelloWorldTest extends munit.FunSuite {
  test("Say Hi!") {
    assertEquals(HelloWorld.hello(), "Hello, World!")
  }
}
