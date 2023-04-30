package twofer

/** @version 1.2.0 */
class TwoferTest extends munit.FunSuite {

  test("no name given") {
    assertEquals(Twofer.twofer(), "One for you, one for me.")
  }

  test("a name given") {
    assertEquals(Twofer.twofer("Alice"), "One for Alice, one for me.")
  }

  test("another name given") {
    assertEquals(Twofer.twofer("Bob"), "One for Bob, one for me.")
  }
}
