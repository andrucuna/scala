package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton 1")
      assert(contains(s2, 2), "Singleton 2")
      assert(contains(s3, 3), "Singleton 3")
      assert(!contains(s1, 0), "Not in singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  
  test("intersect contains mutual elements") {
    new TestSets {
      val s = union( union(s1, s2), s3 )
      val i = intersect( s, s2 )
      assert(contains(i, 2), "Intersect 1")
      assert(!contains(i, 1), "Intersect 2")
      assert(!contains(i, 3), "Intersect 3")
    }
  }
  
  test("diff contains elements in s, that are not in t") {
    new TestSets {
      val s = union( union(s1, s2), s3 )
      val d = diff( s, s2 )
      assert(!contains(d, 2), "Diff 1")
      assert(contains(d, 1), "Diff 2")
      assert(contains(d, 3), "Diff 3")
    }
  }
  
  test("filter returns the subset of `s` for which `p` holds.") {
    new TestSets {
      val s = union( union(s1, s2), s3 )
      val p = ( x: Int ) => x > 1
      val f = filter( s, p )
      
      assert(!contains(f, 1), "Filter 1")
      assert(contains(f, 2), "Filter 2")
      assert(contains(f, 3), "Filter 3")
    }
  }
   
  test("forall returns whether all bounded integers within `s` satisfy `p`") {
    new TestSets {
      val s = union(s1, s3)
      val p = ( x: Int ) => x%2 == 1
      
      assert(forall(s, p), "Forall")
      assert(!forall(s2, p), "Not forall")
    }
  }
   
  test("exists returns whether there exists a bounded integer within `s` that satisfies `p`.") {
    new TestSets {
      val s = union( union(s1, s2), s3 )
      val p = ( x: Int ) => x%2 == 0
      val q = ( x: Int ) => x > 1000
      
      assert(exists(s, p), "Exists")
      assert(!exists(s, q), "Not exists")
    }
  }
  
  test("map returns a set transformed by applying `f` to each element of `s`.") {
    new TestSets {
      val s = union( union(s1, s2), s3 )
      val f = ( x: Int ) => x+10
      val mapped = map( s, f )
      
      assert(contains(mapped, 11), "Map 1")
      assert(contains(mapped, 12), "Map 2")
      assert(contains(mapped, 13), "Map 3")
      assert(!contains(mapped, 1), "Map 4")
      assert(!contains(mapped, 2), "Map 5")
      assert(!contains(mapped, 3), "Map 6")
    }
  }
}
