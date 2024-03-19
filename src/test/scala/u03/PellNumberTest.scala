package u03

import org.junit.Test
import org.junit.Test
import org.junit.Assert._
import u03.Sequences.Sequence._
import u03.Streams.Stream.fill
import u03.Streams.Stream._


class PellNumberTest:
    
  @Test def testFirstFivePellNumbers() = 
    val expected = Cons(0, Cons(1, Cons(2, Cons(5, Cons(12, Nil())))))
    val actual = u03.Streams.Stream.toList(u03.Streams.Stream.take(pell)(5))
    assertEquals(expected, actual)
