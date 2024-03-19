package u03

import org.junit.Test
import org.junit.Assert._
import u03.Sequences.Sequence._
import u03.Streams.Stream.fill
import u03.Streams.Stream._

class FillFunctionTest:

    @Test def testFillWithInt() = 
        val filledStream = fill(3)(10)
        assertEquals( Cons(10, Cons(10, Cons(10, Nil()))), u03.Streams.Stream.toList(filledStream))

    @Test def testFillWithString() =
        val filledStream = fill(3)("Hello")
        assertEquals( Cons("Hello", Cons("Hello", Cons("Hello", Nil()))), u03.Streams.Stream.toList(filledStream))