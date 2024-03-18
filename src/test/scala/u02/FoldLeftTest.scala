package u02

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u03.Sequences.Sequence
import u03.Sequences.Sequence.* 

class FoldLeftTest:

  @Test def testFoldLeft() = 
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))

    val sumResult = foldLeft(lst)(0)(_ + _)
    assertEquals(16, sumResult)

    val productResult = foldLeft(lst)(1)(_ * _)
    assertEquals(105, productResult)

    val stringConcatResult = foldLeft(Cons("Hello", Cons(" ", Cons("world", Nil()))))("")(_ + _)
    assertEquals("Hello world", stringConcatResult)
  
