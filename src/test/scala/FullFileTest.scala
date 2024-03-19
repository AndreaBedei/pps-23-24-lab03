object FullFileTest:

    // Task 1 => test già implementati da voi

    // Task 2
    class TeacherTest:
        @Test def testTeacherCourses() = 
            val persons = Sequence.Cons(
            Person.Teacher("John", "Math"),
            Cons(Person.Student("Alice", 2010), Cons(Person.Teacher("Bob", "Physics"), Nil()))
            )
            val expectedCourses = Cons("Math", Cons("Physics", Nil()))
            assertEquals(expectedCourses, persons.getCoursesOfTeachers)

    class FoldLeftTest:
        @Test def testFoldLeft() = 
            val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))

            val sumResult = foldLeft(lst)(0)(_ + _)
            assertEquals(16, sumResult)

            val productResult = foldLeft(lst)(1)(_ * _)
            assertEquals(105, productResult)

            val stringConcatResult = foldLeft(Cons("Hello", Cons(" ", Cons("world", Nil()))))("")(_ + _)
            assertEquals("Hello world", stringConcatResult)

    // Task 3
    class PellNumberTest:
        @Test def testFirstFivePellNumbers() = 
            val expected = Cons(0, Cons(1, Cons(2, Cons(5, Cons(12, Nil())))))
            val actual = u03.Streams.Stream.toList(u03.Streams.Stream.take(pell)(5))
            assertEquals(expected, actual)

    class FillFunctionTest:
        @Test def testFillWithInt() = 
            val filledStream = fill(3)(10)
            assertEquals( Cons(10, Cons(10, Cons(10, Nil()))), u03.Streams.Stream.toList(filledStream))

        @Test def testFillWithString() =
            val filledStream = fill(3)("Hello")
            assertEquals( Cons("Hello", Cons("Hello", Cons("Hello", Nil()))), u03.Streams.Stream.toList(filledStream))
