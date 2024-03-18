package u02

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u02.Modules._
import u03.Sequences.Sequence.* 
import u03.Sequences.*
import u02.Modules.Person

class ModulesTest:

  @Test def testTeacherCourses() = 
    val persons = Sequence.Cons(
      Person.Teacher("John", "Math"),
      Cons(Person.Student("Alice", 2010), Cons(Person.Teacher("Bob", "Physics"), Nil()))
    )
    val expectedCourses = Cons("Math", Cons("Physics", Nil()))
    assertEquals(expectedCourses, persons.getCoursesOfTeachers)



    
