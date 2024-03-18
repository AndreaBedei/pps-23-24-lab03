package u02

import u03.Sequences.*
import u03.Sequences.Sequence.* 

object Modules extends App :

  // An ADT: type + module
  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:
    def name(p: Person): String = p match
      case Student(n, _) => n
      case Teacher(n, _) => n

  extension (people: Sequence[Person]) def getCoursesOfTeachers: Sequence[String] =
    map(filter(people)(isTeacher))(p => p match
      case Person.Teacher(_, c) => c
    )
    
  println(Person.name(Person.Student("mario", 2015)))

  import Person.*

  println(name(Student("mario", 2015)))

  // a method outside the Person module
  def isTeacher(p: Person): Boolean = p match
    case Teacher(_, _) => true
    case _ => false

  // a method outside the Person module
  def isStudent(p: Person): Boolean = p match
    case Student(_, _) => true
    case _ => false

  println(isStudent(Student("mario", 2015)))