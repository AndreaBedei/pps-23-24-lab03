object FullFile:
  
    // Task part 1
    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()

    def mapWithFlatMap[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = 
      flatMap(l)(a => Cons(mapper(a), Nil()))

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()

    def filterwithFlatmap[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = 
      val m = (v: A) => v match
        case v if pred(v) => Cons(v, Nil())
        case _ => Nil()
      flatMap(l1)(m)
      
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(f, s), Cons(f2, s2)) => Cons((f, f2), zip(s, s2))
      case _ => Nil()
    
    def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
      case Cons(h, t) if n > 0 =>  Cons(h, take(t)(n - 1))
      case _ => Nil()
    
    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = (l1, l2) match
      case (Cons(h, t), l2) => Cons(h, concat(t, l2))
      case (Nil(), Cons(h, t)) => Cons(h, concat(t, Nil()))
      case _ => Nil()
    
    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
      case Nil() => Nil()
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper)) 
    
    def min(l: Sequence[Int]): Optional[Int] = l match
      case Cons(h, t) if Optional.orElse(min(t), Int.MaxValue) > h => Optional.Just(h)
      case Cons(h, t) if Optional.orElse(min(t), Int.MaxValue) < h => min(t)
      case Nil() => Optional.Empty()

    // Task part 2
    extension [A, B](l: Sequence[A]) def foldLeft(first: B)(op: (B, A) => B): B = l match 
      case Cons(h, t) => t.foldLeft(op(first, h))(op)
      case Nil()      => first

    extension (people: Sequence[Person]) def getCoursesOfTeachers: Sequence[String] =
      map(filter(people)(isTeacher))(p => p match
      case Person.Teacher(_, c) => c
      )

    def isTeacher(p: Person): Boolean = p match
      case Teacher(_, _) => true
      case _ => false
    
    // Task 3

    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
      case _ => Empty()

    def fill[A](n: Int)(element: A): Stream[A] = Stream.take(Stream.iterate(element)(x => x))(n)

    val pell: Stream[Int] = 
      def pellNumber(n: Int): Int = n match 
        case 0 => 0
        case 1 => 1
        case _ => 2 * pellNumber(n - 1) + pellNumber(n - 2)
      Stream.map(Stream.iterate(0)(_ + 1))(pellNumber)
