package u03

import u02.AnonymousFunctions.l
import u03.Optionals.Optional
import u02.Modules.isTeacher
import u02.Modules.Person

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

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
      
    // Lab 03
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

    extension [A, B](l: Sequence[A]) def foldLeft(first: B)(op: (B, A) => B): B = l match 
      case Cons(h, t) => t.foldLeft(op(first, h))(op)
      case Nil()      => first
    
@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 60

  import Sequence.*
  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
  val lst = Cons (3, Cons(7, Cons(1, Cons(5, Nil()))))
  println(foldLeft ( lst ) (0) ( _ - _ ))
