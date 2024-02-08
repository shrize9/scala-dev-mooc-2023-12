package module2

object higher_kinded_types{

  def tuple[A, B](a: List[A], b: List[B]): List[(A, B)] =
    a.flatMap{ a => b.map((a, _))}

  def tuple[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
    a.flatMap{ a => b.map((a, _))}

  def tuple[E, A, B](a: Either[E, A], b: Either[E, B]): Either[E, (A, B)] =
    a.flatMap{ a => b.map((a, _))}


  trait Bindable[F[_], A] {
    def map[B](f: A => B): F[B]
    def flatMap[B](f: A => F[B]): F[B]
  }

  def tupleBindable[F[_], A, B](fa: Bindable[F, A], fb: Bindable[F, B]): F[(A, B)]  =
    fa.flatMap(a => fb.map((a, _)))

   def optBindable[A](opt: Option[A]): Bindable[Option, A] =
    new Bindable[Option, A] {
      override def map[B](f: A => B): Option[B] = opt.map(f)

      override def flatMap[B](f: A => Option[B]): Option[B] = opt.flatMap(f)
    }

   def listBindable[A](opt: List[A]): Bindable[List, A] =
    new Bindable[List, A] {
      override def map[B](f: A => B): List[B] = opt.map(f)

      override def flatMap[B](f: A => List[B]): List[B] = opt.flatMap(f)
    }


  object implicits_tupleF{

    def tuplef[F[_], A, B](fa: F[A], fb: F[B])(implicit functor:Functor[F]): F[(A, B)] =
      functor.flatMap(fa, (a:A)=> functor.map(fb, (b:B) => (a, b)))

    trait Functor[F[_]] {
      def map[A,B](x: F[A], f: A => B): F[B]
      def flatMap[A,B](x: F[A], f: A => F[B]): F[B]
    }

    implicit object optionFunctor extends Functor[Option] {
      override def map[A,B](x: Option[A], f: A => B): Option[B] = x map f
      override def flatMap[A,B](x: Option[A], f: A => Option[B]): Option[B] = x flatMap f
    }

    implicit object listFunctor extends Functor[List] {
      override def map[A,B](x: List[A], f: A => B): List[B] = x map f
      override def flatMap[A,B](x: List[A], f: A => List[B]): List[B] = x flatMap f
    }

  }





  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)

  lazy val r3 = println(tupleBindable(optBindable(optA), optBindable(optB)))
  lazy val r4 = println(tupleBindable(listBindable(list1), listBindable(list2)))

  import implicits_tupleF._
  println(tuplef(optA, optB))
  println(tuplef(list1, list2))

}