package module2

object higher_kinded_types{

  def tuple[A, B](a: List[A], b: List[B]): List[(A, B)] =
    a.flatMap{ a => b.map((a, _))}

  def tuple[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
    a.flatMap{ a => b.map((a, _))}

  def tuple[E, A, B](a: Either[E, A], b: Either[E, B]): Either[E, (A, B)] =
    a.flatMap{ a => b.map((a, _))}


  def tuplef[F[_] : Bindable, A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
    val bindable = implicitly[Bindable[F]]
    bindable.flatMap(fa)(a => bindable.map(fb)((a, _)))
  }


  trait Bindable[F[_]] {
    def map[A, B] (fa:F[A])(f: A => B): F[B]
    def flatMap[A, B] (fa: F[A])(f: A => F[B]): F[B]
  }

  /*
  def tupleBindable[F[_], A, B](fa: Bindable[F, A], fb: Bindable[F, B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b)))

  def optBindable[A](opt: Option[A]): Bindable[Option, A] = new Bindable[Option, A] {
    override def map[B](f: A => B): Option[B] = opt.map(f)

    override def flatMap[B](f: A => Option[B]): Option[B] = opt.flatMap(f)
  }

  def listBindable[A](list: List[A]): Bindable[List, A] = ???
  */



  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)

  implicit def listToBindable: Bindable[List] = {
    new Bindable[List] {
      override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

      override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    }
  }

  val list1: List[Int] = List(1, 2, 3)
  val list2: List[Int] = List(4, 5, 6)
  val tupleList = tuplef(list1, list2)

  implicit def optToBindable: Bindable[Option] = {
    new Bindable[Option] {
      override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    }
  }

  val opt1: Option[Int] = Some(1)
  val opt2: Option[Int] = Some(2)
  val tupleOpt = tuplef(opt1, opt2)



  /*
  val r3: Option[(Int, Int)] = tupleBindable(optBindable(optA), optBindable(optB))
  val r4 = println(tupleBindable(listBindable(list1), listBindable(list2)))


  lazy val r1 = println(tuplef(optA, optB))
  lazy val r2 = println(tuplef(list1, list2))
  */

}