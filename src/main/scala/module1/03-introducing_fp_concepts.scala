package module1

import java.util.UUID
import scala.annotation.tailrec
import java.time.Instant
import scala.language.postfixOps



/**
 * referential transparency
 */


 object referential_transparency{

  case class Abiturient(id: String, email: String, fio: String)

  type Html = String

  sealed trait Notification

  object Notification{
    case class Email(email: String, text: Html) extends Notification
    case class Sms(telephone: String, msg: String) extends Notification
  }


  case class AbiturientDTO(email: String, fio: String, password: String)

  trait NotificationService{
    def sendNotification(notification: Notification): Unit
    def createNotification(abiturient: Abiturient): Notification
  }


  trait AbiturientService{

    def registerAbiturient(abiturientDTO: AbiturientDTO): Abiturient
  }

}


 // recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n){
      _n *= i
      i += 1
    }
    _n
  }

  def factRec(n: Int): Int = if(n == 0) 1 else n * factRec(n - 1)

  def tailRec(n: Int): Int = {
    @tailrec
    def loop(i: Int, accum: Int): Int = if(i == 0) accum else loop(i - 1, n * accum)
    loop(n, 1)
  }




  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   *
   */


}

object hof{


  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a => {
    val start = System.currentTimeMillis()
    val result = f(a)
    val end = System.currentTimeMillis()
    println(end - start)
    result
  }

  def doomy(string: String) = {
    Thread.sleep(1000)
    println(string)
  }



  // изменение поведения ф-ции

  val arr = Array(1, 2, 3, 4, 5)

  def isOdd(i: Int): Boolean = i % 2 > 0

  val isEven: Int => Boolean = not(isOdd)

  def not[A](f: A => Boolean): A => Boolean = a => !f(a)


  // изменение самой функции

  def sum(x: Int, y: Int): Int = x + y

  val s1: Int => Int = partial(5, sum)

  s1(3) // 8

  def partial[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  def partial2[A, B, C](a: A, f: (A, B) => C): B => C = f.curried(a)
















  trait Consumer{
       def subscribe(topic: String): LazyList[Record]
   }

   case class Record(value: String)

   case class Request()
   
   object Request {
       def parse(str: String): Request = ???
   }

  /**
   *
   * (Опционально) Реализовать ф-цию, которая будет читать записи Request из топика,
   * и сохранять их в базу
   */
   def createRequestSubscription() = ???



}






/**
 *  Реализуем тип Option
 */


 object opt {

  /**
   *
   * Реализовать структуру данных Option, который будет указывать на присутствие либо отсутсвие результата
   */

   // + covariant Option[Animal] родитель для Option[Dog]
   // invariant Option[Animal] нет связи Option[Dog]
   // - contravariant связь наоборот между Option[Animal] и Option[Dog]

   class Animal
   class Dog extends Animal

   def findAnimal: Option[Animal] = ???
   def findDog: Option[Dog] = ???

   def treat(animal: Animal): Unit = ???

   def treat(animal: Option[Animal]): Unit = ???

   val animal: Animal = ???
   val dog: Dog = ???
   treat(animal)
   treat(dog)

  def divide(x: Int, y: Int): Option[Int] = {
    if(y == 0) None
    else Some(x / y)
  }

  sealed trait Option[+T]{

     def isEmpty: Boolean = this match {
       case None => true
       case Some(v) => false
     }

     def get: T = this match {
       case Some(v) => v
       case None => throw new Exception("get on empty Option")
     }

     def map[B](f: T => B): Option[B] = flatMap(v => Option(f(v)))

     def flatMap[B](f: T => Option[B]): Option[B] = this match {
       case Some(v) => f(v)
       case None => None
     }
  }

  val opt: Option[Int] = ???

  val opt2: Option[Int] =  opt.flatMap(i => Option(i + 1))
  val opt3  =  opt.map(i => i + 1)

  case class Some[T](v: T) extends Option[T]
  case object None extends Option[Nothing]

  object Option{
    def apply[T](v: T): Option[T] = Some(v)
  }









  /**
   *
   * Реализовать метод printIfAny, который будет печатать значение, если оно есть
   */


  /**
   *
   * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
   */


  /**
   *
   * Реализовать метод filter, который будет возвращать не пустой Option
   * в случае если исходный не пуст и предикат от значения = true
   */

 }

 object list {
   /**
    *
    * Реализовать односвязанный иммутабельный список List
    * Список имеет два случая:
    * Nil - пустой список
    * Cons - непустой, содердит первый элемент (голову) и хвост (оставшийся список)
    */

    sealed trait List[+T]{

     /**
      * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
      *
      */
     def ::[TT >: T](elem: TT): List[TT] = Cons(elem, this)

     /**
      * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
      *
      */
     def mkString(separator: String): String = {
       @tailrec
       def go(list: List[T], acc: String): String = {
         list match {
           case Cons(head, tail) => go(tail, acc + separator + head)
           case Nil => acc
         }
       }
       go(this, "")
     }

     /**
      *
      * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
      */
     def reverse(): List[T] = {
       @tailrec
       def go(list: List[T], acc: List[T]): List[T] = {
         list match {
           case Cons(head, tail) => go(tail, head :: acc)
           case Nil => acc
         }
       }
       go(this, Nil)
     }

     /**
      *
      * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
      */
     def map[R](f: T => R): List[R] = {
       @tailrec
       def go(list: List[T], acc: List[R]): List[R] = {
         list match {
           case Cons(head, tail) => go(tail, f(head) :: acc)
           case Nil => acc
         }
       }
       go(this, Nil).reverse()
     }

     /**
      *
      * Реализовать метод filter для списка который будет фильтровать список по некому условию
      */
     def filter(predicate: T => Boolean): List[T] = {
       @tailrec
       def go(list: List[T], acc: List[T]): List[T] = {
         list match {
           case Cons(head, tail) => if (predicate(head)) go(tail, head :: acc) else go(tail, acc)
           case Nil => acc
         }
       }
       go(this, Nil).reverse();
     }
   }

    case class Cons[A](head: A, tail: List[A]) extends List[A]
    case object Nil extends List[Nothing]

   object List{

     /**
      * Конструктор, позволяющий создать список из N - го числа аргументов
      * Для этого можно воспользоваться *
      *
      * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
      * def printArgs(args: Int*) = args.foreach(println(_))
      */
     def apply[A](v: A*): List[A] =
       if(v.isEmpty) Nil
       else Cons(v.head, apply(v.tail:_*))

     /**
      *
      * Написать функцию incList котрая будет принимать список Int и возвращать список,
      * где каждый элемент будет увеличен на 1
      */
     def incList(list: List[Int]): List[Int] = list.map(_+1)

     /**
      *
      * Написать функцию shoutString котрая будет принимать список String и возвращать список,
      * где к каждому элементу будет добавлен префикс в виде '!'
      */
     def shoutString(list: List[String]): List[String] = list.map("!"+_)

   }

   Cons(1, Nil) // List(1)
   Cons(1, Cons(2, Nil)) // List(1, 2, 3)
   Cons(1, Cons(2, Cons(3, Nil))) // List()


   // List(1, 2)


 }