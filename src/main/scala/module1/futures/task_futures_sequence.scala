package module1.futures

import module1.futures.HomeworksUtils.TaskSyntax

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    Future.sequence(futures.map(f => f.transformWith {
      case Failure(exception) => Future(exception)
      case Success(value) => Future(value)
    })).map(list => (
      list.filter(s => !isThrowable(s)).map(s => s.asInstanceOf[A]),
      list.filter(f => isThrowable(f)).map(f => f.asInstanceOf[Throwable]))
    )
  }

  def isThrowable(v: Any): Boolean = v.isInstanceOf[Throwable]
}
