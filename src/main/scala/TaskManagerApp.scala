import scala.annotation.tailrec

/*
Вы разрабатываете программу для управления задачами в списке дел. Сначала вам необходимо реализовать односвязный список, который будет хранить задачи. Каждая задача имеет название и приоритет.
Затем реализуйте методы для управления задачами: добавление задачи в список, удаление задачи из списка и вывод всех задач. Из списка всегда удаляется наиболее приоритетная задача, вывод задач также отсортирован по приоритету.
Формат ввода
Одна строка, которая содержит команды и их параметры (если есть). Команды разделены через точку с запятой, параметры — через запятую. Команды могут быть следующими:
1. ADD,название_задачи,приоритет_задачи — добавление новой задачи в список;
2. REMOVE — удаление наиболее приоритетной задачи из списка. Если несколько задач имеют одинаковый приоритет, удаляется первая по алфавиту задача. Если задач в списке нет, ничего не происходит.
3. Набор команд гарантированно заканчивается командой GET — она выводит все задачи, которые остались в списке, по порядку приоритета.
Название задачи содержит только буквы кириллицей, без пробелов и иных символов. Каждое название, хранящееся в списке, уникально. Приоритет задачи — это целое число от 1 до 5, где 1 — наиболее приоритетные, 5 — наименее приоритетные задачи.
Длина строки — не более 500 символов.
Формат вывода
Одна строка — все задачи, которые остались в списке, через точку с запятой. Строка имеет вид: «название_задачи,приоритет_задачи;название_задачи,приоритет_задачи;...». Задачи отсортированы от наиболее к наименее приоритетным (от 1 до 5), задачи с одним приоритетом отсортированы в алфавитном порядке.
Если в списке не осталось задач, выводится «Список пуст» (без кавычек).
Пример 1
Входные данные:
ADD,НаписатьКод,2;ADD,ТестироватьКод,3;ADD,ОтветитьНаСообщения,1;REMOVE;GET
Выходные данные:
НаписатьКод,2;ТестироватьКод,3
Пример 2
Входные данные:
REMOVE;ADD,КупитьПродукты,3;REMOVE;ADD,СделатьУборку,2;ADD,Постирать,5;ADD,Погладить,5;GET
Выходные данные:
СделатьУборку,2;Погладить,5;Постирать,5
Пример 3
Входные данные:
ADD,ПосетитьВстречу,2;REMOVE;ADD,ПрочитатьГазету,1;REMOVE;GET
Выходные данные:
Список пуст
* */

//DATA
sealed trait TaskManagerCommand
sealed trait TaskManagerExecuter{
  def apply(tasks:List[TaskManagerCommand]):List[TaskManagerCommand]
}

case object NIL extends TaskManagerCommand
case class ADD(name:String, priority:Int) extends TaskManagerCommand
case object REMOVE extends TaskManagerCommand with TaskManagerExecuter{
  override def apply(tasks: List[TaskManagerCommand]): List[TaskManagerCommand] = {
    if(tasks.size >0) tasks.tail else Nil
  }
}
case object GET extends TaskManagerCommand with TaskManagerExecuter{
  override def apply(tasks: List[TaskManagerCommand]): List[TaskManagerCommand] = if (tasks.size !=0){
    tasks.foreach{
      case ADD(name, priority) =>
        print(s"${name},${priority};")
    }
    println()
    tasks
  }else{
    println("Список пуст")
    tasks
  }
}

case class TaskManager(value:TaskManagerCommand, next:TaskManager)

//OPERATIONS
object TaskManagerCommandOps{
  def parse(command:String, delimiterParameters:String=","):Either[Exception, TaskManagerCommand] = command.split(delimiterParameters).toList match {
    case "ADD" :: name :: priority :: Nil => Right(ADD(name, priority.toInt))
    case "REMOVE" :: Nil => Right(REMOVE)
    case "GET" :: Nil => Right(GET)
    case commands => Left(new Exception(s"error ${commands}"))
  }
}

object TaskManagerOps{
   import TaskManagerCommandOps._

   def parseLine(line:String, delimiter:String=";"):TaskManager = {
     def _parseLine(strCommands:List[String]):TaskManager=strCommands match {
       case Nil => TaskManager(NIL, null)
       case strCommand :: Nil => TaskManager(parse(strCommand).toOption.get, null)
       case strCommand :: tails => TaskManager(parse(strCommand).toOption.get, _parseLine(tails))
     }
     _parseLine(line.split(delimiter).toList)
   }

   implicit class ImplTaskManagerOps(taskManager: TaskManager){
     def sorted(accum:List[TaskManagerCommand])={ accum.collect { case add:ADD => add }.sortBy { case ADD(name, priority) => (priority, name)}}

     def execute(): Unit = {
       @tailrec
       def _execute(taskManager: TaskManager, accum:List[TaskManagerCommand]):List[TaskManagerCommand] = taskManager match {
         case null => accum
         case TaskManager(NIL, null) => accum
         case TaskManager(add:ADD, next:TaskManager) => _execute(next, sorted(add :: accum))
         case TaskManager(executer:TaskManagerExecuter, null) => executer(accum)
         case TaskManager(executer:TaskManagerExecuter, next:TaskManager) => _execute(next, executer(accum))
       }

       _execute(taskManager, Nil)
     }

     def forEach(callback:TaskManagerCommand=>Unit)= if(taskManager !=null) {
       def visit(current: TaskManager): Unit = if(current !=null) {current match {
         case TaskManager(command, null) => callback(command)
         case TaskManager(command, next) => {
           callback(command); visit(next)
         }
       }}

       callback(taskManager.value)
       visit(taskManager.next)
     }
   }
}

object TaskManagerApp extends App {
  import TaskManagerOps._
  val sample ="ADD,НаписатьКод,2;ADD,ТестироватьКод,3;ADD,ВернутьДокументы,1;ADD,ОтветитьНаСообщения,1;REMOVE;GET"
  println("INPUT:" +sample)
  val taskManager =parseLine(sample)
  taskManager.execute()

  val sample1 ="GET"
  println("\nINPUT:" +sample1)
  val taskManagerGET =parseLine(sample1)
  taskManagerGET.execute()

  val sample2 ="REMOVE;ADD,КупитьПродукты,3;REMOVE;ADD,СделатьУборку,2;ADD,Постирать,5;ADD,Погладить,5;GET"
  println("\nINPUT:" +sample2)
  val taskManager2 =parseLine(sample2)
  taskManager2.execute()

  val sample3 ="ADD,ПосетитьВстречу,2;REMOVE;ADD,ПрочитатьГазету,1;REMOVE;GET"
  println("\nINPUT:" +sample3)
  val taskManager3 =parseLine(sample3)
  taskManager3.execute()

}
