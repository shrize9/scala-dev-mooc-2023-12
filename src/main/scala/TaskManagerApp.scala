
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
case object NIL extends TaskManagerCommand
case object REMOVE extends TaskManagerCommand
case object GET extends TaskManagerCommand
case class ADD(name:String, priority:Int) extends TaskManagerCommand
case class TaskManager(value:TaskManagerCommand, next:TaskManager) extends TaskManagerCommand

//OPERATIONS
object TaskManagerCommandOps{
  def parse(command:String, delimiterParameters:String=","):TaskManagerCommand = command.split(delimiterParameters).toList match {
    case "ADD" :: name :: priority :: Nil => ADD(name, priority.toInt)
    case "REMOVE" :: Nil => REMOVE
    case "GET" :: Nil => GET
  }
}

object TaskManagerOps{
   import TaskManagerCommandOps._

   def parseLine(line:String, delimiter:String=";"):TaskManager = {
     def _parseLine(strCommands:List[String]):TaskManager=strCommands match {
       case Nil => TaskManager(NIL, null)
       case strCommand :: Nil => TaskManager(parse(strCommand), null)
       case strCommand :: tails => TaskManager(parse(strCommand), _parseLine(tails))
     }
     _parseLine(line.split(delimiter).toList)
   }

   def forEach(taskManager: TaskManager)(callback:TaskManagerCommand=>Unit)= if(taskManager !=null) {
      def visit(current: TaskManager): Unit = current match {
        case _current@TaskManager(_, _) if _current == null =>
        case TaskManager(command, null) => callback(command)
        case TaskManager(command, next) => {
          callback(command); visit(next)
        }
      }

      callback(taskManager.value)
      visit(taskManager.next)
   }
}

object TaskManagerApp extends App {
    import TaskManagerOps._
    val sample ="ADD,НаписатьКод,2;ADD,ТестироватьКод,3;ADD,ОтветитьНаСообщения,1;REMOVE;GET"

    val taaskManager =parseLine(sample)
    forEach(taaskManager)(println)
}
