package module1

import module1.Printer.Formatter

import java.io.PrintStream


case class Printer(private val out:PrintStream){
  def print[A](value:A)(implicit frmt:Formatter[A]) : Unit = {
    out.println(frmt(value))
  }
}

object Printer{
  type Formatter[A] = A => String

  implicit val Formatter_String = new Formatter[String] {
    override def apply(v1: String): String = v1
  }
  implicit val Formatter_Int = new Formatter[Int] {
    override def apply(v1: Int): String = v1.toString
  }

  implicit val Formatter_None:Formatter[None.type] = new Formatter[None.type] {
    override def apply(v1: None.type): String ="null"
  }
  implicit def Formatter_Option[A, C[A] <: Option[A]](implicit frm: Formatter[A]): Formatter[C[A]] = new Formatter[C[A]] {
    override def apply(v1: C[A]): String = v1.map(frm(_)).getOrElse("null")
  }

  def apply():Printer ={
    new Printer(System.out)
  }
}

case class Person(val id:Int, val name:String, val surname:Option[String]=None)

object Person{
  implicit def formatter_Peson(implicit frmtString:Formatter[String], frmtInt:Formatter[Int], frmtstrOpt:Formatter[Option[String]]):Formatter[Person] = new Formatter[Person]{
    override def apply(v1: Person): String = {
      frmtString("[") + frmtInt(v1.id) + "," +frmtString(v1.name) +"," +frmtstrOpt(v1.surname) + frmtString("]")
    }
  }
}

object PrinterFormatter extends App{

  import Printer._
  import Person._

  Printer().print("hello")
  Printer().print(Some("hello"))
  Printer().print(10)
  Printer().print(Some(10))
  Printer().print(None)
  Printer().print(Person(10,"sss"))
  Printer().print(Person(10,"sss",Some("kuzmin")))
}
