package monad

import monad.MonadParser.indexMonad

class Outter(private val p:String=>String){
  def view(in:String)={
    println("start")
    println(p(in))
    println("end")
  }
}

object Outter{
  def apply(p:String=>String)=new Outter(p)
}


class MonadParser[T,Src](private val p: Src=>(T,Src)){
  def flatMap[M](f:T=>MonadParser[M,Src]):MonadParser[M,Src] = MonadParser{
    _src=>
      indexMonad +=1
      val (head, tail) = p(_src)
      val mn =f(head)
      println(s"get word ${head} source ${_src} index ${indexMonad}")
      mn.p(tail)
  }

  def map[M](f:T=>M):MonadParser[M,Src] = MonadParser{
    _src=>
      val (word, src) = p(_src)
      (f(word),src)
  }

  def parse(src: Src):T = {println(s"calling parse ${src} -- ${this}"); p(src)._1}
}

object MonadParser{
  var indexMonad:Int =0

  def apply[T, Src](p: Src=>(T,Src)) = new MonadParser(p)
}

object MonadParserExample extends App {

  def StringField = MonadParser[String,String]{
    line=>
      val idx =line.indexOf(";")
      if(idx > -1)
        (line.substring(0, idx), line.substring(idx +1))
      else
        (line, "")
  }

  def IntField = StringField.map(_.toInt)
  def BooleanField = StringField.map(_.toBoolean)

  case class Car(year:Int, mark:String, model:String, canDrive:Boolean)
  val data ="1997;Ford;F;true\n2000;VV;Passat;false"

  val sParser = for{
    year <- IntField
    mark <- StringField
  } yield (year, mark)
  println(sParser.parse("1997;Ford;F;true"))

  val parser = for{
    year <- IntField
    mark <- StringField
    model <- StringField
    canDrive <- BooleanField
  } yield Car(year, mark, model, canDrive)

  println(
    data.split("\n").map(parser.parse(_)).mkString("\n")
  )

  val _outter=Outter{
    in=> in + "!"
  }

  _outter.view("sdd")
}
