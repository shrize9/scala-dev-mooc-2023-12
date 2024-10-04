package deaplearning4j

import deaplearning4j.rnn.CharacterIterator

import java.nio.charset.Charset
import java.util.Random


object CharacterIteratorSample extends App{
  val miniBatchSize = 32                      //Размер батча
  val exampleLength = 1000                    //Длинна примера для обучения, можно увеличивать
  val validCharacters: Array[Char] = CharacterIterator.getMinimalCharacterSet //Which characters are allowed? Others will be removed
  val characterIteratorSample =new CharacterIterator("/Users/p_kuzmin/IdeaProjects/parserSMM/src/main/resources/textForStatistic/Finansist.txt", Charset.forName("UTF-8"), miniBatchSize, exampleLength, validCharacters, new Random(12345))

  println(characterIteratorSample.totalOutcomes())

}
