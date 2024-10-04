package deaplearning4j

import deaplearning4j.rnn.CharacterIterator


object CharacterIteratorSample extends App{
  val miniBatchSize = 32                      //Размер батча
  val exampleLength = 1000                    //Длинна примера для обучения, можно увеличивать
  val validCharacters: Array[Char] = CharacterIterator.getMinimalCharacterSet //Which characters are allowed? Others will be removed
  validCharacters.foreach(println)
  //val characterIteratorSample =new CharacterIterator(fileLocation, Charset.forName("UTF-8"), miniBatchSize, sequenceLength, validCharacters, new Random(12345))

}
