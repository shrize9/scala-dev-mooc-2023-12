package deaplearning4j.datetimeformatlabel

import org.deeplearning4j.datasets.iterator.{DataSetIteratorSplitter, IteratorDataSetIterator, MultiDataSetIteratorSplitter}

import java.nio.charset.Charset
import java.util.{Date, Random}
import org.deeplearning4j.nn.api.OptimizationAlgorithm
import org.deeplearning4j.nn.conf.layers.{CnnLossLayer, ConvolutionLayer, DenseLayer, OutputLayer, SubsamplingLayer}
import org.deeplearning4j.nn.conf.{BackpropType, MultiLayerConfiguration, NeuralNetConfiguration, Updater}
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork
import org.deeplearning4j.nn.weights.WeightInit
import org.deeplearning4j.optimize.listeners.ScoreIterationListener
import org.nd4j.evaluation.classification.{Evaluation, ROC}
import org.nd4j.linalg.activations.Activation
import org.nd4j.linalg.dataset.DataSet
import org.nd4j.linalg.dataset.api.preprocessor.NormalizerStandardize
import org.nd4j.linalg.dataset.api.preprocessor.serializer.NormalizerSerializer
import org.nd4j.linalg.factory.Nd4j
import org.nd4j.linalg.learning.config.{Adam, Sgd}
import org.nd4j.linalg.learning.regularization.{L2Regularization, Regularization, WeightDecay}
import org.nd4j.linalg.lossfunctions.LossFunctions
import org.nd4j.linalg.lossfunctions.LossFunctions.LossFunction

import scala.jdk.CollectionConverters._
import scala.collection.mutable

object TestDateFormatterModel extends App {

  val dtIterator    =new DateGeneratorDataSet(new Date (2017-1900, 1, 1), new Date(2023 -1900,12,31), 60000, 60000)
  println(s"${dtIterator.inputColumns()}")
  println(s"${dtIterator.totalOutcomes()}")
  val lstmLayerSize =30
  //Настройка нейронной сети:
  val regularizations:List[Regularization] =( new L2Regularization(0.001) :: Nil)
  val conf: MultiLayerConfiguration = new NeuralNetConfiguration.Builder()
    .optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT)
    .updater(new Adam(0.001))
    .seed(12345)
    .weightInit(WeightInit.XAVIER)
    .regularization(regularizations.asJava)
    .list
    .layer(0, new DenseLayer.Builder().nIn(dtIterator.inputColumns()).nOut(lstmLayerSize)
      .activation(Activation.RELU)
      .build())
    .layer(1, new DenseLayer.Builder().nIn(lstmLayerSize).nOut(11)
      .activation(Activation.RELU).build())
    .layer(2, new DenseLayer.Builder().nIn(11).nOut(dtIterator.totalOutcomes())
      .activation(Activation.RELU).build())
    .layer(3, new OutputLayer.Builder()
      .activation(Activation.SOFTMAX)
      .nIn(dtIterator.totalOutcomes()).nOut(dtIterator.totalOutcomes()).build())
    .build()

  val allData =dtIterator.next()
  allData.shuffle()
  val splitIterator =allData.splitTestAndTrain(0.75)
  val train =splitIterator.getTrain
  val test =splitIterator.getTest
  val normalizer = new NormalizerStandardize()
  normalizer.fit(train)
  normalizer.transform(train)
  normalizer.transform(test)

  val net = new MultiLayerNetwork(conf)
  net.init()
  net.setListeners(new ScoreIterationListener(1))

  val layers = net.getLayers
  var totalNumParams = 0L
  for (i <- layers.indices) {
    val nParams = layers(i).numParams
    println("Number of parameters in layer " + i + ": " + nParams)
    totalNumParams = totalNumParams + nParams
  }
  println("Total number of network parameters: " + totalNumParams)

  val trainIter = new IteratorDataSetIterator({train :: Nil}.toIterator.asJava, 100)
  while(trainIter.hasNext)
    net.fit(trainIter.next())

  val testIter = new IteratorDataSetIterator({test :: Nil}.toIterator.asJava, 32)
  val eval:org.deeplearning4j.eval.Evaluation = net.evaluate(testIter)
  println(eval.accuracy())
  println(eval.confusionToString())

  dtIterator.sample().foreach{
    case (dt, label) => {
      val tensor =dtIterator.tensor(dt)
      normalizer.transform(tensor)
      val result =net.output(tensor)
      println(s"$dt - and output ${Nd4j.argMax(result, 1)}(${dtIterator.getLabels.get(Nd4j.argMax(result, 1).getInt(0))})  == $label is format (${dtIterator.getLabels.get(label)})")
    }
  }
}
