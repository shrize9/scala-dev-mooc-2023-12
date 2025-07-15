package deaplearning4j

import ai.onnxruntime.OrtEnvironment
import ai.onnxruntime.OnnxTensor
import ai.onnxruntime.OrtException
import ai.onnxruntime.OrtSession

import java.nio.file.Paths
import java.util.Collections
import java.util


object ONNXMaskedLanguageModelExample extends App {
  try {
    // Создаем OrtEnvironment
    val env = OrtEnvironment.getEnvironment
    // Открываем сессию для инференса
    val session = env.createSession(Paths.get("/Users/p_kuzmin/IdeaProjects/scala-dev-mooc-2023-12/models/COLD2/onnx/model.onnx").toString)
    // Подготовка входных данных
    // Предполагаем, что модель ожидает тензоры в формате Long для input_ids
    // и Float для attention_mask
    val input_ids = Array(Array(101L, 2057L, 4L, 102L)) // Пример входных данных для маскированной модели, где 103 - это [MASK]

    val attention_mask = Array(Array(1L, 1L, 1L, 1L)) // Маска внимания

    val inputs = new util.HashMap[String,OnnxTensor]()
    inputs.put("input_ids", OnnxTensor.createTensor(env, input_ids))
    inputs.put("attention_mask", OnnxTensor.createTensor(env, attention_mask))
    // Инференс
    val output = session.run(inputs)
    // Обработка выхода
    val logits = output.get(0).getValue.asInstanceOf[Array[Array[Array[Float]]]]
    // Ваш код для интерпретации выхода, например, выбора наиболее вероятного токена
    import collection.JavaConverters._
    logits.foreach{
      case arr1=> arr1.foreach{
        case arr2=> {
          println(s"variant: ${arr2.length}")
          println(arr2.take(10).mkString(","))
        }
      }
    }
    // Закрываем ресурсы
    for (tensor <- inputs.asScala.values) {
      tensor.close
    }
    output.close
    session.close
    env.close
  } catch {
    case e: Throwable =>
      e.printStackTrace
  }
}
