package deaplearning4j

import ai.djl._
import ai.djl.repository.zoo._
import ai.djl.huggingface.tokenizers.HuggingFaceTokenizer
import ai.djl.ndarray.NDList
import ai.djl.ndarray.types.DataType
import ai.djl.translate.{Translator, TranslatorContext}

import java.nio.file.{Path, Paths}


object DJLgetModelFromHuggFaceApp extends App {
  // Указываем использование CPU явно
  System.setProperty("ai.djl.default_engine", "OnnxRuntime")
  System.setProperty("ai.djl.onnxruntime.use_gpu", "false") // Убеждаемся, что используем CPU
  // Загрузка модели GPT-2
  val model = Model.newInstance("gpt2",Device.cpu())
  val criteria = Criteria.builder
    .setTypes(classOf[String], classOf[String])
    .optModelPath(Paths.get("/Users/p_kuzmin/IdeaProjects/scala-dev-mooc-2023-12/models/nanoFialka-v1/model.onnx"))
    .optDevice(Device.cpu())
    .optEngine("OnnxRuntime") // Используем ONNX Runtime
    .build

  // Токенизатор для обработки текста
  val tokenizer = HuggingFaceTokenizer.newInstance(Paths.get("/Users/p_kuzmin/IdeaProjects/scala-dev-mooc-2023-12/models/nanoFialka-v1/model.onnx"))

  // Функция для генерации текста
  def generateText(prompt: String): String = {
    val translator = new Translator[String, String] {
      override def processInput(ctx: TranslatorContext, input: String): NDList = {
        val tokenized = tokenizer.encode(input)
        new NDList(ctx.getNDManager.create(tokenized.getIds))
      }

      override def processOutput(ctx: TranslatorContext, list: NDList): String = {
        val outputIds = list.head().toType(DataType.INT32, false).toArray
        tokenizer.decode(outputIds.map(_.longValue()).toArray[Long]).strip()
      }
    }

    val predictor = model.newPredictor(translator)
    try {
      predictor.predict(prompt)
    } finally {
      predictor.close()
    }
  }

  // Использование модели
  val prompt = "Расскажи о будущем Искуственного интелекта"
  val generatedText = generateText(prompt)
  println(s"Generated text: ${prompt} ${generatedText}")

  // Закрытие токенизатора
  model.close()
  tokenizer.close()
}
