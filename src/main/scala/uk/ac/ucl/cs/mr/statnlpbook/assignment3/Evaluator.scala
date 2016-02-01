package uk.ac.ucl.cs.mr.statnlpbook.assignment3

/**
 * @author rockt
 */
object Evaluator {
  def apply(model: Model, corpus: String, iter_p: Int, print: Int): Double = {
    val total = SentimentAnalysisCorpus.numExamples(corpus)
    var correct = 0.0
    for (i <- 0 until total) {
      val (sentence, target) = SentimentAnalysisCorpus.getExample(corpus)
      val predict = model.predict(sentence)
      if (target == predict) correct = correct + 1

      var prediction = 0

      if (predict.equals(true)) {
        prediction = 1
      } else if (predict.equals(false)) {
        prediction = 0
      }

      if (print == 1) {

        val model_p = model.getClass.getSimpleName

        if (corpus =="test" && model_p == "SumOfWordVectorsModel" && iter_p == 29) {
          scala.tools.nsc.io.File(s"./data/assignment3/predictions_model_$model_p-$iter_p.txt").appendAll(s"$prediction\n")
        } else if (corpus =="test" && model_p == "RecurrentNeuralNetworkModel" && iter_p == 34) {
          scala.tools.nsc.io.File(s"./data/assignment3/predictions_model_$model_p-$iter_p.txt").appendAll(s"$prediction\n")
        } else if (corpus =="test" && model_p == "RecurrentNeuralNetworkModelLSTM" && iter_p == 0) {
          scala.tools.nsc.io.File(s"./data/assignment3/predictions_model_$model_p-$iter_p.txt").appendAll(s"$prediction\n")
        }
      }

    }

    correct / total
  }
}
