package uk.ac.ucl.cs.mr.statnlpbook.assignment3

/**
 * @author rockt
 */
object Evaluator {
  def apply(model: Model, corpus: String, model_p: Int, iter_p: Int, print: Int = 0): Double = {
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
        if (model_p == 1 & iter_p == 53) {
          scala.tools.nsc.io.File(s"./data/assignment3/predictions_model_$model_p-$iter_p.txt").appendAll(s"$prediction\n")
        } else if (model_p == 2 && iter_p == 71) {
          scala.tools.nsc.io.File(s"./data/assignment3/predictions_model_$model_p-$iter_p.txt").appendAll(s"$prediction\n")
        } else if (model_p == 3 && iter_p == 91) {
          scala.tools.nsc.io.File(s"./data/assignment3/predictions_model_$model_p-$iter_p.txt").appendAll(s"$prediction\n")
        }
      }

    }

    correct / total
  }
}
