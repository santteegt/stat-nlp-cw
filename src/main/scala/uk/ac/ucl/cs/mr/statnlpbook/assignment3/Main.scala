package uk.ac.ucl.cs.mr.statnlpbook.assignment3

/**
 * @author rockt
 */
object Main extends App {
  /**
   * Example training of a model
   *
   * Problems 2/3/4: perform a grid search over the parameters below
   */
  val learningRate = 0.01 //big value yield less epochs
  val vectorRegularizationStrength = 0.01 // best value until now
  val matrixRegularizationStrength = 0.0
  val wordDim = 10
  val hiddenDim = 10

  val trainSetName = "train"
  //val trainSetName = "debug"
  val validationSetName = "test"

  val model: Model = new SumOfWordVectorsModel(wordDim, vectorRegularizationStrength)
//  val model: Model = new RecurrentNeuralNetworkModel(wordDim, hiddenDim, vectorRegularizationStrength, matrixRegularizationStrength)
  //val model: Model = new RecurrentNeuralNetworkModelDropout(wordDim, hiddenDim, vectorRegularizationStrength, matrixRegularizationStrength)
//  val model: Model = new RecurrentNeuralNetworkModelLSTM(wordDim, hiddenDim, vectorRegularizationStrength, matrixRegularizationStrength)

//  model.loadFixedWordVectors(wordDim) //pre-catch vector weights using the word2vec file

  def epochHook(iter: Int, accLoss: Double): Unit = {
    println("Epoch %4d\tLoss %8.4f\tTrain Acc %4.2f\tDev Acc %4.2f".format(
      iter, accLoss, 100 * Evaluator(model, trainSetName, iter_p = iter, print = 0), 100 * Evaluator(model, validationSetName, iter_p = iter, print = 0)))
  }

  val timer = System.currentTimeMillis()
  StochasticGradientDescentLearner(model, trainSetName, 100, learningRate, epochHook)
  val timeElapsed = (System.currentTimeMillis() - timer)/1000.00
  println("Process time elapsed: %.2f seconds".format(timeElapsed))

  /**
   * Comment this in if you want to look at trained parameters
   */

  /*
  val weight = model.vectorParams("param_w")
  for ((paramName, paramBlock) <- model.vectorParams) {
    if (!paramName.equals("param_w")) {
        val new_weight = Sigmoid(Dot(weight, paramBlock)).forward()
        println(s"${paramName} \t ${paramBlock.param} \t $new_weight)\n")
    }
  }
  for ((paramName, paramBlock) <- model.matrixParams) {
    println(s"$paramName:\n${paramBlock.param}\n")
  }
  */

}