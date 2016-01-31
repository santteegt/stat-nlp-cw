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
  //val learningRate = 0.01
  //val vectorRegularizationStrength = 0.01
  val learningRate = 0.01 //big value yield less epochs
  val vectorRegularizationStrength = 0.01 // best value until now
  val matrixRegularizationStrength = 0.0
  val wordDim = 10
  val hiddenDim = 10

  val trainSetName = "train"
  //val trainSetName = "debug"
  val validationSetName = "dev"

  //val model: Model = new SumOfWordVectorsModel(wordDim, vectorRegularizationStrength)
  //val model: Model = new RecurrentNeuralNetworkModel(wordDim, hiddenDim, vectorRegularizationStrength, matrixRegularizationStrength)
  val model: Model = new OurOwnModel(wordDim, hiddenDim, vectorRegularizationStrength, matrixRegularizationStrength)

  def epochHook(iter: Int, accLoss: Double): Unit = {
    println("Epoch %4d\tLoss %8.4f\tTrain Acc %4.2f\tDev Acc %4.2f".format(
      iter, accLoss, 100 * Evaluator(model, trainSetName, model_p = 0, iter_p = 0, print = 0), 100 * Evaluator(model, validationSetName, model_p = 1, iter_p = iter, print = 0)))
  }

  val timer = System.currentTimeMillis()
  StochasticGradientDescentLearner(model, trainSetName, 100, learningRate, epochHook)
  val timeElapsed = (System.currentTimeMillis() - timer)/1000.00
  println("Process time elapsed: %.2f seconds".format(timeElapsed))

  /**
   * Comment this in if you want to look at trained parameters
   */

  /*
  for ((paramName, paramBlock) <- model.vectorParams) {
    scala.tools.nsc.io.File("./data/assignment3/tsne.txt").appendAll(s"${paramName.take(5000)}, ${paramBlock.param}\n")

  }
  for ((paramName, paramBlock) <- model.matrixParams) {
    println(s"$paramName:\n${paramBlock.param}\n")
  }
  */

}