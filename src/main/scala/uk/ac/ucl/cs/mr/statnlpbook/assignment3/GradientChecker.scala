package uk.ac.ucl.cs.mr.statnlpbook.assignment3

import breeze.linalg.{QuasiTensor, TensorLike, sum}
import breeze.numerics._

/**
 * Problem 1
 */
object GradientChecker extends App {
  val EPSILON = 1e-6

  /**
   * For an introduction see http://cs231n.github.io/neural-networks-3/#gradcheck
   *
   * This is a basic implementation of gradient checking.
   * It is restricted in that it assumes that the function to test evaluates to a double.
   * Moreover, another constraint is that it always tests by backpropagating a gradient of 1.0.
   */
  def apply[P](model: Block[Double], paramBlock: ParamBlock[P]) = {
    paramBlock.resetGradient()
    model.forward()
    model.backward(1.0)

    var avgError = 0.0

    val gradient = paramBlock.gradParam match {
      case m: Matrix => m.toDenseVector
      case v: Vector => v
    }

    /**
     * Calculates f_theta(x_i + eps)
     * @param index i in x_i
     * @param eps value that is added to x_i
     * @return
     */
    def wiggledForward(index: Int, eps: Double): Double = {
      var result = 0.0
      paramBlock.param match {
        case v: Vector =>
          val tmp = v(index)
          v(index) = tmp + eps
          result = model.forward()
          v(index) = tmp
        case m: Matrix =>
          val (row, col) = m.rowColumnFromLinearIndex(index)
          val tmp = m(row, col)
          m(row, col) = tmp + eps
          result = model.forward()
          m(row, col) = tmp
      }
      result
    }

    for (i <- 0 until gradient.activeSize) {
      //todo: your code goes here!
      val gradientExpected: Double = (wiggledForward(i, EPSILON) - wiggledForward(i, -1*EPSILON))/(2*EPSILON) //todo: ???

      avgError = avgError + math.abs(gradientExpected - gradient(i))

      assert(
        math.abs(gradientExpected - gradient(i)) < EPSILON,
        "Gradient check failed!\n" +
          s"Expected gradient for ${i}th component in input is $gradientExpected but I got ${gradient(i)}"
      )
    }

    println("Average error: " + avgError)
  }

  /**
    * A very silly block to test if gradient checking is working.
    * Will only work if the implementation of the Dot block is already correct
    */
  val a = vec(-1.5, 1.0, 1.5, 0.5)
  val b = VectorParam(4)
  b.set(vec(1.0, 2.0, -0.5, 2.5))
  val c = VectorParam(4)
  c.set(vec(2.0, 1.0, -2.5, 0.5))
  //val simpleBlock = Dot(a, b) //Check implementation of DOT
  //GradientChecker(simpleBlock, b) //Check for Blocks
  val d = Sum(Seq(b, c))
  //val simpleBlock = Dot(a, d) //Check implementation of DOT and SUM
  //GradientChecker(simpleBlock, d) //Check for Blocks
  val e = DoubleConstant(0.45)
  //val simpleBlock = Sigmoid(e) //Check implementation of SIGMOID


  //val simpleBlock = ReLU(b) //Check implementation of SIGMOID
  //GradientChecker(simpleBlock, b) //Check for Blocks


  //val simpleBlock = NegativeLogLikelihoodLoss(e, 1) //Check implementation of NEGATIVE LOG LIKELIHOOD
  //val simpleBlock = L2Regularization(0.003, b, c) //Check implementation of L2 REGULARISATION on vectors
  //val simpleBlock = Dot (a, Tanh(b)) //Check implementation of TANH

  val (sentence, target) = SentimentAnalysisCorpus.getExample("train") //Check the implementation of SUMOFWORDSVECTOR model
  val model = new SumOfWordVectorsModel(10, 0)
//  val wordVectors = sentence.map(model.wordToVector)
//  val sentenceVector = model.wordVectorsToSentenceVector(wordVectors)
  //val predict = model.predict(sentence)

//  val simpleBlock = model.loss(sentence, target)
//  GradientChecker(simpleBlock, model.vectorParams.head._2) //Check for models
  //GradientChecker(simpleBlock, model.vectorParams("param_w")) //Check for models


  val W = MatrixParam(2, 3)
  W.set( mat(2,3)(1.0, -2.0, 3.0, -4.0, 5.0, -6.0) )
  //val simpleBlock = L2Regularization(0.03, W)
  //GradientChecker(simpleBlock, W) //Check for Blocks

  val const = VectorParam(3)
  const.set(vec(-1.5, 1.0, 1.5))

  val param = Mul(W, const)
  //val simpleBlock = Dot(vec(5, -1), param)
  //GradientChecker(simpleBlock, W) //Check for Blocks

  val tanh =Tanh(param)
  val param2 = VectorParam(tanh.forward().activeSize)
  param2.set(tanh.output)
//  val simpleBlock  = Dot(vec(5, -1), param2)
//  GradientChecker(simpleBlock, param2) //Check for Blocks

  val model2 = new RecurrentNeuralNetworkModel(10, 10, 0, 0) // Check the implementation of RNN model
//  val simpleBlock = model2.loss(sentence, target)
//  GradientChecker(simpleBlock, model.vectorParams.head._2) //Check for models
  //GradientChecker(simpleBlock, model.vectorParams("param_w")) //Check for models


  val rr = model.vectorParams.head._2
  val dropout = Dropout(0.5, rr, false)
  //val simpleBlock = Dot(dropout, rr) //Check implementation of SIGMOID
  //GradientChecker(simpleBlock, rr)

//  val param3 = VectorParam(dropout.forward().activeSize)
//  param3.set(dropout.output)
//  val simpleBlock  = Dot(model.vectorParams("param_w"), param3)
//  GradientChecker(simpleBlock, param3) //Check for Blocks

//  val mulv = MulV(a, b) //Check implementation of MULV
//  val simpleBlock = Dot(mulv, b)
//    GradientChecker(simpleBlock, b) //Check for Blocks

  val sigmoidv = SigmoidV(b) //Check implementation of SIGMOIDV
  val simpleBlock = Dot(sigmoidv, c)
  GradientChecker(simpleBlock, c) //Check for Blocks

}
