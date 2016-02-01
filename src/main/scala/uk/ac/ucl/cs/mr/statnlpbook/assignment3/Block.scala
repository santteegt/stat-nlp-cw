package uk.ac.ucl.cs.mr.statnlpbook.assignment3

import breeze.numerics.{log, sigmoid, tanh, pow, sqrt, cosh}
import breeze.stats.distributions.Bernoulli

//yes, you will need them ;)
import breeze.linalg.{DenseMatrix => Matrix, DenseVector => Vector, clip}

/**
 * @author rockt
 */

/**
 * A trait for the core building **block** of our computation graphs
  *
  * @tparam T the type parameter this block evaluates to (can be Double, Vector, Matrix)
 */
trait Block[T] {
  //caches output after call of forward
  var output: T = _
  //fun fact: when you say "forward" or "back" your lips move in the respective direction
  def forward(): T
  //assumes that forward has been called first!
  def backward(gradient: T): Unit
  //updates parameters of the block
  def update(learningRate: Double)
}

/**
 * A loss function is a block that evaluates to a single double.
 * Generally loss functions don't have upstream gradients,
 * so we can provide an implementation of backward without arguments.
 */
trait Loss extends Block[Double] {
  def backward(): Unit
}

trait ParamBlock[P] extends Block[P] {
  var param: P
  val gradParam: P
  def initialize(dist: () => Double): P
  def set(p: P): Unit = {
    param = p
  }
  def resetGradient(): Unit
}

trait DefaultInitialization {
  def defaultInitialization(): Double
}

/**
 * This trait defines a default way of initializing weights of parameters
 */
trait GaussianDefaultInitialization extends DefaultInitialization {
  def defaultInitialization(): Double = random.nextGaussian() * 0.1
  def xavierInitialization(mean: Double, variance: Double): Double = mean + (random.nextGaussian() * variance)
//  def xavierInitialization(mean: Double, variance: Double): Double = mean + (random.nextDouble() * variance)
}

trait BernoulliInitialization extends DefaultInitialization {
  val Bernoulli: breeze.stats.distributions.Bernoulli
  def defaultInitialization(): Double = if(Bernoulli.draw()) 1.0 else 0.0
}

/**
 * A simple block that represents a constant double value
  *
  * @param arg the constant double value
 */
case class DoubleConstant(arg: Double) extends Block[Double] with Loss {
  output = arg
  def forward(): Double = output
  def backward(gradient: Double): Unit = {} //nothing to do since fixed
  def update(learningRate: Double): Unit = {} //nothing to do since fixed and no child blocks
  def backward(): Unit = {} //nothing to do since fixed
}

/**
 * A simple block that represents a constant vector
  *
  * @param arg the constant vector
 */
case class VectorConstant(arg: Vector) extends Block[Vector] {
  output = arg
  def forward(): Vector = output
  def backward(gradient: Vector): Unit = {} //nothing to do since fixed
  def update(learningRate: Double): Unit = {} //nothing to do since fixed and no child blocks
}

/**
 * A block representing a sum of doubles
  *
  * @param args a sequence of blocks that evaluate to doubles
 */
case class DoubleSum(args: Block[Double]*) extends Block[Double] {
  def forward(): Double = {
    output = args.map(_.forward()).sum
    output
  }
  def backward(gradient: Double): Unit = args.foreach(_.backward(gradient))
  def update(learningRate: Double): Unit = args.foreach(_.update(learningRate))
}

class LossSum(override val args: Loss*) extends DoubleSum(args:_*) with Loss {
  def backward(): Unit = args.foreach(_.backward())
}



/**
 * Problem 2
 */

/**
 * A block representing a vector parameter
  *
  * @param dim dimension of the vector
 * @param clip defines range in which gradients are clipped, i.e., (-clip, clip)
 */
case class VectorParam(dim: Int, clip: Double = 10.0) extends ParamBlock[Vector] with GaussianDefaultInitialization {
  var param: Vector = initialize(defaultInitialization) //todo: initialize using default initialization
//  var param: Vector = xavierInitialization()
  val gradParam: Vector = Vector.zeros[Double](dim) //todo: initialize with zeros
  /**
   * @return the current value of the vector parameter and caches it into output
   */
  def forward(): Vector = {
    output = param
    output
  }
  /**
   * Accumulates the gradient in gradParam
    *
    * @param gradient an upstream gradient
   */
  def backward(gradient: Vector): Unit = gradParam :+= gradient
  /**
   * Resets gradParam to zero
   */
  def resetGradient(): Unit = { //todo:
    gradParam :*= Vector.zeros[Double](gradParam.activeSize)
  }
  /**
   * Updates param using the accumulated gradient. Clips the gradient to the interval (-clip, clip) before the update
    *
    * @param learningRate learning rate used for the update
   */
  def update(learningRate: Double): Unit = {
    param :-= (breeze.linalg.clip(gradParam, -clip, clip) :* learningRate) //in-place
    resetGradient()
  }
  /**
   * Initializes the parameter randomly using a sampling function
    *
    * @param dist sampling function
   * @return the random parameter vector
   */
  def initialize(dist: () => Double): Vector = {
    param = randVec(dim, dist)
    param
  }

  def xavierInitialization(): Vector = {
    param = vec((0 until dim).map(i => xavierInitialization(0.0, (1.0/dim))):_*)
//    param = vec((0 until dim).map(i => xavierInitialization(0.0, (1.0/scala.math.sqrt(dim)))):_*)
//    param = vec((0 until dim).map(i => xavierInitialization(0.0, (scala.math.sqrt(6)/scala.math.sqrt(dim*2)))):_*)
    param
  }


}

/**
 * A block representing the sum of vectors
  *
  * @param args a sequence of blocks that evaluate to vectors
 */
case class Sum(args: Seq[Block[Vector]]) extends Block[Vector] {
  def forward(): Vector = { //todo: ???
    output = Vector.zeros[Double](args.head.forward().activeSize)
    args.foreach(x => output :+= x.forward())
    output
  }
  def backward(gradient: Vector): Unit = args.foreach(_.backward(gradient)) //todo: ???
  def update(learningRate: Double): Unit = args.foreach(_.update(learningRate)) //todo: ???
}

/**
 * A block representing the dot product between two vectors
  *
  * @param arg1 left block that evaluates to a vector
 * @param arg2 right block that evaluates to a vector
 */
case class Dot(arg1: Block[Vector], arg2: Block[Vector]) extends Block[Double] {
  def forward(): Double = { //todo: ???
    output = arg1.forward() dot arg2.forward()
    output
  }
  def backward(gradient: Double): Unit = {
    //val upstreamGradientVector = Vector.ones[Double](arg1.output.activeSize) * gradient
    arg1.backward( gradient * arg2.output ) //todo: ???
    arg2.backward( gradient * arg1.output )
  }
  def update(learningRate: Double): Unit = {  //todo: ???
    arg1.update(learningRate)
    arg2.update(learningRate)
  }
}

/**
 * A block representing the sigmoid of a scalar value
  *
  * @param arg a block that evaluates to a double
 */
case class Sigmoid(arg: Block[Double]) extends Block[Double] {

  def forward(): Double = { //todo: ???
    output = sigmoid(arg.forward())
    output
  }
  def backward(gradient: Double): Unit = { //todo: ???
    val derivative = sigmoid(arg.output) * (1 - sigmoid(arg.output))
    arg.backward( gradient * derivative )
  }
  def update(learningRate: Double): Unit = { //todo: ???
    arg.update(learningRate)
  }
}

/**
 * A block representing the negative log-likelihood loss
  *
  * @param arg a block evaluating to a scalar value
 * @param target the target value (1.0 positive sentiment, 0.0 negative sentiment)
 */
case class NegativeLogLikelihoodLoss(arg: Block[Double], target: Double) extends Loss {
  def forward(): Double = { //todo: ???
    output = ( -target*log(arg.forward()) ) - ( (1-target) * (log(1-arg.forward())) )
    output
  }
  //loss functions are root nodes so they don't have upstream gradients
  def backward(gradient: Double): Unit = backward()
  def backward(): Unit = { //todo: ???
    arg.backward( ( target - arg.output ) / ( arg.output * (arg.output - 1) ) )
  }
  def update(learningRate: Double): Unit = { } //todo: ???
}

/**
 * A block representing the l2 regularization of a vector or matrix
  *
  * @param strength the strength of the regularization (often denoted as lambda)
 * @param args a block evaluating to a vector or matrix
 * @tparam P type of the input block (we assume this is Block[Vector] or Block[Matrix]
 */
case class L2Regularization[P](strength: Double, args: Block[P]*) extends Loss {
  def forward(): Double = {
    /**
     * Calculates the loss individually for every vector/matrix parameter in args
     */
    val losses = args.map(arg => {
      val in = arg.forward()
      in match {
        case v: Vector => (strength/2)*pow( breeze.linalg.norm(v) , 2) //todo: ???
        case w: Matrix => (strength/2)*pow( breeze.linalg.norm(w.toDenseVector) , 2) //todo: ??? w.toDenseVector
      }
    })
    output = losses.sum//todo: ??? //sums the losses up
    output
  }
  def update(learningRate: Double): Unit = args.map(_.update(learningRate)) //todo: ???
  //loss functions are root nodes so they don't have upstream gradients
  def backward(gradient: Double): Unit = backward()
  def backward(): Unit = args.foreach(x => x.backward((x.forward() match { //todo: ???
    case v: Vector => strength * v
    case w: Matrix => strength * w
  }).asInstanceOf[P]))
}



/**
 * Problem 3
 */

/**
 * A block representing a matrix parameter
  *
  * @param dim1 first dimension of the matrix
 * @param dim2 second dimension of the matrix
 * @param clip defines range in which gradients are clipped, i.e., (-clip, clip)
 */
case class MatrixParam(dim1: Int, dim2: Int, clip: Double = 10.0) extends ParamBlock[Matrix] with GaussianDefaultInitialization {

  var param: Matrix = initialize(defaultInitialization) //todo: ???
//  var param: Matrix = xavierInitialization
  val gradParam: Matrix = Matrix.zeros[Double](dim1, dim2) //todo: ???

  def forward(): Matrix = { //todo: ???
    output = param
    param
  }

  def backward(gradient: Matrix): Unit = gradParam :+= gradient //todo: ???

  def resetGradient(): Unit = {
    gradParam :*= Matrix.zeros[Double](dim1, dim2) //todo: ???
  }

  def update(learningRate: Double): Unit = { //todo: ???
    param :-= (breeze.linalg.clip(gradParam, -clip, clip) :* learningRate)
    resetGradient()
  }

  def initialize(dist: () => Double): Matrix = { //todo: ???
    param = randMat(dim1, dim2, dist)
    param
  }

  def xavierInitialization(): Matrix = { //todo: ???
//    param = new Matrix(dim1, dim2, (0 until dim1 * dim2).map(i => xavierInitialization(0.0, (1.0/dim2))).toArray)
    param = new Matrix(dim1, dim2, (0 until dim1 * dim2).map(i => xavierInitialization(0.0, (1.0/scala.math.sqrt(dim2)))).toArray)
//    param = new Matrix(dim1, dim2, (0 until dim1 * dim2).map(i => xavierInitialization(0.0, (scala.math.sqrt(6)/scala.math.sqrt(dim2*2)))).toArray)
    param
  }
}

/**
 * A block representing matrix-vector multiplication
  *
  * @param arg1 the left block evaluating to a matrix
 * @param arg2 the right block evaluation to a vector
 */
case class Mul(arg1: Block[Matrix], arg2: Block[Vector]) extends Block[Vector] {

  def forward(): Vector = { //todo: ???
    output = arg1.forward() * arg2.forward()
    output
  }

  def backward(gradient: Vector): Unit = { //todo: ???
    arg1.backward( outer(gradient, arg2.output) )
    arg2.backward( arg1.output.t * gradient )
  }

  def update(learningRate: Double): Unit = { //todo: ???
    arg1.update(learningRate)
    arg2.update(learningRate)
  }

}

/**
 * A block representing the element-wise application of the tanh function to a vector
  *
  * @param arg a block evaluating to a vector
 */
case class Tanh(arg: Block[Vector]) extends Block[Vector] {

  def forward(): Vector = { //todo: ???
    output = tanh(arg.forward())
    output
  }

  def backward(gradient: Vector): Unit = { //todo: ???
    arg.backward( gradient :* pow(  cosh(arg.output) :* cosh(arg.output) , -1) )
  }

  def update(learningRate: Double): Unit = { //todo: ???
    arg.update(learningRate)
  }
}

/**
  * Problem 4
  */

/**
 * A potentially useful block for training a better model (https://en.wikipedia.org/wiki/Dropout_(neural_networks))
 *
 * @param prob dropout probability
 * @param arg a block evaluating to a vector whose components we want to drop
 */
case class Dropout(prob: Double, arg: Block[Vector], train: Boolean) extends Block[Vector] with BernoulliInitialization {
  override val Bernoulli = new Bernoulli(1 - prob)
  var dropout = doubleToVector(0.0)
  def forward(): Vector = { //todo: ???
    output = arg.forward()
    dropout = randVec(output.activeSize, defaultInitialization) :/ (1-prob)
    if(train) {
      output :*= dropout
    } else {
      output *= (1-prob)
    }
    output
  }
  def backward(gradient: Vector): Unit = { //todo: ???
    if(train)
//      arg.backward((gradient :* dropout) * (1-prob))
//      arg.backward((gradient :* dropout) * prob)
      arg.backward((gradient :* dropout))
//    else
//      arg.backward(gradient *prob)
  }
  def update(learningRate: Double): Unit = { //todo: ???
    arg.update(learningRate)
  }
}

/**
  * ... be free, be creative :)
  */

/**
  * Element-wise Sigmoid
  * @param arg
  */
case class SigmoidV(arg: Block[Vector]) extends Block[Vector] {

  def forward(): Vector = { //todo: ???
    output = sigmoid(arg.forward())
    output
  }
  def backward(gradient: Vector): Unit = { //todo: ???
  val derivative = sigmoid(arg.output) :* (Vector.ones[Double](arg.output.activeSize) - sigmoid(arg.output))
    arg.backward( gradient :* derivative )
  }
  def update(learningRate: Double): Unit = { //todo: ???
    arg.update(learningRate)
  }
}

/**
  * Multiplication of-word-vectors
  * @param arg1
  * @param arg2
  */
case class MulV(arg1: Block[Vector], arg2: Block[Vector]) extends Block[Vector] {
  def forward(): Vector = { //todo: ???
    output = arg1.forward() :* arg2.forward()
    output
  }
  def backward(gradient: Vector): Unit = {
    //val upstreamGradientVector = Vector.ones[Double](arg1.output.activeSize) * gradient
    arg1.backward( gradient :* arg2.output ) //todo: ???
    arg2.backward( gradient :* arg1.output )
  }
  def update(learningRate: Double): Unit = {  //todo: ???
    arg1.update(learningRate)
    arg2.update(learningRate)
  }
}

/**
  * A block representing the sigmoid of a scalar value
  *
  * @param arg a block that evaluates to a double
  */
case class ReLU(arg: Block[Vector]) extends Block[Double] {

  def forward(): Double = { //todo: ???
    output = scala.math.max(0, breeze.linalg.max(arg.forward()) )
    output
  }

  def backward(gradient: Double): Unit = { //todo: ???
    val derivative = gradient * clip(arg.output, 0.0, 1.0)
    arg.backward( derivative )
  }

  def update(learningRate: Double): Unit = { //todo: ???
    arg.update(learningRate)
  }
}