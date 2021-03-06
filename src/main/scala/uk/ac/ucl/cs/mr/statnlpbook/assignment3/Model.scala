package uk.ac.ucl.cs.mr.statnlpbook.assignment3

import java.util.StringTokenizer

import breeze.linalg.{DenseVector}

import scala.collection.mutable

/**
 * @author rockt
 */
trait Model {
  /**
   * Stores all vector parameters
   */
  val vectorParams = new mutable.HashMap[String, VectorParam]()
  /**
   * Stores all matrix parameters
   */
  val matrixParams = new mutable.HashMap[String, MatrixParam]()
  /**
   * Maps a word to its trainable or fixed vector representation
    *
    * @param word the input word represented as string
   * @return a block that evaluates to a vector/embedding for that word
   */
  def wordToVector(word: String): Block[Vector]
  /**
   * Composes a sequence of word vectors to a sentence vectors
    *
    * @param words a sequence of blocks that evaluate to word vectors
   * @return a block evaluating to a sentence vector
   */
  def wordVectorsToSentenceVector(words: Seq[Block[Vector]]): Block[Vector]
  /**
   * Calculates the score of a sentence based on the vector representation of that sentence
    *
    * @param sentence a block evaluating to a sentence vector
   * @return a block evaluating to the score between 0.0 and 1.0 of that sentence (1.0 positive sentiment, 0.0 negative sentiment)
   */
  def scoreSentence(sentence: Block[Vector]): Block[Double]
  /**
   * Predicts whether a sentence is of positive or negative sentiment (true: positive, false: negative)
    *
    * @param sentence a tweet as a sequence of words
   * @param threshold the value above which we predict positive sentiment
   * @return whether the sentence is of positive sentiment
   */
  def predict(sentence: Seq[String])(implicit threshold: Double = 0.5): Boolean = {
    val wordVectors = sentence.map(wordToVector)
    val sentenceVector = wordVectorsToSentenceVector(wordVectors)
    scoreSentence(sentenceVector).forward() >= threshold
  }
  /**
   * Defines the training loss
    *
    * @param sentence a tweet as a sequence of words
   * @param target the gold label of the tweet (true: positive sentiement, false: negative sentiment)
   * @return a block evaluating to the negative log-likelihod plus a regularization term
   */
  def loss(sentence: Seq[String], target: Boolean): Loss = {
    val targetScore = if (target) 1.0 else 0.0
    val wordVectors = sentence.map(wordToVector)
    val sentenceVector = wordVectorsToSentenceVector(wordVectors)
    val score = scoreSentence(sentenceVector)
    new LossSum(NegativeLogLikelihoodLoss(score, targetScore), regularizer(wordVectors))
  }
  /**
   * Regularizes the parameters of the model for a given input example
    *
    * @param words a sequence of blocks evaluating to word vectors
   * @return a block representing the regularization loss on the parameters of the model
   */
  def regularizer(words: Seq[Block[Vector]]): Loss

  var isTraining = true

  def setTrainingMode(train: Boolean): Unit = isTraining = train

  def loadFixedWordVectors(wordDim: Int): Unit = {
    for(line <- io.Source.fromFile("./data/assignment3/word2vec/vectors.txt", "ISO-8859-1").getLines() ) {
      var count = 0
      val tokenizer = new StringTokenizer(line, " ")
      val values = mutable.Buffer[Double]()

      var word = ""
      while(tokenizer.hasMoreTokens && count <= wordDim) {
        if(count == 0) {
          word = tokenizer.nextToken()
        } else {
          values += tokenizer.nextToken().toDouble
        }
        count += 1
      }
      LookupTable.addFixedWordVector(word, new Vector(values.toArray))
    }
  }
}


/**
 * Problem 2
 * A sum of word vectors model
  *
  * @param embeddingSize dimension of the word vectors used in this model
 * @param regularizationStrength strength of the regularization on the word vectors and global parameter vector w
 */
class SumOfWordVectorsModel(embeddingSize: Int, regularizationStrength: Double = 0.0) extends Model {
  /**
   * We use a lookup table to keep track of the word representations
   */
  override val vectorParams: mutable.HashMap[String, VectorParam] =
    LookupTable.trainableWordVectors
  /**
   * We are also going to need another global vector parameter
   */
  vectorParams += "param_w" -> VectorParam(embeddingSize)
  
  def wordToVector(word: String): Block[Vector] = {
  var vectorBlock: Block[Vector] = null
  try {
      vectorBlock = LookupTable.get(word)
      LookupTable.addTrainableWordVector(word, vectorBlock.forward())
    }catch  {
      case e: Exception => vectorBlock = LookupTable.addTrainableWordVector(word, embeddingSize)
    }
    vectorBlock
  }


  def wordVectorsToSentenceVector(words: Seq[Block[Vector]]): Block[Vector] = Sum(words)

  def scoreSentence(sentence: Block[Vector]): Block[Double] = Sigmoid( Dot(vectorParams("param_w"), sentence) )

  def regularizer(words: Seq[Block[Vector]]): Loss = L2Regularization(regularizationStrength, (words ++ Seq[Block[Vector]](vectorParams("param_w"))):_* )
}


/**
 * Problem 3
 * A recurrent neural network model
  *
  * @param embeddingSize dimension of the word vectors used in this model
 * @param hiddenSize dimension of the hidden state vector used in this model
 * @param vectorRegularizationStrength strength of the regularization on the word vectors and global parameter vector w
 * @param matrixRegularizationStrength strength of the regularization of the transition matrices used in this model
 */
class RecurrentNeuralNetworkModel(embeddingSize: Int, hiddenSize: Int,
                                  vectorRegularizationStrength: Double = 0.0,
                                  matrixRegularizationStrength: Double = 0.0) extends Model {
  override val vectorParams: mutable.HashMap[String, VectorParam] =
    LookupTable.trainableWordVectors

  vectorParams += "param_w" -> VectorParam(hiddenSize)
  vectorParams += "param_h0" -> VectorParam(hiddenSize)
  vectorParams += "param_b" -> VectorParam(hiddenSize)

  override val matrixParams: mutable.HashMap[String, MatrixParam] =
    new mutable.HashMap[String, MatrixParam]()
    matrixParams += "param_Wx" -> MatrixParam(hiddenSize, embeddingSize)
    matrixParams += "param_Wh" -> MatrixParam(hiddenSize, hiddenSize)

  def wordToVector(word: String): Block[Vector] = {
  var vectorBlock: Block[Vector] = null
    try {
      vectorBlock = LookupTable.get(word)
      LookupTable.addTrainableWordVector(word, vectorBlock.forward())
    }catch  {
      case e: Exception => vectorBlock = LookupTable.addTrainableWordVector(word, embeddingSize)
    }
    vectorBlock
  }

  def wordVectorsToSentenceVector(words: Seq[Block[Vector]]): Block[Vector] = {
    val h_0 = Tanh( Sum( Seq( Mul(matrixParams("param_Wh"), vectorParams("param_h0")), vectorParams("param_b") ) ) )
    words.foldLeft(h_0)( (h_t_m1, word) =>
                                            Tanh(
                                              Sum(
                                                Seq(
                                                  Mul( matrixParams("param_Wh"), h_t_m1.forward() ),
                                                  Mul( matrixParams("param_Wx"), word ),
                                                  vectorParams("param_b")
                                                )
                                              )
                                            )

    )
  }

  def scoreSentence(sentence: Block[Vector]): Block[Double] = Sigmoid( Dot(vectorParams("param_w"), sentence) ) 
//  def scoreSentence(sentence: Block[Vector]): Block[Double] = ReLU( Dot(vectorParams("param_w"), sentence) ) 

  def regularizer(words: Seq[Block[Vector]]): Loss =
    new LossSum(
      L2Regularization(vectorRegularizationStrength, (words ++ Seq[Block[Vector]](vectorParams("param_w"), vectorParams("param_h0"), vectorParams("param_b"))):_*),
      L2Regularization(matrixRegularizationStrength, Seq(matrixParams("param_Wh"), matrixParams("param_Wx")):_*)
    )
}

class RecurrentNeuralNetworkModelReLU(embeddingSize: Int, hiddenSize: Int,
                                  vectorRegularizationStrength: Double = 0.0,
                                  matrixRegularizationStrength: Double = 0.0) extends Model {
  override val vectorParams: mutable.HashMap[String, VectorParam] =
    LookupTable.trainableWordVectors

  vectorParams += "param_w" -> VectorParam(hiddenSize)
  vectorParams += "param_h0" -> VectorParam(hiddenSize)
  vectorParams += "param_b" -> VectorParam(hiddenSize)

  override val matrixParams: mutable.HashMap[String, MatrixParam] =
    new mutable.HashMap[String, MatrixParam]()
  matrixParams += "param_Wx" -> MatrixParam(hiddenSize, embeddingSize)
  matrixParams += "param_Wh" -> MatrixParam(hiddenSize, hiddenSize)

  def wordToVector(word: String): Block[Vector] = {
  var vectorBlock: Block[Vector] = null
    try {
      vectorBlock = LookupTable.get(word)
      LookupTable.addTrainableWordVector(word, vectorBlock.forward())
    }catch  {
      case e: Exception => vectorBlock = LookupTable.addTrainableWordVector(word, embeddingSize)
    }
    vectorBlock
  }

  def wordVectorsToSentenceVector(words: Seq[Block[Vector]]): Block[Vector] = {
  val h_0 = Sum( Seq( VectorConstant(DenseVector.zeros[Double](hiddenSize)) ) )
    words.foldLeft(h_0)( (h_t_m1, word) =>
        Sum(
          Seq(
            Sum(
              Seq(
                Mul( matrixParams("param_Wh"), h_t_m1.forward() ),
                Mul( matrixParams("param_Wx"), word )
              )
            ),
            vectorParams("param_b")
          )
        )
    )
  }

    def scoreSentence(sentence: Block[Vector]): Block[Double] = ReLU( sentence ) 

  def regularizer(words: Seq[Block[Vector]]): Loss =
    new LossSum(
      L2Regularization(vectorRegularizationStrength, (words ++ Seq[Block[Vector]](vectorParams("param_w"), vectorParams("param_h0"), vectorParams("param_b"))):_*),
      L2Regularization(matrixRegularizationStrength, Seq(matrixParams("param_Wh"), matrixParams("param_Wx")):_*)
    )
}


class RecurrentNeuralNetworkModelDropout(embeddingSize: Int, hiddenSize: Int,
                                  vectorRegularizationStrength: Double = 0.0,
                                  matrixRegularizationStrength: Double = 0.0) extends Model {
  override val vectorParams: mutable.HashMap[String, VectorParam] =
    LookupTable.trainableWordVectors

  vectorParams += "param_w" -> VectorParam(hiddenSize)
  vectorParams += "param_h0" -> VectorParam(hiddenSize)
  vectorParams += "param_b" -> VectorParam(hiddenSize)

  override val matrixParams: mutable.HashMap[String, MatrixParam] =
    new mutable.HashMap[String, MatrixParam]()
  matrixParams += "param_Wx" -> MatrixParam(hiddenSize, embeddingSize)
  matrixParams += "param_Wh" -> MatrixParam(hiddenSize, hiddenSize)

  def wordToVector(word: String): Block[Vector] = {
  var vectorBlock: Block[Vector] = null
    try {
      vectorBlock = LookupTable.get(word)
      LookupTable.addTrainableWordVector(word, vectorBlock.forward())
    }catch  {
      case e: Exception => vectorBlock = LookupTable.addTrainableWordVector(word, embeddingSize)
    }
    vectorBlock
  }

  def wordVectorsToSentenceVector(words: Seq[Block[Vector]]): Block[Vector] = {
      val h_0 = Tanh( Sum( Seq( Mul(matrixParams("param_Wh"), vectorParams("param_h0")), vectorParams("param_b") ) ) )
    words.foldLeft(h_0)( (h_t_m1, word) =>
                                          Tanh(
                                            Sum(
                                              Seq(
                                                Mul( matrixParams("param_Wh"), h_t_m1.forward() ),
                                                Mul( matrixParams("param_Wx"), word ),
                                                vectorParams("param_b")
                                              )
                                            )
                                          )
    )
  }

  def scoreSentence(sentence: Block[Vector]): Block[Double] = Sigmoid( Dot(vectorParams("param_w"), Dropout(0.5, sentence, isTraining)) )

  def regularizer(words: Seq[Block[Vector]]): Loss =
    new LossSum(
      L2Regularization(vectorRegularizationStrength, (words ++ Seq[Block[Vector]](vectorParams("param_w"), vectorParams("param_h0"), vectorParams("param_b"))):_*),
      L2Regularization(matrixRegularizationStrength, Seq(matrixParams("param_Wh"), matrixParams("param_Wx")):_*)
    )
}

class RecurrentNeuralNetworkModelLSTM(embeddingSize: Int, hiddenSize: Int,
                                  vectorRegularizationStrength: Double = 0.0,
                                  matrixRegularizationStrength: Double = 0.0) extends Model {
  override val vectorParams: mutable.HashMap[String, VectorParam] =
    LookupTable.trainableWordVectors

  vectorParams += "param_w" -> VectorParam(hiddenSize)
  vectorParams += "param_h0" -> VectorParam(hiddenSize)

  vectorParams += "param_bi" -> VectorParam(hiddenSize)
  vectorParams += "param_bf" -> VectorParam(hiddenSize)
  vectorParams += "param_bo" -> VectorParam(hiddenSize)
  vectorParams += "param_bc" -> VectorParam(hiddenSize)

  override val matrixParams: mutable.HashMap[String, MatrixParam] =
    new mutable.HashMap[String, MatrixParam]()

  matrixParams += "param_Wxi" -> MatrixParam(hiddenSize, embeddingSize)
  matrixParams += "param_Wxf" -> MatrixParam(hiddenSize, embeddingSize)
  matrixParams += "param_Wxo" -> MatrixParam(hiddenSize, embeddingSize)
  matrixParams += "param_Wxc" -> MatrixParam(hiddenSize, embeddingSize)

  matrixParams += "param_Whi" -> MatrixParam(hiddenSize, hiddenSize)
  matrixParams += "param_Whf" -> MatrixParam(hiddenSize, hiddenSize)
  matrixParams += "param_Who" -> MatrixParam(hiddenSize, hiddenSize)
  matrixParams += "param_Whc" -> MatrixParam(hiddenSize, hiddenSize)

  def wordToVector(word: String): Block[Vector] = {
  var vectorBlock: Block[Vector] = null
    try {
      vectorBlock = LookupTable.get(word)
      LookupTable.addTrainableWordVector(word, vectorBlock.forward())
    }catch  {
      case e: Exception => vectorBlock = LookupTable.addTrainableWordVector(word, embeddingSize)
    }
    vectorBlock
  }

  def wordVectorsToSentenceVector(words: Seq[Block[Vector]]): Block[Vector] = {

    val const = DenseVector.zeros[Double](hiddenSize)
    val constant = VectorConstant(const)
    val h_0 = MulV(const, const)
    var c_m1 = Sum(Seq(constant))
    var counter = 0
    words.foldLeft(h_0)( (h_t_m1, word) => {

      val c_expected = Tanh(
                          Sum(
                            Seq(
                              Mul(matrixParams("param_Whc"), h_t_m1.forward()),
                              Mul(matrixParams("param_Wxc"), word),
                              vectorParams("param_bc")
                            )
                          )
                        )
      val i = SigmoidV(
                    Sum(
                      Seq(
                        Mul(matrixParams("param_Wxi"), word),
                        Mul(matrixParams("param_Whi"), h_t_m1.forward()),
                        vectorParams("param_bi")
                      )
                    )
              )

      val f = SigmoidV(
            Sum(
              Seq(
                Mul(matrixParams("param_Wxf"), word),
                Mul(matrixParams("param_Whf"), h_t_m1.forward()),
                vectorParams("param_bf")
              )
            )
      )

      val o = SigmoidV(
            Sum(
              Seq(
                Mul(matrixParams("param_Wxf"), word),
                Mul(matrixParams("param_Whf"), h_t_m1.forward()),
                vectorParams("param_bo")
              )
            )
      )

      val c =  Sum( Seq( MulV(i, c_expected), MulV(f, c_m1) ) )
      c_m1 = c
      counter += 1
      MulV(o, Tanh(c))

    })
  }

  def scoreSentence(sentence: Block[Vector]): Block[Double] = Sigmoid( Dot(vectorParams("param_w"), Dropout(0.5, sentence, isTraining)) )

  def regularizer(words: Seq[Block[Vector]]): Loss =
    new LossSum(
      L2Regularization(vectorRegularizationStrength, (words ++ Seq[Block[Vector]](vectorParams("param_w"), vectorParams("param_bc"), vectorParams("param_bi"), vectorParams("param_bf"), vectorParams("param_bo"))):_*),
      L2Regularization(matrixRegularizationStrength, Seq(matrixParams("param_Whc"), matrixParams("param_Whi"), matrixParams("param_Whf"), matrixParams("param_Who"), matrixParams("param_Wxc"), matrixParams("param_Wxi"), matrixParams("param_Wxf"), matrixParams("param_Wxo")):_*)
    )
}