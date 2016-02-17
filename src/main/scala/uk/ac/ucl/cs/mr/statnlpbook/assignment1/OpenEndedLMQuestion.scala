package uk.ac.ucl.cs.mr.statnlpbook.assignment1

import java.io.File

import uk.ac.ucl.cs.mr.statnlpbook.assignment1.BarQuestion.MyBarAwareLM
import uk.ac.ucl.cs.mr.statnlpbook.chapter.languagemodels.{Util, LanguageModel}

/**
 * @author riedel
 */
object OpenEndedLMQuestion {

  /**
   * An LM that performs well on the dev set.
   */
  case class MyReallyGooDLM(vocab: Set[String]) extends LanguageModel {
    def order = ???
    def probability(word: String, history: String*) = ???
  }

  def main(args: Array[String]) {
    //The training file we provide you
    val trainFile = new File(args(0))

    //the dev file we provide you.
    val devFile = new File(args(1))

    //the training sequence of words
    val train = Assignment1Util.loadWords(trainFile).toBuffer

    //the dev sequence of words
    val dev = Assignment1Util.loadWords(devFile).toBuffer

    //the vocabulary. Contains the training words and the OOV symbol (as the dev set has been preprocessed by
    //replacing words not in the training set with OOV).
    val vocab = train.toSet + Util.OOV

    //TODO: Improve the MyBarAwareLM implementation
    val lm = MyReallyGooDLM(vocab)

    //This calculates the perplexity of the
    val pp = LanguageModel.perplexity(lm, dev)

    println(pp)

  }

}
