package uk.ac.ucl.cs.mr.statnlpbook.assignment1

import java.io.{FileOutputStream, File}

import uk.ac.ucl.cs.mr.statnlpbook.chapter.languagemodels._

import scala.collection.mutable

/**
 * @author riedel
 * @author santteegt
 */
object BarQuestion {

  trait LinearLM {
    def linearProbability(matchWord: String, history: String*): Double;
    def uniformProbability(history: String*): Double = {
      0.0
    }

  }

  /**
   * An LM that assigns increasing probability to the [/BAR] token the further the last [/BAR] token is away,
   * and uniform probability for all other tokens. How to choose the function that maps [/BAR] distance to a probability
   * is left to you, but you should guide your choice by the goal of minimizing perplexity.
   * @param vocab the vocabulary.
   */
  case class MyBarAwareLM(a: Double, b: Double, vocab: Set[String]) extends LanguageModel with LinearLM{
    def order = 20

    def linearProbability(matchWord: String, history: String*): Double = {
      //val distance = (history.reverse.takeWhile(!_.equals(matchWord)).size + 1.0) / vocab.size
      val distance = (history.reverse.takeWhile(!_.equals(matchWord)).size) / vocab.size
      //val distance = (history.reverse.takeWhile(!_.equals(matchWord)).size + 1.0)
      //a*distance + b + distance //4318, a=2, b=0.09
      //a*distance + b - distance //4330, a=2, b=0.09
      a*distance + b //4324.34
      //(a*distance + b)/vocab.size
    }

    override def uniformProbability(history: String*): Double = {
      //val distance = (history.reverse.takeWhile(!_.equals("[/BAR]")).size + 1.0) / vocab.size
      val distance = (history.length + 1.0) / vocab.size
      //val distance = (history.length + 1)
      (1.0 - (a*distance + b)) / (vocab.size-1)
    }

    //TODO: This needs to be improved by you.
    def probability(word: String, history: String*): Double = {
      val cBarProb = word match {
        case "[/BAR]" => linearProbability("[BAR]", history: _*);
        case _ => uniformProbability(history: _*);
      }
      cBarProb
    }
  }

  /**
   * THIS IS MY PER-DISTANCE BAR-AWARE LM
   * @param a NOT USED IN THIS MODEL
   * @param train
   */
  case class MyDistanceBarAwareLM(a: Double, train: IndexedSeq[String]) extends LanguageModel {
    def order = 20
    val vocab = train.toSet
    val counts = new mutable.HashMap[Int, Double] withDefaultValue 0.0
    val norm = new mutable.HashMap[Int, Double] withDefaultValue 0.0
    for (i <- order until train.length) {
      val history = train.slice(i - order + 1, i).toList
      val word = train(i)
      if(word.equals("[/BAR]")) {
        //val distance = cBARDistance("[BAR]", history: _*).toInt
        val distance = thetaBARIndex(history: _*)
        counts(distance) += 1.0
      }
      val index = thetaBARIndex(history: _*);
      norm(index) += 1.0
    }

    /**
     * calculate parameters for the MLE
     * @param history
     * @return (distance, indexBAR)
     */
    def calculateParameters(history: String*): (Int, Int) =
      //(cBARDistance("[BAR]", history: _*).toInt, thetaBARIndex(history: _*).toInt);
      (thetaBARIndex(history: _*), thetaBARIndex(history: _*));

    def thetaBARIndex(history: String*): Int = {
      //val distance = history.reverse.indexOf("[BAR]", 0)
      val distance = cBARDistance("[BAR]", history: _*)
      //val theta = if(distance >= 0) history.size - (distance + 1) else -1;
      val theta = if(distance >= 0) history.size - (distance) else -1;
      theta
    }

    def cBARDistance(matchWord: String, history: String*): Int = {
      //val distance = (history.reverse.takeWhile(!_.equals(matchWord)).size + 1.0)
      val distance = (history.reverse.takeWhile(!_.equals(matchWord)).size)
      distance
    }

    override def probability(word: String, history: String*) = {
      val parameters = calculateParameters(history: _*);
      val mle = counts(parameters._1) / norm(parameters._2)
      val oovWeight = if(word.equals(Util.OOV)) a else 1 //ALWAYS 1
      word match {
        case "[/BAR]" => mle
        case _ => (1-(oovWeight*mle))/(vocab.size-1)
      }
    }
  }

  /**
   * THIS IS MY **IMPROVED** PER-DISTANCE BAR-AWARE LM
   * @param a NOT USED IN THIS MODEL
   * @param train
   */
  case class MyImprovedDistanceBarAwareLM(a: Double, train: IndexedSeq[String]) extends LanguageModel {
    def order = 20
    val vocab = train.toSet
    val counts = new mutable.HashMap[Int, Double] withDefaultValue 0.0
    val norm = new mutable.HashMap[Int, Double] withDefaultValue 0.0
    for (i <- order until train.length) {
      val history = train.slice(i - order + 1, i).toList
      val word = train(i)
      if(word.equals("[/BAR]")) {
        //val distance = cBARDistance("[BAR]", history: _*).toInt
        val distance = thetaBARIndex(history: _*)
        counts(distance) += 1.0
      }
      if(word.equals("[BAR]") && history.last.equals("[/BAR]")) {
        //val distance = cBARDistance("[BAR]", history: _*).toInt
        //val distance = thetaBARIndex(history: _*).toInt
        val distance = thetaBARIndex(history: _*)
        counts(distance) += 1.0
      }
      val index = thetaBARIndex(history: _*);
      norm(index) += 1.0
    }

    /**
     * calculate parameters for the MLE
     * @param history
     * @return (distance, indexBAR)
     */
    def calculateParameters(word:String, history: String*): (Int, Int) =
    //(cBARDistance("[BAR]", history: _*).toInt, thetaBARIndex(history: _*).toInt);
      (thetaBARIndex(history: _*), thetaBARIndex(history: _*));

    def thetaBARIndex(history: String*): Int = {
      var theta = 0
      //val distance = history.reverse.indexOf("[BAR]", 0)
      if(history.size > 0 && history.last.equals("[/BAR]")) {
        theta = -2
      } else {
        val distance = cBARDistance("[BAR]", history: _*)
        //val theta = if(distance >= 0) history.size - (distance + 1) else -1;
        theta = if (distance >= 0) history.size - (distance) else -1;
      }
      theta
    }

    def cBARDistance(matchWord: String, history: String*): Int = {
      //val distance = (history.reverse.takeWhile(!_.equals(matchWord)).size + 1.0)
      val distance = (history.reverse.takeWhile(!_.equals(matchWord)).size)
      distance
    }

    def startofBars(word: String, history: String*): Int = {
      if(history.last.equals("[/BAR]")) 1 else -2
    }

    override def probability(word: String, history: String*) = {
      val parameters = calculateParameters(word, history: _*);
      val mle = counts(parameters._1) / norm(parameters._2)
      val oovWeight = if(word.equals(Util.OOV)) a else 1 //ALWAYS 1
      word match {
        case "[BAR]" => mle
        case _ => (1-(oovWeight*mle))/(vocab.size-1)
      }
    }
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
    println("== PRESENTING THE BAR AWARE MODEL\n")

    val lm = MyBarAwareLM(-1.0, 0.09, vocab)

    //This calculates the perplexity of the
    val pp = LanguageModel.perplexity(lm, dev)

    println("Perplexity for Improved Bar-Aware LM: " +pp)
//    val probs = vocab.map(w => w -> lm.probability(w)).toSeq.sortBy(-_._2)
//    println("Some probs: " + vocab.map(w => w -> lm.probability(w)).toSeq.sortBy(-_._2).take(10))
//    println("Total: " + probs.foldLeft(0.0)((count, x) => count + x._2));
//    println("Language generated using this LM:" + LanguageModel.sample(lm, Nil, 10) );

    //4324
//    println("===TEST3");
//    val results3 = for (alpha <- -1.0 to 1.0 by 0.01; beta <- -1.0 to 1.0 by 0.01)
//      yield (alpha,beta) -> LanguageModel.perplexity(MyBarAwareLM(alpha, beta, vocab), dev)
//    val rs3 = results3.filter(x => (x._2.isInfinite == false) && (x._2.isNaN == false)).sortBy(_._2);
//    println("Testing different perplexity Results: " + rs3.take(20))

//    var stream = "";
//    for(alpha <- -1.0 to 0 by 0.01) {
//      var values = new ListBuffer[Double]
//      for(beta <- 0.9 to 1.0 by 0.01) {
//        val value = LanguageModel.perplexity(MyBarAwareLM(alpha, beta, vocab), dev)
//        values += value
//      }
//      stream += values.mkString(",") + "\n"
//    }
//    //results3.filter((_._2.isInfinite == false) && (_._2.isNaN == false));
//    val out = new FileOutputStream("data/assignment1/p2/2-2values.txt");
//    out.write( stream.getBytes );
//    out.close()

    val uniform = UniformLM(vocab);
    println("Perplexity Uniform LM: " + LanguageModel.perplexity(uniform, dev));
//    val probsu = vocab.map(w => w -> uniform.probability(w)).toSeq.sortBy(-_._2)
//    println("Some probs: " + vocab.map(w => w -> uniform.probability(w)).toSeq.sortBy(-_._2).take(10))
//    println("Total: " + probsu.foldLeft(0.0)((count, x) => count + x._2));
//    println("Language generated using this LM:" + LanguageModel.sample(uniform, Nil, 10) );

    //TODO:
    println("== PRESENTING THE PER-DISTANCE BAR AWARE MODEL\n")

    val pdLM = MyDistanceBarAwareLM(1, train.toIndexedSeq)
    println("Perplexity for Per-Distance Bar LM: " + LanguageModel.perplexity(pdLM, dev));
//    val probsPD = vocab.map(w => w -> pdLM.probability(w)).toSeq.sortBy(-_._2)
//    println("Some probs: " + probsPD.take(10))
//    println("Total: " + probsPD.foldLeft(0.0)((count, x) => count + x._2));
//    println("Language generated using this LM:" + LanguageModel.sample(pdLM, Nil, 10) );

    //TODO: combine a unigram model with the BAR aware LM through interpolation.
    //TODO: Improve the BarAwareLM to give probability 1 to a [BAR] following a [/BAR], and 0 otherwise.
    println("\n== PRESENTING THE **IMPROVED** PER-DISTANCE BAR AWARE MODEL\n")

    val ipdLM = MyImprovedDistanceBarAwareLM(1, train.toIndexedSeq)
    println("Perplexity for Improved Per-Distance Bar LM: " + LanguageModel.perplexity(ipdLM, dev));
//    val probsIPD = vocab.map(w => w -> ipdLM.probability(w)).toSeq.sortBy(-_._2)
//    println("Some probs: " + probsIPD.take(10))
//    println("Probability of BAR: " + ipdLM.probability("[BAR]","[/BAR]"))
//    println("Total: " + probsIPD.foldLeft(0.0)((count, x) => count + x._2));
//    println("Language generated using this LM:" + LanguageModel.sample(ipdLM, Nil, 10) );

    val unigramLM = NGramLM(train.toIndexedSeq, 1)
    val interpolatedLM = InterpolatedLM(ipdLM, unigramLM, 0.25)
    println("\n== PRESENTING THE **INTERPOLATED** MODEL\n")
    println("Perplexity for Interpolated Start of Bars LM: " + LanguageModel.perplexity(interpolatedLM, dev));
//    val probsI = vocab.map(w => w -> interpolatedLM.probability(w,"[/BAR]")).toSeq.sortBy(-_._2)
//    println("Some probs: " + probsI.take(10))
//    println("Probability of BAR: " + interpolatedLM.probability("[BAR]","[/BAR]"))
//    //println("WRONG PROBS: " + probsPD.filter(w => w._2 <0 || w._2>1).toSeq.length)
//    println("Total: " + probsI.foldLeft(0.0)((count, x) => count + x._2));
//    val sampling = LanguageModel.sample(interpolatedLM, Nil, 1000)
//    println("Language generated using this LM:" + sampling.take(100) );

//    var empDistr = new ListBuffer[Int]();
//    var count = 0;
//    for(token <- sampling) {
//      //val token = words(i);
//      if(token.equals("[BAR]")) count = 0;
//      else if(token.equals("[/BAR]")) empDistr += count;
//      else count += 1;
//    }
//    val out1 = new FileOutputStream("data/assignment1/p2/sampling.txt");
//    out1.write(empDistr.mkString(",").getBytes);
//    out1.close()
//
//    val resultsI = for (alpha <- (0 until 20).map(_ / 20.0))
//      yield alpha -> LanguageModel.perplexity(InterpolatedLM(ipdLM, unigramLM, alpha), dev)
//
//    val out2 = new FileOutputStream("data/assignment1/p2/alpha.txt");
//    out2.write( resultsI.mkString("",",","\n").getBytes );
//    out2.close()
//    val rs = resultsI.filter(x => (x._2.isInfinite == false) && (x._2.isNaN == false)).sortBy(_._2);
//    println("Testing best alpha for different perplexity Results: " + rs.take(20))

  }
}
