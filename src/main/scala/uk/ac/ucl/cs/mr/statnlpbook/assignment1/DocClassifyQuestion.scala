package uk.ac.ucl.cs.mr.statnlpbook.assignment1

import java.io.{FileOutputStream, File}
import uk.ac.ucl.cs.mr.statnlpbook.assignment1.Assignment1Util.Instance
import uk.ac.ucl.cs.mr.statnlpbook.chapter.languagemodels._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


case class ClassificationLM(train: Seq[Instance]) {

  case class Param(classId: String, lm: CountLM, probClass: Double)
  val classifier = new mutable.HashMap[String, Param];

  var collectionDocs = new ListBuffer[String]
  train.foreach(instance => collectionDocs ++= instance.lyrics.split(" ").toList)
  val collectionLM = NGramLM(collectionDocs.toIndexedSeq, 1) //NOT USED

  val classes = train.groupBy(_.author).map(x => x._1.get -> x._2.map(_.lyrics))
  for(authorClass <- classes) {
    var docs = new ListBuffer[String]
    authorClass._2.foreach(lyrics => docs ++= lyrics.split(" ").toList)
//    val out = new FileOutputStream("data/assignment1/p3/" + authorClass._1 + ".txt");
//    out.write( docs.mkString(" ").getBytes );
//    out.close()
    //val baseline = NGramLM(docs.toIndexedSeq, 2)
    //val baseline = NGramLM(docs.toIndexedSeq, 1)
    //val baseline = NGramLM( injectOOVs(OOV, docs.toSeq), 1 )
    val baseline = NGramLM(docs.toIndexedSeq, 1)
    val backoff = NGramLM(docs.toIndexedSeq, 1) //NOT USED
    val lm = LaplaceLM(baseline, 1)
    //val lm = LaplaceLMWithDiscounts(baseline, 1)
    //val lm = InterpolatedLM(baseline, backoff, 0.6)


    classifier(authorClass._1) = Param("", lm, math.log((authorClass._2.size.toDouble)/train.size.toDouble) )
    //classifier(authorClass._1) = Param("", lm, ((train.size.toDouble-authorClass._2.size.toDouble)/train.size.toDouble) )
    //classifier(authorClass._1) = Param("", lm, ((authorClass._2.size.toDouble)/train.size.toDouble) )
  }

  def classify(doc: Instance) = {
    val words = doc.lyrics.split(" ").toIndexedSeq
    //val dev_smoothed = replaceOOVs(OOV, train_smoothed.toSet, dev.toSeq)
    val scores = classifier.map(classLM => classLM._1 -> (classLM._2.probClass + probability(classLM._2.lm, words) ))
    //val scores = classifier.map(classLM => classLM._1 -> classLM._2.probClass * probability(UniformLM(classLM._2.lm.vocab), words))
    //val scores = classifier.map(classLM => classLM._1 -> probability(UniformLM(classLM._2.lm.vocab), words))
    //val scores = classifier.map(classLM => classLM._1 -> probability(classLM._2.lm, words))
    Some(scores.maxBy(_._2)._1)
  }

  def probability(lm: CountLM, data: Seq[String]): Double = {
    var logProb = 0.0
    val historyOrder = lm.order - 1
    //val data_smoothed = replaceOOVs(OOV, lm.vocab, data)
    val data_smoothed = data
    for (i <- historyOrder until data_smoothed.length) {
      val history = data_smoothed.slice(i - historyOrder, i)
      val word = data_smoothed(i)
      logProb += math.log( lm.probability(word, history: _*) )
      //prob *= math.log( lm.probability(word, history: _*) )
      //prob += (if(lm.counts(word :: history.toList) > 0) math.log( lm.probability(word, history: _*) ) else math.log( collectionLM.probability(word, history: _*) ))
    }
    logProb
  }

}

/**
 * @author mbosnjak
 * @author santteegt
 */
object DocClassifyQuestion {

  def main(args: Array[String]): Unit = {
    // load the datasets

    val train = Assignment1Util.loadDataset(new File(args(0)))
    val dev = Assignment1Util.loadDataset(new File(args(1)))
    //val test = Assignment1Util.loadDataset(new File(args(2)))

    val classificationLM = ClassificationLM(train)

    // TODO given an instance, how would you classify it
    def classify(instance: Instance) = {
      classificationLM.classify(instance)
    }

    // execute your classifier
    val predictions = train.map(i => Instance(i.lyrics, i.author, classify(i)))

    // accurately predicted instances
    val accuratelyPredicted = predictions.map(i => i.author.get == i.prediction.get).count(_ == true)

//    println("Set size: " + dev.size)
//    println("Accuracies: " +
//      predictions.filter(i => (i.author.get.equals(i.prediction.get))).groupBy(_.author).mapValues(_.size)
//    )
//    println("Innacuracies: " +
//    predictions.filter(i => !(i.author.get.equals(i.prediction.get))).groupBy(_.author).mapValues(_.size)
//    )

    // total number of instances
    val totalInstances = predictions.length

    // evaluate accuracy
    val accuracy = 1.0 * accuratelyPredicted / totalInstances

    println("classification accuracy:" + accuracy)

  }

}
