package uk.ac.ucl.cs.mr.statnlpbook.assignment2

import scala.collection.mutable

/**
 * Created by Georgios on 05/11/2015.
 */

object Features {

  /**
   * a feature function with two templates w:word,label and l:label.
   * Example for Trigger Exraction
   * @param x
   * @param y
   * @return
   */
  def defaultTriggerFeatures(x: Candidate, y: Label): FeatureVector = {
    val doc = x.doc
    val begin = x.begin
    val end = x.end
    val thisSentence = doc.sentences(x.sentenceIndex) //use this to gain access to the parent sentence
    val feats = new mutable.HashMap[FeatureKey,Double]
    feats += FeatureKey("label bias", List(y)) -> 1.0 //bias feature
    val token = thisSentence.tokens(begin) //first token of Trigger
    feats += FeatureKey("first trigger word", List(token.word, y)) -> 1.0 //word feature
    feats.toMap
  }
  /**
   * a feature function with two templates w:word,label and l:label.
   * Example for Argument Exraction
   * @param x
   * @param y
   * @return
   */
  def defaultArgumentFeatures(x: Candidate, y: Label): FeatureVector = {
    val doc = x.doc
    val begin = x.begin
    val end = x.end
    val thisSentence = doc.sentences(x.sentenceIndex)
    val event = thisSentence.events(x.parentIndex) //use this to gain access to the parent event
    val eventHeadToken = thisSentence.tokens(event.begin) //first token of event
    val feats = new mutable.HashMap[FeatureKey,Double]
    feats += FeatureKey("label bias", List(y)) -> 1.0
    val token = thisSentence.tokens(begin) //first word of argument
    feats += FeatureKey("first argument word", List(token.word, y)) -> 1.0
    feats += FeatureKey("is protein_first trigger word", List(x.isProtein.toString,eventHeadToken.word, y)) -> 1.0
    feats.toMap
  }

  def myTriggerFeatures(x: Candidate, y: Label): FeatureVector = {
    val doc = x.doc
    val begin = x.begin
    val end = x.end
    val thisSentence = doc.sentences(x.sentenceIndex) //use this to gain access to the parent sentence
    val feats = new mutable.HashMap[FeatureKey,Double]

    val token = thisSentence.tokens(begin) //first token of Trigger

    //val mentions = thisSentence.mentions(begin)
    //val leftMention = thisSentence.mentions(begin-1)
    //val rightMention = thisSentence.mentions(begin+1)

    //val deps = thisSentence.deps(begin)
    //val leftDep = thisSentence.deps(begin-1)
    //val rightDep = thisSentence.deps(begin+1)

    //features
    feats += FeatureKey("label bias", List(y)) -> 1.0 //bias feature
    feats += FeatureKey("first trigger word", List(token.word, y)) -> 1.0 //word feature
    feats += FeatureKey("first trigger pos", List(token.pos, y)) -> 1.0 //pos feature
    feats += FeatureKey("first trigger stem", List(token.stem, y)) -> 1.0 //stem feature

    if (begin > 0) {

      if (token.word.contains("-") ||
        token.word.startsWith("-") ||
        token.word.startsWith("over") ||
        token.word.startsWith("up") ||
        token.word.startsWith("down") ||
        token.word.startsWith("co")) {
        feats += FeatureKey("word contains/startswith", List(token.word, y)) -> 1.0 //word contains/starts with specific words or symbols
      }

      val leftToken = thisSentence.tokens(begin-1)
      val rightToken = thisSentence.tokens(begin+1)

      if (leftToken.pos == "NN" && token.pos == "NN" && rightToken.pos == "IN") {
        feats += FeatureKey("left right pos on NN", List(token.pos, y)) -> 1.0 //left right pos on NN
      }

      if (leftToken.pos == "JJ" && token.pos == "VBN" && rightToken.pos == "NN") {
        feats += FeatureKey("left right pos on VBN", List(token.pos, y)) -> 1.0 //left right pos on VBN
      }

      feats += FeatureKey("left bigram", List(leftToken.word, token.word, y)) -> 1.0 //left bigram
      feats += FeatureKey("right bigram", List(token.word, rightToken.word, y)) -> 1.0 //right bigram

    }

    feats.toMap
  }
  def myArgumentFeatures(x: Candidate, y: Label): FeatureVector = {
    ???
  }


}
