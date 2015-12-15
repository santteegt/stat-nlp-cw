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

    //features provided
    feats += FeatureKey("label bias", List(y)) -> 1.0 //bias feature
    feats += FeatureKey("first trigger word", List(token.word, y)) -> 1.0 //word feature

    //features developed
    feats += FeatureKey("first trigger pos", List(token.pos, y)) -> 1.0 //pos feature
    feats += FeatureKey("first trigger stem", List(token.stem, y)) -> 1.0 //stem feature
    feats += FeatureKey("first trigger size", List(token.word.length.toString, y)) -> 1.0 //first trigger size feature

    //tokens
    if ((begin > 0) && (begin < thisSentence.tokens.length)) {

      val leftToken = thisSentence.tokens(begin-1)
      val rightToken = thisSentence.tokens(begin+1)

      if (leftToken.pos == "NN" && token.pos.startsWith("N") && rightToken.pos == "IN") {
        feats += FeatureKey("left right pos on Nouns", List(leftToken.pos, token.pos, rightToken.pos, y)) -> 1.0 //left right pos on N*
      }

      if (leftToken.pos == "JJ" && token.pos.startsWith("V") && rightToken.pos == "NN") {
        feats += FeatureKey("left right pos on Verbs", List(leftToken.pos, token.pos, rightToken.pos, y)) -> 1.0 //left right pos on V*
      }

      if (!Set("NN").contains(leftToken.pos) && token.pos == "IN" && Set("DT","NN", "JJ").contains(rightToken.pos)) {
        feats += FeatureKey("None trigger Event", List(token.word, y)) -> 1.0 //left right pos on VBN
      }

      if (token.pos.equals("VBD") && Set("TO").contains(rightToken.pos)) {
        feats += FeatureKey("None trigger Event TO", List(token.word, y)) -> 1.0 //left right pos on VBN
      }

      //feats += FeatureKey("token unigram on the left", List(leftToken.word, y)) -> 1.0 //unigram on the left feature
      feats += FeatureKey("token bigram on the left", List(leftToken.word, token.word, y)) -> 1.0 //bigram on the left feature
      //feats += FeatureKey("token unigram on the right", List(rightToken.word, y)) -> 1.0 //unigram on the right feature
      feats += FeatureKey("token bigram on the right", List(token.word, rightToken.word, y)) -> 1.0 //bigram on the right feature

      if (token.word.startsWith("-") || token.word.contains("-")) {
        feats += FeatureKey("token startswith/contains", List(token.word, rightToken.word, y)) -> 1.0 //token startswith/contains
      }

    }

    val tokenizer = token.word.split("-")
    for(segment <- tokenizer) {
      val index_ = if(y.indexOf('_') > 0) y.indexOf('_') else 0
      val labelGold = y.substring(index_ + 1)
      if (labelGold.toLowerCase.endsWith(segment)) {
        feats += FeatureKey("trigger dictionary ends with", List(token.word, segment, y)) -> 1.0 //segment word stem as part of trigger dictionary feature
      }
    }

    val tokenizerStem = token.stem.split("-")
    for(segment <- tokenizerStem) {
      if (y.toLowerCase.startsWith(segment)) {
        feats += FeatureKey("trigger dictionary starts with", List(token.word, segment, y)) -> 1.0 //segment word stem as part of trigger dictionary feature
      }
    }

    //mentions
    val mentions = thisSentence.mentions
    val leftMention = mentions.filter(m => m.begin <= begin)
    val rightMention = mentions.filter(m => m.begin >= end)

    feats += FeatureKey("first mention size", List(mentions.length.toString, y)) -> 1.0 //first mention size feature

    if (leftMention.nonEmpty) {
      val distanceLeft = (begin - leftMention.last.begin).toString
      feats += FeatureKey("distance to left mention", List(distanceLeft, y)) -> 1.0 //distance to left mention feature
    }

    if (rightMention.nonEmpty) {
      val distanceRight = (rightMention.head.begin - end).toString
      feats += FeatureKey("distance to right mention", List(distanceRight, y)) -> 1.0 //distance to right mention feature
    }

    //deps
    val deps = thisSentence.deps
    val depHead = deps.filter(dh => {dh.head == begin})
    val depMod = deps.filter(dm => {dm.mod == begin})

    depHead.foreach(dh => {
      feats += FeatureKey("dep head", List(dh.label, y)) -> 1.0 //dep head feature
    })

    depMod.foreach(dm => {
      feats += FeatureKey("dep mod", List(dm.label, y)) -> 1.0 //dep mod feature
    })

    feats.toMap
  }

  def myArgumentFeatures(x: Candidate, y: Label): FeatureVector = {
    val doc = x.doc
    val begin = x.begin
    val end = x.end
    val thisSentence = doc.sentences(x.sentenceIndex)
    val event = thisSentence.events(x.parentIndex) //use this to gain access to the parent event
    val eventHeadToken = thisSentence.tokens(event.begin) //first token of event
    val feats = new mutable.HashMap[FeatureKey,Double]
    val token = thisSentence.tokens(begin) //first word of argument

    //features provided
    feats += FeatureKey("label bias", List(y)) -> 1.0 //label bias feature
    feats += FeatureKey("first argument word", List(token.word, y)) -> 1.0 //first argument word feature
    feats += FeatureKey("is protein_first trigger word", List(x.isProtein.toString,eventHeadToken.word, y)) -> 1.0 //is protein_first trigger word feature

    //features developed
    feats += FeatureKey("first token of event pos", List(eventHeadToken.pos, y)) -> 1.0 //first token of event pos feature
    feats += FeatureKey("first token of event stem", List(eventHeadToken.stem, y)) -> 1.0 //first token of event stem feature
    feats += FeatureKey("is protein_first", List(x.isProtein.toString, y)) -> 1.0 //is protein_first feature

    if (begin > 0) {
      val leftEvent = thisSentence.tokens(begin-1)
      feats += FeatureKey("begin event unigram on the left", List(leftEvent.word, y)) -> 1.0 //begin event unigram on the left feature
      feats += FeatureKey("begin event bigram on the left", List(leftEvent.word, eventHeadToken.word, y)) -> 1.0 //begin event bigram on the left feature
    }

    if (begin < thisSentence.tokens.length) {
      val rightEvent = thisSentence.tokens(begin+1)
      feats += FeatureKey("begin event unigram on the right", List(rightEvent.word, y)) -> 1.0 //begin event unigram on the right feature
      feats += FeatureKey("begin event bigram on the right", List(eventHeadToken.word, rightEvent.word, y)) -> 1.0 //begin event bigram on the right feature
    }

    if (event.begin > 0) {
      val leftEvent = thisSentence.tokens(event.begin-1)
      feats += FeatureKey("event begin event unigram on the left", List(leftEvent.word, y)) -> 1.0 //event begin event unigram on the left feature
      feats += FeatureKey("event begin event bigram on the left", List(leftEvent.word, eventHeadToken.word, y)) -> 1.0 //event begin event bigram on the left feature
    }

    if (event.begin < thisSentence.tokens.length) {
      val rightEvent = thisSentence.tokens(event.begin+1)
      feats += FeatureKey("event begin event unigram on the right", List(rightEvent.word, y)) -> 1.0 //event begin event unigram on the right feature
      feats += FeatureKey("event begin event bigram on the right", List(eventHeadToken.word, rightEvent.word, y)) -> 1.0 //event begin event bigram on the right feature
    }

    feats.toMap

  }


}
