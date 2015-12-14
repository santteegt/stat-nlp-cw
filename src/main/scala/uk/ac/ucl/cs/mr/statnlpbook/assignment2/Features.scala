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

    val mentions = thisSentence.mentions
    val leftMention = mentions.filter(m => m.begin <= begin)
    val rightMention = mentions.filter(m => m.begin >= end)

    //features
    feats += FeatureKey("label bias", List(y)) -> 1.0 //bias feature
    feats += FeatureKey("first trigger word", List(token.word, y)) -> 1.0 //word feature
    feats += FeatureKey("first trigger pos", List(token.pos, y)) -> 1.0 //pos feature
    feats += FeatureKey("first trigger stem", List(token.stem, y)) -> 1.0 //stem feature
    feats += FeatureKey("first trigger size", List(token.word.length.toString, y)) -> 1.0 //first trigger size
    feats += FeatureKey("first mention size", List(mentions.length.toString, y)) -> 1.0 //first mention size

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

      //Changes by Santiago
      if (/*!Set("CC").contains(leftToken.pos) &&*/ leftToken.pos == "NN" /*&& token.pos == "NN"*/&& token.pos.startsWith("N") && rightToken.pos == "IN") {
        //feats += FeatureKey("left right pos on NN", List(token.pos, y)) -> 1.0 //left right pos on NN
        feats += FeatureKey("left right pos on NN", List(leftToken.pos, token.pos, rightToken.pos, y)) -> 1.0 //left right pos on NN
      }

      //Changes by Santiago
      if (leftToken.pos == "JJ" /*&& token.pos == "VBN"*/&& token.pos.startsWith("V") && rightToken.pos == "NN") {
        //feats += FeatureKey("left right pos on VBN", List(token.pos, y)) -> 1.0 //left right pos on VBN
        feats += FeatureKey("left right pos on VBN", List(leftToken.pos, token.pos, rightToken.pos, y)) -> 1.0 //left right pos on VBN
      }
      feats += FeatureKey("left unigram trigger", List(leftToken.word, y)) -> 1.0 //left unigram trigger
      feats += FeatureKey("left bigram trigger", List(leftToken.word, token.word, y)) -> 1.0 //left bigram trigger
      feats += FeatureKey("right unigram trigger", List(rightToken.word, y)) -> 1.0 //right unigram trigger
      feats += FeatureKey("right bigram trigger", List(token.word, rightToken.word, y)) -> 1.0 //right bigram trigger

      //Changes by Santiago
      if (!Set("NN").contains(leftToken.pos) && token.pos == "IN" && Set("DT","NN", "JJ").contains(rightToken.pos)) {
        feats += FeatureKey("None trigger Event", List(token.word, y)) -> 1.0 //left right pos on VBN
      }
      //Changes by Santiago
      if (token.pos.equals("VBD") && Set("TO").contains(rightToken.pos)) {
        feats += FeatureKey("None trigger Event TO", List(token.word, y)) -> 1.0 //left right pos on VBN
      }
    }

    //Changes by Santiago
    val tokenizer = token.word.split("-")
    for(segment <- tokenizer) {
      val index_ = if(y.indexOf('_') > 0) y.indexOf('_') else 0;
      val labelGold = y.substring(index_ + 1)
      if (labelGold.toLowerCase().endsWith(segment)) {
        feats += FeatureKey("trigger dictionary ends with", List(token.word, segment, y)) -> 1.0 //segment word stem as part of trigger dictionary
      }
    }
    //Changes by Santiago
    val tokenizerStem = token.stem.split("-")
    for(segment <- tokenizerStem) {
      if (y.toLowerCase().startsWith(segment)) {
        feats += FeatureKey("trigger dictionary starts with", List(token.word, segment, y)) -> 1.0 //segment word stem as part of trigger dictionary
      }
    }

//    thisSentence.deps.filter(x => x.head == begin && token.pos.matches("^(V|N).*")).foreach(dep =>  {
//      if(Set("nn","det","amod","prep_of","prep_in").contains(dep.label) && thisSentence.mentions.map(_.begin).contains(dep.mod)) {
//        feats += FeatureKey("None Trigger Event DEPS HEAD", List(token.word, y)) -> 1.0
//      }
//
//    })

//    thisSentence.deps.filter(x => x.head == begin).foreach(dep =>  {
//      if(token.pos.matches("^V|N") && thisSentence.mentions.map(_.begin).contains(dep.mod)) {
//        feats += FeatureKey("trigger goes to a protein", List(token.word, y)) -> 1.0
//      }
//
//
//    })

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
    feats += FeatureKey("label bias", List(y)) -> 1.0
    val token = thisSentence.tokens(begin) //first word of argument

    feats += FeatureKey("first argument word", List(token.word, y)) -> 1.0 //first argument word

    feats += FeatureKey("first token of event pos", List(eventHeadToken.pos, y)) -> 1.0 //first token of event pos
    feats += FeatureKey("first token of event stem", List(eventHeadToken.stem, y)) -> 1.0 //first token of event stem

    feats += FeatureKey("is protein_first trigger word", List(x.isProtein.toString,eventHeadToken.word, y)) -> 1.0 //is protein_first trigger word
    feats += FeatureKey("is protein_first", List(x.isProtein.toString, y)) -> 1.0 //is protein_first

    if (event.begin > 0) {

      val leftEvent = thisSentence.tokens(event.begin-1)
      val rightEvent = thisSentence.tokens(event.begin+1)

      feats += FeatureKey("left unigram event", List(leftEvent.word, y)) -> 1.0 //left unigram event
      feats += FeatureKey("left bigram event", List(leftEvent.word, eventHeadToken.word, y)) -> 1.0 //left bigram event
      feats += FeatureKey("right unigram event", List(rightEvent.word, y)) -> 1.0 //right unigram event
      feats += FeatureKey("right bigram event", List(eventHeadToken.word, rightEvent.word, y)) -> 1.0 //right bigram event
    }

    feats.toMap

  }


}
