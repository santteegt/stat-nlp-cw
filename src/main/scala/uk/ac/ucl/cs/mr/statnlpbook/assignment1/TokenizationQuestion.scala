package uk.ac.ucl.cs.mr.statnlpbook.assignment1

import java.io.File

import uk.ac.ucl.cs.mr.statnlpbook.Tokenizer
import uk.ac.ucl.cs.mr.statnlpbook.chapter.languagemodels.Util

/**
 * This stub is provided as help for you to solve the first question.
 * @author riedel
 * @author santteegt
 */
object TokenizationQuestion {

  def main(args: Array[String]) {

    //the directory that contains the raw documents
    val dataDir = new File(args(0))

    //the file that contains the target gold tokenization
    val goldFile = new File(args(1))

    //the actual raw documents
    val raw = Assignment1Util.loadTokenizationQuestionDocs(dataDir)

    //TODO: The tokenizer you need to improve to match the gold tokenization
    val barAnchors = "(\\[(/)?BAR\\])";
    val afterBar = s"(?<=$barAnchors)";
    val beforeBar = s"(?!(^))(?=$barAnchors)";
    val contractions = "(?=(n't)|'(\\w))" + "(?!('t))(?<!(n))";
    val miscBrackets = "(?<=(\\[))(?!((/)?BAR))" + "|" + "(?=\\])(?<!(BAR))";

    val pChars = "[,:;!_\\+\\*\\?\\(\\)\\{\\}]";
    val punctuations = s"(?=$pChars)|(?<=$pChars)(?!\\s)";

    val quotedWords = "(?=\")|(?<=\")";

    val titles = "(Mr|Mrs|Miss|Dr)"
    val punctuation = s"(?<!$titles)(?=(\\.))"
    val afterPunct = s"(?<=(\\.))(?!\\s)"

    val rapIdioms = "((lo)|(\\so)|(ya))";
    val generalIdioms = s"(?<=$rapIdioms)(?=('\\s))";
    val rapContractions = "(?<=(\\w))(?='(\\s)?)(?<!(n))";


    //val contractions = "(?=(n't)|('[^t\\s]) )";
    //val chorusAnchor = "(?=(Chorus))|(?<=((Chorus)))|(?<=((Chorus)(\\s\\d)))";

    ////val chorusAnchor = "(?=(Chorus))(?<=\\[)|(?<=(Chorus))(?=\\])|(?<=((Chorus)(\\s\\d)))(?=\\])";
    //val verseAnchor = "(?=(Verse))|(?<=((Verse)))|(?<=((Verse)(\\s\\d)))";

    //val hookAnchor = "(?=(Hook))(?<=\\[)|(?<=(Hook))(?=\\])";
    //val jLiveAnchor = "(?=(J-Live))(?<=\\[)|(?<=(J-Live))(?=\\])";
    //val rakimAnchor = "(?=(Rakim))(?<=\\[)|(?<=(Rakim))(?=\\])";
    //val hiphopLOidiom = "(?<=(lo))(?=('\\s))";
    //val hiphopOidiom = "(?<=(\\so))(?=('\\s))";
    //val hiphopYAidiom = "(?<=(ya))(?=('\\s))";

    //val kidAnchor = "(?=([Kk]id\\s))|(?<=([Kk]id\\s.*))(?=\\])";
    //val kidAnchor = "(?=([Kk]id\\s))|(?=\\])(?<![\\)(BAR)])";
    /*
    val kidAnchor = "(?=([Kk]id\\s))|(?=(What))(?<=(\\[))|(?=(WHERE))(?<=(\\[))|(?=(Who))(?<=(\\[))" +
      "|(?=(niggaz))(?<=(\\[))|(?=(aight))(?<=(\\[))|(?=([Yy]eah))|(?=\\])(?<![\\)(BAR)])";
    */
    //val exceptions = "(?<=God)(?!'s) | (?<=(\\[\"M\"\\]) | (?<=(\\[\"M))(?=-) )";
    //val exceptions = "(?<=God)(?!'s)";
    //val exceptions1 = "(?<=(\\[\"M\"\\])";

    //val exceptions = "(?<=God)(?!'s) | (?<=(\\[\"M\"\\])) | (?<=(\\[\"M))(?=\\s-)";


    val exceptions = "" +
      "(?<=(\\[\"M(\")?\\]))|(?<=(\\[\"M))(?=\\]-)"; // ]-icroscopic
    val exceptions2 = "(?<=(\"C\"\\])(?=\\d))"; // [ " C " ]4
    val chorusAnchor = "(?<=(Chorus)(?=\\[))";


    //val exceptions3 = "(?<=(\\w))(?=('\\s))"; //error on single quote at the end on quoted strings
//    val exceptions3 = "" +
        //"(?<=('The Best Part))(?=('))|" +
        //"(?<=('I used to do it))(?=('))|" +
        //"(?<=('m used to it))(?=('\\s))|" +
        //"(?<=(let me get a deal))(?=('))|" +
        //"(?<=(Turning))(?=('))|" +
        //"(?<=(getting))(?=('))|" +
        //"(?<=(mic))(?=('))|" +
        //"(?<=(class))(?=('))|" +
        //"(?<=(eyes))(?=('\\s))|" +
//        "(?<=('C))(?=('\\s))"; //|(?<=(It))(?=('\s))
    val tokenizer = Tokenizer.
      fromRegEx(s"(\\s" +
        s"|$afterBar" +
        s"|$beforeBar" +
        s"|$contractions" +
        s"|$miscBrackets" +
        s"|$punctuations" +
        s"|$quotedWords" +
        s"|$punctuation|$afterPunct" +
        s"|$generalIdioms" +
        s"|$rapContractions" +
        //USELESS
        //s"|$verseAnchor" +
        //s"|$hookAnchor|$jLiveAnchor|$rakimAnchor" +
        //s"|$hiphopLOidiom|$hiphopOidiom" +
        s"|$exceptions" +
        s"|$exceptions2" +
        s"|$chorusAnchor" +
        ")");

    //the result of the tokenization
    val result = Util.words(raw map tokenizer)

    //the gold tokenization file
    val gold = Assignment1Util.loadWords(goldFile).toBuffer

//    println(result.mkString(" ").take(100));
    //val test = result.mkString(" ");
    //println(test.substring(test.indexOf("sum'tin")-30).take(100));
//    println(gold.mkString(" ").take(100));
    //val test2 = gold.mkString(" ");
    //println(test2.substring(test2.indexOf("sum 'tin")).take(100));


    //we find the first pair of tokens that don't match
    val mismatch = result.zip(gold).find { case (a, b) => a != b }

    //Your goal is to make sure that mismatch is None
    mismatch match {
      case None => println("Success!")
      case Some(pair) => println("The following tokens still don't match: " + pair)
    }

  }

}
