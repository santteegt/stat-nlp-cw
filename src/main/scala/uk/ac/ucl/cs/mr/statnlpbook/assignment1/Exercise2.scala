package uk.ac.ucl.cs.mr.statnlpbook.assignment1

import java.io.{FileOutputStream, File}

import scala.collection.mutable.ListBuffer

/**
 * Created by santteegt on 11/1/15.
 */
object Exercise2 {

  val cBAR = "[/BAR]";
  val BAR = "[BAR]";

  def main (args: Array[String]){
    val trainFile = new File("data/assignment1/p2/p2_train.txt");
    val empDistrFile = new File("data/assignment1/p2/empDistr.txt");
    val out = new FileOutputStream("data/assignment1/p2/empDistr.txt");
    val words = Assignment1Util.loadWords(trainFile);
    var empDistr = new ListBuffer[Int]();
    var count = 0;
    //for(i <- words.length to 0 by -1) {
    for(i <- 0 to words.length-1) {
      val token = words(i);
      if(token.equals(BAR)) count = 0;
      else if(token.equals(cBAR)) empDistr += count;
      else count += 1;

    }
    println(empDistr.mkString(","));
    out.write(empDistr.mkString(",").getBytes);
    out.close()


    println("END OF EXERCISE");

  }

}
