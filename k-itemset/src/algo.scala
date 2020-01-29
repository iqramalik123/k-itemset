
import scala.io.Source
import scala.util.control.Breaks._
import java.io.File
import java.io.PrintWriter

object algo {
  def main(args:Array[String]){
    // file creat
  val file= new File("abc.txt")
  //file write
  val print= new PrintWriter(file)
  print.write("Bread,Butter,egg\n")
  print.write("Butter,egg,oil\n")
  print.write("Butter,egg") 
  print.close()
  val fname ="abc.txt"
  var trans1 : List[Set[String]] = List()
  var setitem : Set[String] = Set()
  println("***Transection set ****\n")
 // fetch data from file
  for (line<-Source.fromFile("abc.txt").getLines()) {
    
    println(line)
    // split string
    val setofelement = line.trim.split(',').toSet
    //check empty
    if (setofelement.size > 0) {
      trans1 = trans1 :+ setofelement
      setitem = setitem ++ setofelement
    }
  }
  var itemtoRet : Map[Set[String],Double] = Map()
  var rulesofassociation : List[(Set[String], Set[String], Double)] = List()



def Aprioriruning(minimumSupport : Double = 0.5, minimumConfidence : Double = 0.6) = {
    // store combination of item
  var combitem : Set[(Set[String], Double)] = Set()
  //first candidate
  var currentCSet : Set[Set[String]] = setitem.map( x => Set(x) )
  var k : Int = 2
  breakable {
    while (true) {
      //support countof each item and check minimum support
      val currentItemCombs : Set[(Set[String], Double)] = currentCSet.map( x => (x, togetSupp(x)))
                                        .filter(y => (y._2 >= minimumSupport))
                                    
      val currentLSet = currentItemCombs.map(y => y._1).toSet
      //check currentlset is empty
      if (currentLSet.isEmpty) break
      //if not empty then
      //join item
      currentCSet = currentLSet.map( x => currentLSet.map(y => x | y))
                                                          .reduceRight( (set1, set2) => set1 | set2)
                                                          .filter( wordSet => (wordSet.size==k))
     combitem = combitem | currentItemCombs
    
      k += 1
    }
  }
  for (item <- combitem) {
    itemtoRet += (item._1 -> item._2)
  }
  AssociationRulecalculate(minimumConfidence)
}
  def togetSupp(combitem : Set[String]) : Double = {
    val counter = trans1.filter(transaction => combitem.subsetOf(transaction)).size
    counter.toDouble / trans1.size.toDouble
  }

  def AssociationRulecalculate(minConfidence : Double = 0.6) = {
    itemtoRet.keys.foreach(item =>
      item.subsets.filter( wordSet => (wordSet.size<item.size & wordSet.size>0))
          .foreach( subset => {rulesofassociation = rulesofassociation :+ (subset, item diff subset,
                                                                       itemtoRet(item).toDouble/itemtoRet(subset).toDouble)
                              }
                  )
    )
    rulesofassociation = rulesofassociation.filter( rule => rule._3>=minConfidence)
  }
  Aprioriruning(0.5, 0.6);
  println(" ***Support of items ***")
  println()
  itemtoRet.foreach(println)
  println("***rules of Association *** ")
   println()
  rulesofassociation.foreach(println)


  }

  
}