import scala.io.Codec

object Converter extends App {

  case class FoodStuff(food: String, mainCat: String, subCat: String, giGlucose: String, giBread: String,
    subjects: String, refFood: String, ref: String, serveSize: String, ch: String, gl: String) {

    override def toString: String =
      s"""["$food", "$mainCat", "$subCat", "$giGlucose", "$giBread", "$serveSize", "$ch", "$gl"]"""
  }

  object FoodStuff {
    def apply(str: String, mainCat: String, subCat: String): FoodStuff = {
      val splitted = str.split('|')
      require(splitted.size == 10, s"wrong column count: ${splitted.size}, str: $str")
      FoodStuff(splitted(1), mainCat, subCat, splitted(2), splitted(3), splitted(4),
        splitted(5), splitted(6), splitted(7), splitted(8), splitted(9))
    }
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  val source = scala.io.Source.fromFile("../data.csv")(Codec.UTF8)
  val lines = source.getLines

  var mainCategory: String = ""
  var subCategory: String = ""

  val foodstuffs = lines.map(_.trim).flatMap { line =>
    if (line.startsWith("#####")) { // main-category
      mainCategory = line.substring(5).trim
      subCategory = ""
      None
    } else if (line.startsWith("###")) { // sub-category
      subCategory = line.substring(3).trim
      None
    } else if (line.isEmpty || line.startsWith("#")) { // empty or comment
      None
    } else { // real data
      val foodStuff = FoodStuff(line, mainCategory, subCategory)
      Some(foodStuff)
    }
  }

  printToFile(new java.io.File("../data.json")) { p =>
    p.println("{")
    p.println("\t\"data\": [")
    p.println(foodstuffs.mkString(",\n"))
    p.println("\t]")
    p.println("}")
  }
}
