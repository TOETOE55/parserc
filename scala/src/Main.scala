import java.util.Scanner

object Main {
  def main(args: Array[String]): Unit = {
    val scanner = new Scanner(System.in)
    var expr = ""
    while (true) { //input-eval-output loop
      System.out.print(">")
      expr = scanner.nextLine
      try {
        val result = DvExpr(expr).dvExpr.done.value
        System.out.println(result)
        System.out.println(result.eval())
      } catch {
        case ParseErr() =>
          System.out.println("Excepted expression")
      }
    }
  }
}
