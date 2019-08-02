import java.util.Scanner;


public class Main {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        String expr = "";
        for (;;) {
            //input-eval-output loop
            System.out.print(">");
            expr = scanner.nextLine();
            try {
                double result = ParserExpr
                        .parseExpr().done()
                        .run(expr)
                        .getResult()
                        .eval();
                System.out.println(result);
            } catch (ParseError e) {
                System.out.println("Excepted expression");
            }
        }

    }
}
