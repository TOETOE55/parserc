import java.util.Optional;
import java.util.function.Function;

abstract class Expr {
    abstract double eval();
}

class Val extends Expr {
    private final double val;
    public Val(double v) {
        val = v;
    }

    double eval() {
        return val;
    }
}

class Posi extends Expr {
    private final Expr exprR;
    public Posi(Expr r) {
        exprR = r;
    }

    double eval() {
        return exprR.eval();
    }
}

class Nega extends Expr {
    private final Expr exprR;
    public Nega(Expr r) {
        exprR = r;
    }

    double eval() {
        return -exprR.eval();
    }
}

class Plus extends Expr {
    private final Expr exprL;
    private final Expr exprR;
    public Plus(Expr l, Expr r) {
        exprL = l;
        exprR = r;
    }

    double eval() {
        return exprL.eval()+exprR.eval();
    }
}

class Minus extends Expr {
    private final Expr exprL;
    private final Expr exprR;
    public Minus(Expr l, Expr r) {
        exprL = l;
        exprR = r;
    }

    double eval() {
        return exprL.eval() - exprR.eval();
    }
}

class Mult extends Expr {
    private final Expr exprL;
    private final Expr exprR;
    public Mult(Expr l, Expr r) {
        exprL = l;
        exprR = r;
    }

    double eval() {
        return exprL.eval()*exprR.eval();
    }
}

class Divi extends Expr {
    private final Expr exprL;
    private final Expr exprR;
    public Divi(Expr l, Expr r) {
        exprL = l;
        exprR = r;
    }

    double eval() {
        return exprL.eval() / exprR.eval();
    }
}

class Pow extends Expr {
    private final Expr exprL;
    private final Expr exprR;
    public Pow(Expr l, Expr r) {
        exprL = l;
        exprR = r;
    }

    double eval() {
        return Math.pow(exprL.eval(), exprR.eval());
    }
}

class Exp extends Expr {
    private final Expr exprR;
    public Exp(Expr r) {
        exprR = r;
    }

    double eval() {
        return Math.exp(exprR.eval());
    }
}

class Log extends Expr {
    private final Expr exprR;
    public Log(Expr r) {
        exprR = r;
    }

    double eval() {
        return Math.log(exprR.eval());
    }
}

class Sin extends Expr {
    private final Expr exprR;
    public Sin(Expr r) {
        exprR = r;
    }

    double eval() {
        return Math.sin(exprR.eval());
    }
}

class Cos extends Expr {
    private final Expr exprR;
    public Cos(Expr r) {
        exprR = r;
    }

    double eval() {
        return Math.cos(exprR.eval());
    }
}

class Tan extends Expr {
    private final Expr exprR;

    public Tan(Expr r) {
        exprR = r;
    }

    double eval() {
        return Math.tan(exprR.eval());
    }
}

//Parser of expression
/*
 * Expr  ::= Expr '+' Mult | Expr '-' Mult | Mult
 * Mult  ::= Mult '*' UExpr | Mult '/' UExpr | UExpr
 * UExpr ::= unaryOperators UExpr | Pow
 * Pow   ::= Num '^' Pow | Num
 * Num   ::= '(' Expr ')' | Real
 *
 * ->removing left recursion
 *
 * Expr  ::= Mult Expr_
 * Expr_ ::= '+' Mult Expr_|'-' Mult Expr_|empty
 * Mult  ::= UExpr Mult_
 * Mult_ ::= '*' UExpr Mult_|'/' UExpr Mult_|empty
 * UExpr ::= unaryOperators UExpr | Pow
 * Pow ::= Num '^' Pow | Num
 * Num ::= (Expr) | Real
 *
 * */
class ParserExpr {
    //Expr ::= Mult Expr_
    static Parser<Expr> parseExpr() {
        return  parseMult().bind(e1->
                parseExpr_().bind(e2->
                        Parser.unit(e2.orElse(Function.identity())
                                .apply(e1))));
    }
    //Expr_ ::= '+' Mult Expr_|'-' Mult Expr_|empty
    static Parser<Optional<Function<Expr, Expr>>> parseExpr_() {
        return  Space.lexeme(Char.eat('+')).bind(c->
                parseMult().bind(e1->
                        parseExpr_().bind(e2->
                                Parser.unit(Optional.<Function<Expr, Expr>>of(_e->
                                        e2.orElse(Function.identity())
                                                .apply(new Plus(_e, e1))))))).attempt()
                .or(
                        Space.lexeme(Char.eat('-')).bind(c->
                                parseMult().bind(e1->
                                        parseExpr_().bind(e2->
                                                Parser.unit(Optional.<Function<Expr, Expr>>of(_e->
                                                        e2.orElse(Function.identity())
                                                                .apply(new Minus(_e, e1)))))))).attempt()

                .or(Parser.unit(Optional.empty()));
    }
    //Mult ::= UExpr Mult_
    static Parser<Expr> parseMult() {
        return  parseUExpr().bind(e1->
                parseMult_().bind(e2->
                        Parser.unit(e2.orElse(Function.identity())
                                .apply(e1))));
    }
    //Mult_ ::= '*' UExpr Mult_|'/' UExpr Mult_|empty
    static Parser<Optional<Function<Expr, Expr>>> parseMult_() {
        return  Space.lexeme(Char.eat('*')).bind(c->
                parseUExpr().bind(e1->
                        parseMult_().bind(e2->
                                Parser.unit(Optional.<Function<Expr, Expr>>of(_e->
                                        e2.orElse(Function.identity())
                                                .apply(new Mult(_e, e1))))))).attempt()
                .or(
                        Space.lexeme(Char.eat('/')).bind(c->
                                parseUExpr().bind(e1->
                                        parseMult_().bind(e2->
                                                Parser.unit(Optional.<Function<Expr, Expr>>of(_e->
                                                        e2.orElse(Function.identity())
                                                                .apply(new Divi(_e, e1)))))))).attempt()

                .or(Parser.unit(Optional.<Function<Expr, Expr>>empty()));
    }
    //UExpr ::= unaryOperators UExpr | Pow
    static Parser<Expr> parseUExpr() {
        return  Space.lexeme(Strg.eatOnly("+")).<Expr>bind(u->
                parseUExpr().bind(e->
                        Parser.unit(new Posi(e)))).attempt()
                .or(
                        Space.lexeme(Strg.eatOnly("-")).<Expr>bind(u->
                                parseUExpr().bind(e->
                                        Parser.unit(new Nega(e))))).attempt()
                .or(
                        Space.lexeme(Strg.eatOnly("exp")).<Expr>bind(u->
                                parseUExpr().bind(e->
                                        Parser.unit(new Exp(e))))).attempt()
                .or(
                        Space.lexeme(Strg.eatOnly("log")).<Expr>bind(u->
                                parseUExpr().bind(e->
                                        Parser.unit(new Log(e))))).attempt()
                .or(
                        Space.lexeme(Strg.eatOnly("sin")).<Expr>bind(u->
                                parseUExpr().bind(e->
                                        Parser.unit(new Sin(e))))).attempt()
                .or(
                        Space.lexeme(Strg.eatOnly("cos")).<Expr>bind(u->
                                parseUExpr().bind(e->
                                        Parser.unit(new Cos(e))))).attempt()
                .or(
                        Space.lexeme(Strg.eatOnly("tan")).<Expr>bind(u->
                                parseUExpr().bind(e->
                                        Parser.unit(new Tan(e))))).attempt()

                .or(parsePow());
    }
    //Pow ::= Num '^' Pow | Num
    static Parser<Expr> parsePow() {
        return  parseNum().<Expr>bind(num->
                Char.eat('^').bind(c->
                        parsePow().bind(e->
                                Parser.unit(new Pow(num, e))))).attempt()

                .or(parseNum());
    }
    //Num ::= (Expr) | Real
    static Parser<Expr> parseNum() {
        return  Char.eat('(').bind(c1->
                parseExpr().bind(e->
                        Char.eat(')').bind(c2->
                                Parser.unit(e)))).attempt()
                .or(
                        Real.eat().bind(num->
                                Parser.unit(new Val(num))));
    }
}