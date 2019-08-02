sealed trait Expr {def eval():Double}
case class Val(v:Double) extends Expr {
  override def eval(): Double = v
}
case class Posi(r:Expr) extends Expr {
  override def eval(): Double = r.eval()
}
case class Nega(r:Expr) extends Expr {
  override def eval(): Double = -r.eval()
}
case class Plus(l:Expr, r:Expr) extends Expr {
  override def eval(): Double = l.eval() + r.eval()
}
case class Minus(l:Expr, r:Expr) extends Expr {
  override def eval(): Double = l.eval() - r.eval()
}
case class Mult(l:Expr, r:Expr) extends Expr {
  override def eval(): Double = l.eval() * r.eval()
}
case class Divi(l:Expr, r:Expr) extends Expr {
  override def eval(): Double = l.eval() / r.eval()
}
case class Pow(l:Expr, r:Expr) extends Expr {
  override def eval(): Double = Math.pow(l.eval(),r.eval())
}
case class Exp(r:Expr) extends Expr {
  override def eval(): Double = Math.exp(r.eval())
}
case class Log(r:Expr) extends Expr {
  override def eval(): Double = Math.log(r.eval())
}
case class Sin(r:Expr) extends Expr {
  override def eval(): Double = Math.sin(r.eval())
}
case class Cos(r:Expr) extends Expr {
  override def eval(): Double = Math.cos(r.eval())
}
case class Tan(r:Expr) extends Expr {
  override def eval(): Double = Math.tan(r.eval())
}
case class DvExpr(remaining: String) extends DvToken[DvExpr] {
  import ParseExpr._
  override def make(source: String): DvExpr = DvExpr(source)

  //expr
  lazy val dvExpr:Result[DvExpr, Expr] = dvAdd
  lazy val dvAdd:Result[DvExpr, Expr] = parseAdd.unParser(this)
  lazy val dvAdd$:Result[DvExpr, Option[Expr=>Expr]] = parseAdd$.unParser(this)
  lazy val dvMult:Result[DvExpr, Expr] = parseMult.unParser(this)
  lazy val dvMult$:Result[DvExpr, Option[Expr=>Expr]] = parseMult$.unParser(this)
  lazy val dvUExpr:Result[DvExpr, Expr] = parseUExpr.unParser(this)
  lazy val dvPow:Result[DvExpr, Expr] = parsePow.unParser(this)
  lazy val dvNum:Result[DvExpr, Expr] = parseNum.unParser(this)
}

object ParseExpr {
  import Parser._
  import Tokenizer._
  def parseAdd:Parser[DvExpr, Expr] =
    Parser[DvExpr, Expr](_.dvMult) >>= (e1 =>
      Parser((dv:DvExpr)=>dv.dvAdd$) >>= (e2 =>
        unit(e2.getOrElse(identity[Expr](_))(e1))))
  def parseAdd$:Parser[DvExpr, Option[Expr=>Expr]] =
    (lexeme[DvExpr, Char](char('+')) >>
      Parser(_.dvMult) >>= (e1 =>
      Parser((dv:DvExpr)=>dv.dvAdd$) >>= (e2 =>
        unit(Some((e:Expr)=>e2.getOrElse(identity[Expr](_))(Plus(e, e1)))))))
      .or(
        lexeme[DvExpr, Char](char('-')) >>
          Parser(_.dvMult) >>= (e1 =>
          Parser((dv:DvExpr)=>dv.dvAdd$) >>= (e2 =>
            unit(Some((e:Expr) => e2.getOrElse(identity[Expr](_))(Minus(e, e1)))))))
      .or(unit(None))
  def parseMult:Parser[DvExpr, Expr] =
    Parser[DvExpr, Expr](_.dvUExpr) >>= (e1 =>
      Parser((dv:DvExpr)=>dv.dvMult$) >>= (e2 =>
        unit(e2.getOrElse(identity[Expr](_))(e1))))
  def parseMult$:Parser[DvExpr, Option[Expr=>Expr]] =
    (lexeme[DvExpr, Char](char('*')) >>
      Parser(_.dvUExpr) >>= (e1 =>
      Parser((dv:DvExpr)=>dv.dvMult$) >>= (e2 =>
        unit(Some((e:Expr) => e2.getOrElse(identity[Expr](_))(Mult(e, e1)))))))
      .or(
        lexeme[DvExpr, Char](char('/')) >>
          Parser(_.dvUExpr) >>= (e1 =>
          Parser((dv:DvExpr)=>dv.dvMult$) >>= (e2 =>
            unit(Some((e:Expr) => e2.getOrElse(identity[Expr](_))(Divi(e, e1)))))))
      .or(unit(None))
  def parseUExpr:Parser[DvExpr, Expr] =
    (lexeme[DvExpr, String](strg("+")) >>
      Parser(_.dvUExpr) >>= (e=>
      unit(Posi(e))))
      .or(
        lexeme[DvExpr, String](strg("-")) >>
          Parser(_.dvUExpr) >>= (e=>
          unit(Nega(e))))
      .or(
        lexeme[DvExpr, String](strg("exp")) >>
          Parser(_.dvUExpr) >>= (e=>
          unit(Exp(e))))
      .or(
        lexeme[DvExpr, String](strg("log")) >>
          Parser((dv:DvExpr)=>dv.dvUExpr) >>= (e=>
          unit(Log(e))))
      .or(
        lexeme[DvExpr, String](strg("sin")) >>
          Parser(_.dvUExpr) >>= (e=>
          unit(Sin(e))))
      .or(
        lexeme[DvExpr, String](strg("cos")) >>
          Parser(_.dvUExpr) >>= (e=>
          unit(Cos(e))))
      .or(
        lexeme[DvExpr, String](strg("tan")) >>
          Parser(_.dvUExpr) >>= (e=>
          unit(Tan(e)))
      ) | Parser(_.dvPow)

  def parsePow:Parser[DvExpr, Expr] =
    (Parser[DvExpr, Expr](_.dvNum) >>= (e1 =>
      lexeme[DvExpr, Char](char('^')) >>
        Parser[DvExpr, Expr](_.dvPow) >>= (e2 =>
        unit(Pow(e1,e2))))) | Parser[DvExpr, Expr](_.dvNum)
  def parseNum:Parser[DvExpr, Expr] =
    (lexeme[DvExpr, Char](char('(')) >>
      Parser(_.dvAdd) >>= (e =>
      lexeme[DvExpr, Char](char(')')) >>
        unit(e)))
      .or(
        lexeme(Parser[DvExpr, Double](_.dvReal))>>=(v => unit(Val(v))))
}
