case class ParseErr() extends Throwable
case class Result[Dv<:Derivs[Dv], +A](value: A, nextDv: Dv) {
  def done:Result[Dv, A] =
    if (nextDv.remaining.isEmpty)
      this
    else throw ParseErr()
}
trait Derivs[Dv<:Derivs[Dv]] { this:Dv =>
  val remaining: String
  def make(source: String):Dv
}

case class Parser[Dv<:Derivs[Dv], +A](unParser: Dv=>Result[Dv, A]) {
  def or[B>:A](p: Parser[Dv, B]):Parser[Dv, B] = Parser(dv=>
    try {
      this.unParser(dv)
    } catch {
      case ParseErr() => p.unParser(dv)
    })
  def bind[B](f:A=>Parser[Dv, B]):Parser[Dv, B] = Parser(dv=> {
    val Result(a, newDv) = this.unParser(dv)
    f(a).unParser(newDv)
  })
  def next[B](q: =>Parser[Dv, B]):Parser[Dv, B] = this bind (_=>q)

  //Operation
  def |[B>:A](p: Parser[Dv, B]):Parser[Dv, B] = this or p
  def >>[B](q: =>Parser[Dv, B]):Parser[Dv, B] = this next q
  def >>=[B](f:A=>Parser[Dv, B]):Parser[Dv, B] = this bind f
}

object Parser {
  def unit[Dv<:Derivs[Dv], A](a:A):Parser[Dv, A] = Parser(dv=>Result(a,dv))
  def fail[Dv<:Derivs[Dv], A]:Parser[Dv, A] = throw ParseErr()
  def many[Dv<:Derivs[Dv], A](p: => Parser[Dv, A]):Parser[Dv, List[A]] =
    some(p) | unit(List.empty)
  def some[Dv<:Derivs[Dv], A](p: => Parser[Dv, A]):Parser[Dv, List[A]] =
    p >>= (a => many(p) >>= (as => unit(a+:as)))
  /*def choice[Dv<:Derivs[Dv], A](p: List[Parser[Dv, A]]):Parser[Dv, A] =
    p.foldRight(fail[Dv, A])(_.or(_))*/
}


//
trait DvToken[Dv<:DvToken[Dv]] extends Derivs[Dv] {
  this:Dv=>
  import Parser._
  import Tokenizer._

  lazy val dvChar:Result[Dv, Char] =
    if (remaining.isEmpty)
      throw ParseErr()
    else
      Result(remaining.charAt(0), make(remaining.substring(1)))
  lazy val dvDigit:Result[Dv, Int] = digit.unParser(this)
  lazy val dvInt:Result[Dv, Int] = int.unParser(this)
  lazy val dvReal:Result[Dv, Double] = real.unParser(this)
  lazy val dvSpace:Result[Dv, Unit] = (many[Dv,Char](char(' ')) >> unit(())).unParser(this)
}


object Tokenizer {
  import Parser._

  def char[Dv<:DvToken[Dv]](c: Char):Parser[Dv, Char] = Parser(dv=>
    if (dv.dvChar.value == c)
      Result(c, dv.dvChar.nextDv)
    else
      throw ParseErr())
  def digit[Dv<:DvToken[Dv]]:Parser[Dv, Int] = Parser(dv=>{
    val rd = dv.dvChar
    if ('0'<=rd.value && rd.value<='9')
      Result(rd.value-'0', rd.nextDv)
    else
      throw ParseErr()
  })
  def int[Dv<:DvToken[Dv]]:Parser[Dv, Int] = Parser (dv => {
    var v = 0
    var d = dv
    v = d.dvDigit.value
    d = d.dvDigit.nextDv
    try {
      while(true) {
        v = 10*v + d.dvDigit.value
        d = d.dvDigit.nextDv
      }
    } catch {
      case ParseErr() => ()
    }
    Result(v, d)
  })
  def real[Dv<:DvToken[Dv]]:Parser[Dv, Double] =
    (Parser[Dv,Int](_.dvInt) >>= (i1 =>
      char('.') >>
        (Parser[Dv,Int](_.dvInt) >>= (i2 =>
          unit((i1.toString + '.' + i2.toString).toDouble))))).
      |(int >>= (i=>unit(i.toDouble)))
  def strg[Dv<:DvToken[Dv]](s:String):Parser[Dv, String] =
    if (s.isEmpty)
      unit(s)
    else
      char(s.charAt(0)) >>= (c =>
        strg[Dv](s.substring(1)) >>= (ss =>
          unit(c+ss)))
  def lexeme[Dv<:DvToken[Dv], A](p: => Parser[Dv, A]):Parser[Dv, A] =
    Parser[Dv, Unit](_.dvSpace)  >> p >>= (a => Parser[Dv, Unit](_.dvSpace) >> unit(a))
}

