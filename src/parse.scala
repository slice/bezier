package zone.slice.bezier

import fastparse.{parse => parsley, _}
import NoWhitespace._

sealed trait Expr
object Expr {
  final case class Int(value: scala.Int)                    extends Expr
  final case class Str(value: String)                       extends Expr
  final case class Block(exprs: Seq[Expr])                  extends Expr
  final case class Call(name: AST.Ident, params: Seq[Expr]) extends Expr
}

sealed trait AST
object AST {
  final case class Ident(name: String) extends AST
  final case class Funcdef(name: Ident, params: Seq[Ident], value: Expr)
      extends AST
  final case class Directive(code: Ident) extends AST
}

object parse {
  // whitespace, separators, and newlines
  def sp[_: P]: P[Unit]             = P(CharIn(" \t"))
  def nl[_: P]: P[Unit]             = P(CharIn("\n\r"))
  def ws[_: P]: P[Unit]             = P(sp | nl)
  def nlws[_: P]: P[Unit]           = P(nl ~ sp.rep)
  def com[_: P]: P[Unit]            = P("," ~ ws.?)
  def blockSeparator[_: P]: P[Unit] = P(("\n" | ".") ~ ws.rep.?)

  // tokens
  def ident[_: P]: P[AST.Ident] =
    P(CharsWhileIn("a-zA-Z'!?-_0-9").!)
      .map(AST.Ident(_))
  def digits[_: P]: P[Int] =
    P(CharsWhileIn("0-9").!)
      .map(_.toInt)

  def exprlist[_: P]: P[Seq[Expr]] =
    P(expr.rep(sep = com))
  def identlist[_: P]: P[Seq[AST.Ident]] =
    P(ident.rep(sep = com))

  // expressions
  def int[_: P]: P[Expr.Int] = P(digits).map(Expr.Int(_))
  def str[_: P]: P[Expr.Str] =
    P("\"" ~ CharPred(_ != '"').rep.! ~ "\"")
      .map(Expr.Str(_))
  def fncall[_: P]: P[Expr.Call] =
    P(ident ~ "(" ~ exprlist ~ ")")
      .map {
        case (name, params) => Expr.Call(name, params)
      }
  def expr[_: P]: P[Expr] =
    P(
      (int | fncall | str)
        .map(Seq(_)) | ("{" ~ ws.rep ~ expr
        .rep(sep = blockSeparator) ~ ws.rep ~ "}"),
    ).map {
      case seq if seq.size == 1 => seq.head
      case seq                  => Expr.Block(seq)
    }

  // definitions
  def directive[_: P] = P("@" ~ ident).map(AST.Directive(_))
  def funcdef[_: P]: P[AST.Funcdef] =
    P(ident ~ "(" ~ identlist ~ ")" ~ sp ~ "=" ~ sp ~ expr)
      .map(AST.Funcdef.tupled)
  def defn[_: P]: P[AST] = P(directive | funcdef)

  def program[_: P]: P[Seq[AST]] =
    P(ws.rep ~ defn.rep(min = 1, sep = nl) ~ ws.rep ~ End)

  def apply(sourceText: String): Unit =
    parsley(sourceText, program(_)) match {
      case Parsed.Success(result, _) =>
        println(s"parsed = $result")
      case failure @ Parsed.Failure(_, _, _) =>
        val trace = failure.trace()
        println(s"failed to parse: ${trace.longMsg}")
    }
}
