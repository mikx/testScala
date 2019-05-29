package example

import fastparse._
import NoWhitespace._

object MyParser extends App {

  def term[_: P] = P(CharIn("A-Z_a-z").rep.!.map(_.toUpperCase))
  def text[_: P] = P((!"${" ~ AnyChar).rep.!.?).map(_.getOrElse(""))
  def part[_: P] = P(text ~ "${" ~/ term ~ "}" ~ text).map(_.productIterator.mkString)
  def expr[_: P] = P(part.rep).map(_.mkString)

  val Parsed.Success(result, _) = parse("aa $ { ${xxx} bbb ${yyy} ccc ${zzz}", expr(_))
  println(s"'${result}'")


}
