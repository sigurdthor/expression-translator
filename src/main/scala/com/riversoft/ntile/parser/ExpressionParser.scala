package com.riversoft.ntile.parser

import org.parboiled2._


class ExpressionParser(val input: ParserInput,
                       val maybePattern: Option[String] = None) extends Parser {


  def InputLine: Rule1[ASTNode] = rule {
    Expression ~ EOI ~> ((node: ASTNode) =>
      maybePattern.fold[OutputExpression](QueryExpression(node)) { pattern =>
        QueryWithFulltextExpression(node, pattern)
      })
  }

  def Expression: Rule1[ASTNode] = rule {
    SubExpression ~ zeroOrMore(
      AndPredicate ~ SubExpression ~> AndExpressionNode
        | OrPredicate ~ SubExpression ~> OrExpressionNode)
  }

  def SubExpression: Rule1[ASTNode] = rule {
    NotPredicate ~ (HashTagWithAttributes | LeftBrace ~ Expression ~ RightBrace) ~> SubExpressionNode
  }

  def NotPredicate = rule {
    OptWs ~ capture(optional("not" | "!")) ~ OptWs ~> ((not: String) => !not.isEmpty)
  }

  def AndPredicate = rule {
    OptWs ~ ("and" | "&&") ~ OptWs
  }

  def OrPredicate = rule {
    OptWs ~ ("or" | "||") ~ OptWs
  }

  def AttributesExpression: Rule1[ASTNode] = rule {
    SubAttributesExpression ~ zeroOrMore(
      AndPredicate ~ SubAttributesExpression ~> AndExpressionNode
        | OrPredicate ~ SubAttributesExpression ~> OrExpressionNode)
  }

  def ValueExpression: Rule1[ASTNode] = rule {
    Value ~ zeroOrMore(
      AndPredicate ~ Value ~> AndExpressionNode
        | OrPredicate ~ Value ~> OrExpressionNode)
  }

  def Value = rule {
    Apostrophe ~ capture(oneOrMore(AllowedValueChars)) ~ Apostrophe ~> ValueNode
  }

  def SubAttributesExpression: Rule1[ASTNode] = rule {
    capture(oneOrMore(AllowedChars)) ~ OptWs ~ capture(GreaterOrEq | LowerOrEq | Equals | NotEquals | Greater | Lower) ~
      OptWs ~ (Value | LeftBrace ~ ValueExpression ~ RightBrace) ~> SubAttributesExpressionNode
  }

  def HashTagWithAttributes = rule {
    HashTag ~ optional("::" ~ zeroOrMore(LeftBrace) ~ AttributesExpression ~ zeroOrMore(RightBrace) ~> HashTagWithAttributesNode)
  }

  def HashTag: Rule1[ASTNode] = rule {
    Hash ~ capture(oneOrMore(AllowedChars)) ~> HashTagNode
  }

  def WhiteSpace = rule {
    anyOf(WhitespaceChars)
  }

  def OptWs = rule {
    zeroOrMore(WhiteSpace)
  }

  def Ws = rule {
    oneOrMore(WhiteSpace)
  }
}
