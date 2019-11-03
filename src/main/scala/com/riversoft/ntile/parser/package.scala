package com.riversoft.ntile

import com.typesafe.scalalogging.Logger
import org.parboiled2.CharPredicate
import org.slf4j.LoggerFactory

package object parser {

  val log = Logger(LoggerFactory.getLogger("expression-translator"))

  type EntityData = Map[String, Set[String]]


  sealed trait OutputExpression extends ASTNode {

    val expr: ASTNode

    override val tags: Set[String] = expr.tags

    override val entityUids = expr.entityUids
  }

  sealed trait ASTNode {

    def value: String

    var attr: String = ""

    val tags: Set[String] = Set.empty

    val entityUids: EntityData
  }

  sealed trait LogicalPredicateNode extends ASTNode {

    val left: ASTNode
    val right: ASTNode

    override val tags: Set[String] = left.tags ++ right.tags

    override val entityUids = left.entityUids ++ right.entityUids.map {
      case (k, v) => k -> (v ++ left.entityUids.getOrElse(k, Set.empty))
    }
  }

  case class SubAttributesExpressionNode(attribute: String, sign: String, values: ASTNode) extends ASTNode {

    injectAttributeToDescendantNodes

    def value = s"""{ $$and: [{$$ne : ["$$attributes.${attribute.trim}.targets", undefined]}, ${equalPredicate(sign, values.value)}] }"""

    override val entityUids = values.entityUids + ("attributes" -> Set(attribute))

    private def equalPredicate(sign: String, value: String) = sign match {
      case Equals => s"${value.replace("%PLCHLDR%", "$eq")}"
      case NotEquals => s"{ $$not : ${value.replace("%PLCHLDR%", "$eq")}}"
      case Greater => s"${value.replace("%PLCHLDR%", "$gt")}"
      case GreaterOrEq => s"${value.replace("%PLCHLDR%", "$gte")}"
      case Lower => s"${value.replace("%PLCHLDR%", "$lt")}"
      case LowerOrEq => s"${value.replace("%PLCHLDR%", "$lte")}"
    }

    private def injectAttributeToDescendantNodes = values match {
      case v: ValueNode => v.attr = attribute
      case p: LogicalPredicateNode => p.left.attr = attribute
        p.right.attr = attribute
      case _ => log.error("Unexpected AST node for attribute injection")
    }
  }

  case class HashTagWithAttributesNode(hashTag: ASTNode, attrs: ASTNode) extends ASTNode {

    def value = if (attrs != null) s"{ $$and: [${hashTag.value},${attrs.value}]}" else hashTag.value

    override val tags: Set[String] = hashTag.tags

    override val entityUids = hashTag.entityUids ++ attrs.entityUids
  }

  case class HashTagNode(tag: String) extends ASTNode {
    def value = s"""{ $$and: [{$$ne : ["$$tags", undefined]}, { $$in : ["${tag.trim}", "$$tags"] }] }"""

    override val tags: Set[String] = Set(tag)

    override val entityUids = Map("tags" -> Set(tag))
  }

  case class QueryExpression(expr: ASTNode) extends OutputExpression {
    def value = s"""{$$expr : ${expr.value}}"""
  }

  case class QueryWithFulltextExpression(expr: ASTNode, pattern: String) extends OutputExpression {
    def value = s"""{$$and : [{$$expr : ${expr.value}}, {title: {$$regex: /^$pattern/i}}]}"""
  }

  case class ValueNode(v: String) extends ASTNode {

    private lazy val numericValue = if (isNumeric(v)) v.trim else s""" "${v.trim}" """

    def value = s"""{$$switch: {branches: [{case: {$$in: ["$$attributes.$attr.type", ["NUMBER", "DATE"]]}, then: {%PLCHLDR% : ["$$attributes.$attr.numericTarget", $numericValue]}}], default: {$$in: ["${v.trim}", "$$attributes.$attr.targets"]}}}"""

    override val entityUids = {
      val elements: Set[String] = if (isLink(v)) Set(v) else Set.empty
      Map("elements" -> elements)
    }
  }

  case class SubExpressionNode(not: Boolean, expr: ASTNode) extends ASTNode {

    def value = if (not) s"{ $$not : ${expr.value}}" else expr.value

    override val tags: Set[String] = if (not) Set.empty else expr.tags

    override val entityUids = expr.entityUids
  }

  case class AndExpressionNode(left: ASTNode, right: ASTNode) extends LogicalPredicateNode {
    def value = s"{ $$and: [${left.value},${right.value}]}"
  }

  case class OrExpressionNode(left: ASTNode, right: ASTNode) extends LogicalPredicateNode {
    def value = s"{ $$or: [${left.value},${right.value}]}"
  }

  def isNotEmpty(x: String) = !Option(x).forall(_.isEmpty)

  def isNumeric(input: String): Boolean = input.matches("""[+-]?((\d+(e\d+)?[lL]?)|(((\d+(\.\d*)?)|(\.\d+))(e\d+)?[fF]?))""")

  def isLink(input: String): Boolean = input.matches("""^[a-f\d]{24}$""")


  val WhitespaceChars = "\n\t "

  val Hash = '#'

  val LeftBrace = '('

  val RightBrace = ')'

  val Equals = "="

  val Greater = ">"

  val Lower = "<"

  val GreaterOrEq = ">="

  val LowerOrEq = "<="

  val NotEquals = "!="

  val Apostrophe = '"'

  val AllowedChars = CharPredicate.AlphaNum

  val Dividers = ".,"

  val AdditionalValueChars = "!@?/:;$%*&+-_"

  val LowerCyrillic = CharPredicate('а' to 'я')
  val UpperCyrillic = CharPredicate('А' to 'Я')
  val Cyrillic = LowerCyrillic ++ UpperCyrillic

  val AllowedValueChars = CharPredicate.AlphaNum ++ WhitespaceChars ++ Cyrillic ++ Dividers ++ AdditionalValueChars

}
