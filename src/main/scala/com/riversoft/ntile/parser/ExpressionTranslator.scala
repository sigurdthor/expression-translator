package com.riversoft.ntile.parser

import com.riversoft.ntile.parser.exceptions.ParsingException
import org.parboiled2.{ErrorFormatter, ParseError}

import scala.util.{Failure, Success}

import collection.JavaConverters._

case class EntityInfo(tags: java.util.Set[String], attributes: java.util.Set[String], elements: java.util.Set[String])

case class Result(expression: String, tags: java.util.Set[String], entityInfo: EntityInfo)

object ExpressionTranslator {

  def translate(input: String): Result = {

    require(isNotEmpty(input), "Expression string shouldn't be empty!")

    val parser = new ExpressionParser(input)

    parser.InputLine.run() match {

      case Success(output) =>
        val entityInfo = EntityInfo(
          output.entityUids.getOrElse("tags", Set.empty).asJava,
          output.entityUids.getOrElse("attributes", Set.empty).asJava,
          output.entityUids.getOrElse("elements", Set.empty).asJava)

        Result(output.value, output.tags.asJava, entityInfo)

      case Failure(e: ParseError) => throw new ParsingException(parser.formatError(e, new ErrorFormatter(showTraces = true)))

      case Failure(e: Throwable) => throw e
    }
  }

  def translateWithFulltextPredicate(input: String, pattern: String): String = {

    require(isNotEmpty(input), "Expression string shouldn't be empty!")

    val parser = new ExpressionParser(input, Some(pattern))

    parser.InputLine.run() match {

      case Success(output) => output.value

      case Failure(e: ParseError) => throw new ParsingException(parser.formatError(e, new ErrorFormatter(showTraces = true)))

      case Failure(e: Throwable) => throw e
    }
  }
}



