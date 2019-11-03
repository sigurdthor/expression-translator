package com.riversoft.ntile.parser

import java.util

import org.scalatest._

import scala.collection.immutable.Stream.Empty

class ExpressionTranslatorSpec extends FeatureSpec with GivenWhenThen with Matchers {

  feature("Expression translator") {
    scenario("Translate composite query with and & or & not") {

      Given("Expression with and & or ")

      val expression = "(#oleg or not #petya) and not (#vasya or #kolya)"
      val tags = new java.util.HashSet[String](util.Arrays.asList("oleg", "petya", "vasya", "kolya"))

      When(s"Expression ${expression} is translated")

      val query = ExpressionTranslator.translate(expression)

      Then(s"Query should be ${query}")

      query.expression should include("tags")
      query.tags should contain("oleg")
      query.entityInfo.tags should be(tags)
    }

    scenario("Translate composite query with && and || and !") {

      Given("Expression with && and || and !")

      val expression = "(#oleg||!#petya)&&!(#vasya && #kolya)"
      val tags = new java.util.HashSet[String](util.Arrays.asList("oleg", "petya", "vasya", "kolya"))

      When(s"Expression ${expression} is translated")

      val query = ExpressionTranslator.translate(expression)

      Then(s"Query should be ${query}")

      query.expression should include("tags")
      query.tags should contain("oleg")
      query.entityInfo.tags should be(tags)
    }

    scenario("Translate single tag") {

      Given("Expression with tag")

      val expression = "#oleg"
      val tags = new java.util.HashSet[String](util.Arrays.asList("oleg"))

      When(s"Expression ${expression} is translated")

      val query = ExpressionTranslator.translate(expression)

      Then(s"Query should be ${query} should contains tags")

      query.expression should include("tags")
      query.tags should contain("oleg")
      query.entityInfo.tags should be(tags)
    }

    scenario("Translate with attribute") {

      Given("Expression with attributes")

      val expression = "#bugs::(exec = (\"5b20f96c15a2775e63dc5e23\" && \"5b20f96c15a2775e63dc5c23\") && status != \"done\") && !#editor"
      val tags = new java.util.HashSet[String](util.Arrays.asList("bugs", "editor"))
      val attrs = new java.util.HashSet[String](util.Arrays.asList("exec", "status"))
      val elements = new java.util.HashSet[String](util.Arrays.asList("5b20f96c15a2775e63dc5e23", "5b20f96c15a2775e63dc5c23"))

      When(s"Expression ${expression} is translated")

      val query = ExpressionTranslator.translate(expression)

      Then(s"Query should be ${query}")

      query.expression should include("tags")
      query.tags should contain("bugs")
      query.entityInfo.tags should be(tags)
      query.entityInfo.attributes should be(attrs)
      query.entityInfo.elements should be(elements)
    }

    scenario("Translate with attribute without braces") {

      Given("Expression with attributes")

      val expression = "#bugs::exec=\"5b20f96c15a2775e63dc5e23\" && #editor"
      val tags = new java.util.HashSet[String](util.Arrays.asList("bugs", "editor"))
      val attrs = new java.util.HashSet[String](util.Arrays.asList("exec"))
      val elements = new java.util.HashSet[String](util.Arrays.asList("5b20f96c15a2775e63dc5e23"))

      When(s"Expression ${expression} is translated")

      val query = ExpressionTranslator.translate(expression)

      Then(s"Query should be ${query}")

      query.expression should include("tags")
      query.tags should contain("bugs")
      query.tags should contain("editor")
      query.entityInfo.tags should be(tags)
      query.entityInfo.attributes should be(attrs)
      query.entityInfo.elements should be(elements)
    }

    scenario("Translate repeatable attributes") {

      Given("Expression with attributes")

      val expression = "#bugs::(exec = \"kfkfkf\")&&#bugs::(status!=\"5b20f96c15a2775e63dc5e23\")"
      val tags = new java.util.HashSet[String](util.Arrays.asList("bugs"))
      val attrs = new java.util.HashSet[String](util.Arrays.asList("exec", "status"))
      val elements = new java.util.HashSet[String](util.Arrays.asList("5b20f96c15a2775e63dc5e23"))

      When(s"Expression ${expression} is translated")

      val query = ExpressionTranslator.translate(expression)

      Then(s"Query should be ${query}")

      query.expression should include("tags")
      query.tags should contain("bugs")
      query.entityInfo.tags should be(tags)
      query.entityInfo.attributes should be(attrs)
      query.entityInfo.elements should be(elements)
    }

    scenario("Translate with attribute and || expression") {

      Given("Expression with attributes and ||")

      val expression = "#bugs::(exec!=\"5b20f96c15a2775e63dc5e23\")"
      val tags = new java.util.HashSet[String](util.Arrays.asList("bugs"))
      val attrs = new java.util.HashSet[String](util.Arrays.asList("exec"))
      val elements = new java.util.HashSet[String](util.Arrays.asList("5b20f96c15a2775e63dc5e23"))

      When(s"Expression ${expression} is translated")

      val query = ExpressionTranslator.translate(expression)

      Then(s"Query should be ${query}")

      query.expression should include("tags")
      query.tags should contain("bugs")
      query.entityInfo.tags should be(tags)
      query.entityInfo.attributes should be(attrs)
      query.entityInfo.elements should be(elements)
    }

    scenario("Translate with cyrillic symbols") {

      Given("Expression with attributes and ||")

      val expression = "#bugs::(exec=\"вася пупкин\")"
      val tags = new java.util.HashSet[String](util.Arrays.asList("bugs"))
      val attrs = new java.util.HashSet[String](util.Arrays.asList("exec"))

      When(s"Expression ${expression} is translated")

      val query = ExpressionTranslator.translate(expression)

      Then(s"Query should be ${query}")

      query.expression should include("exec")
      query.tags should contain("bugs")
      query.entityInfo.tags should be(tags)
      query.entityInfo.attributes should be(attrs)
      query.entityInfo.elements should be('empty)
    }

    scenario("Translate with greateror equals") {

      Given("Expression with greater")

      val numb = 65.98
      val expression = s"""#bugs::(exec >= "${numb}")"""
      val tags = new java.util.HashSet[String](util.Arrays.asList("bugs"))
      val attrs = new java.util.HashSet[String](util.Arrays.asList("exec"))

      When(s"Expression ${expression} is translated")

      val query = ExpressionTranslator.translate(expression)

      Then(s"Query should be ${query}")

      query.expression should include("exec")
      query.tags should contain("bugs")
      query.entityInfo.tags should be(tags)
      query.entityInfo.attributes should be(attrs)
      query.entityInfo.elements should be('empty)
    }

    scenario("Translate with fulltext predicate") {

      Given("Expression with fulltext")

      val expression = "#oleg"
      val pattern = "elem"

      When(s"Expression ${expression} is translated")

      val queryExpression = ExpressionTranslator.translateWithFulltextPredicate(expression, pattern)

      Then(s"Query should be ${queryExpression}")

      queryExpression should include("oleg")
      queryExpression should include(pattern)
    }

  }
}
