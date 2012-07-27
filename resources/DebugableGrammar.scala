package com.github.hubertp.parserexperiments

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

import scala.util.parsing.combinator.debugging
import scala.util.parsing.combinator.debugging.Controllers
import debugging.ParserMacros._  

object DebugableGrammar extends DebugableTest {

  def main(args: Array[String]) {
    //val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
    //val tokens = new lexical.Scanner("Blip Blop Blap Blap Blap")
    val tokens = new lexical.Scanner("succ succ succ of succ zero")
    //val tokens = new lexical.Scanner("succ succ succ zero")
    val mainParser = phrase(Term)
    mainParser(tokens) match {
      case Success(trees, _) =>
        try {
          println("Parsed as: " + trees)
        } catch {
          case tperror => println(tperror.toString)
        }
      case e =>

        println(e)
    }
  }
}

trait DebugableTest extends StandardTokenParsers with Controllers {

  def runMain(c : Controller) : Unit = {
    registerController(c)
    DebugableGrammar.main(Array(""))
  }


  lexical.delimiters ++= List("(", ")", "{", "}", ",", "*", "+")
  lexical.reserved   ++= List("true", "false", "succ",
                              "pred", "iszero", "zero", "of", "Mip", "Mup", "Map", "Blip", "Blop", "Blap", "Blup")
  
  def Term(implicit loc0: debugging.ParserLocation): Parser[Term] = (
    BoolTerm
    //rep1("succ") ^^^ True
    // | SimpleChurchNumTerm
     | "Blop" ~> "Blop" ~> "Blop" ^^^ True
     // rep1("Blop") ~ rep("nBlap") ^^^ True
     | "Blip" ~ ("Blup" | "Blop") ~ "Blap" ~ "Blap" ~ "Blup" ^^^ True
     | "Blip" ~ bopbop ~ "Blap" ~ "Blap" ~ "Blip" ^^^ True
    // //| rep("Blip") ~ "Blop" ~ rep("Blap") ~ "Blip" ^^^ False
    // | "Blip" ~ tjah ~ "Blap" ~ "Blap" ^^^ True
    // | IsZeroTerm
  )

  def bopbop(implicit loc0: debugging.ParserLocation) : Parser[Term] = (
      "Mip" ^^^ True
    | "Mup" ^^^ True
    | "Map" ^^^ True
  )

  def tjah(implicit loc0: debugging.ParserLocation) : Parser[Term] = "Blop" ~ "Blap" ^^^ False

  def IsZeroTerm(implicit loc0: debugging.ParserLocation): Parser[Term] = (
    "iszero" ~> SimpleChurchNumTerm ^^ (t => IsZero(t))
  )

//    | failure("illegal start of simple term"))
  
  def BoolTerm(implicit loc0: debugging.ParserLocation): Parser[Term] = (
    "true"   ^^^ True
    | "false"  ^^^ False
  )

  def SimpleChurchNumTerm(implicit loc: debugging.ParserLocation): Parser[Term] = (
    "zero"   ^^^ Zero
    | "of" ~> SimpleChurchNumTerm ^^ { case t => Succ(Succ(Succ(t))) }
    | "succ" ~ SimpleChurchNumTerm ^^ { case "succ" ~ t => Succ(t) }
  ) 
  
}
