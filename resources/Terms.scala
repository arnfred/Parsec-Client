package com.github.hubertp.parserexperiments

abstract class Term

case object True extends Term

case object False extends Term

case object Zero extends Term

case class Succ(t: Term) extends Term

case class IsZero(t: Term) extends Term
