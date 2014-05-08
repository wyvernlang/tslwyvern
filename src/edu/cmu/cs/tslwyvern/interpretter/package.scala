package edu.cmu.cs.tslwyvern

import edu.cmu.cs.tslwyvern.ast._
import edu.cmu.cs.tslwyvern.translator._
import scala.collection.immutable.ListMap
import scala.tools.nsc.{Interpreter, Settings}

package object interpretter {
  def check_and_eval(program : Program) : AnyRef = 
    eval(check(program))
  
  def eval(tiprogram : TIProgram) : AnyRef = {
    val s_types = translate(tiprogram.types)
    val s_i = translate(tiprogram.i)
    val settings = new Settings
    settings processArgumentString "-usejavacp"
    val interpreter = new Interpreter(settings)
    interpreter.eval(s_types)
    interpreter.interpret(s_i)
  }
  
  def main(args : Array[String]) {
    val e : EExp = Asc(Str("Hello, TSL Wyvern!"), StringType())
    println(check_and_eval(Program(ListMap(), e)))
  }
}