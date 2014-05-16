package edu.cmu.cs.tslwyvern

import scala.collection.immutable.ListMap
import scala.tools.nsc.{ Interpreter, Settings }

package object interpretter {
  def check_and_eval(program: ast.Program): AnyRef =
    eval(typechecker.check(program))

  def eval(tiprogram: ast.TIProgram): AnyRef = {
    val s_types = translator.translate(tiprogram.types)
    val s_i = translator.translate(tiprogram.i)
    val settings = new Settings
    settings processArgumentString "-usejavacp"
    val interpreter = new Interpreter(settings)
    interpreter.eval(s_types)
    interpreter.interpret(s_i)
  }

  def main(args: Array[String]) {
    val e: ast.EExp = ast.Asc(ast.Str("Hello, TSL Wyvern!"), ast.StringType())
    println(check_and_eval(ast.Program(List(), e)))
  }
}