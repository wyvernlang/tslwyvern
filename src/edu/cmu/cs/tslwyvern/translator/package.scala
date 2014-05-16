package edu.cmu.cs.tslwyvern

import scala.collection.immutable.ListMap
import edu.cmu.cs.tslwyvern.ast._

package object translator { // To Scala source code
  class TranslationError extends Throwable

  def translate(p: TIProgram): String = {
    val TIProgram(types, i) = p
    val s_types = translate(types)
    val s_i = translate(i)
    return s"$s_types\n\n$s_i"
  }

  def translate(types: TContext): String = {
    val builder = new StringBuilder()
    for ((name, TypeDesc(delta, mu)) <- types) {
      delta match {
        case Some(decl) =>
          decl match {
            case ObjTypeDecl(decls) => {
              builder ++= s"sealed class $name = {\n";
              for (decl <- decls) {
                decl match {
                  case (label, ValDecl(t)) => {
                    val s_type = translate(t)
                    builder ++= s"val $label : $s_type\n"
                  }
                  case (label, DefDecl(t)) => {
                    val s_type = translate(t)
                    builder ++= s"val $label : $name => $s_type\n"
                  }
                }
              }
              builder ++= "}\n\n"
            }
            case CaseTypeDecl(cases) => {
              builder ++= s"sealed abstract class $name\n"
              for ((c, CaseDecl(t)) <- cases) {
                val s_type = translate(t)
                builder ++= s"case class $name$$$c($s_type) extends $name\n"
              }
            }
          }
        case _ => throw new TranslationError
      }
      mu match {
        case Some(metadata) => {
          val s_metadata = translate(metadata)
          builder ++= s"val _metadata$$$name = $s_metadata\n"
        }
        case _ => () // no metadata is ok
      }
    }
    builder.toString()
  }

  def translate(t: Type): String = {
    t match {
      case Named(name) => name
      case Arrow(t1, t2) => {
        val s_t1 = translate(t1)
        val s_t2 = translate(t2)
        s"($s_t1) => $s_t2"
      }
      case ParseStreamType() => "String"
      case TupleType(t1, t2) => {
        val s_t1 = translate(t1)
        val s_t2 = translate(t2)
        s"($s_t1, s_t2)"
      }
      case UnitType() => s"Unit"
      case NumberType() => "Int"
      case StringType() => "String"
    }
  }

  def translate(i: TIExp): String = i match {
    case TIVar(x, _) => x
    case TILam(x, i, Arrow(t1, t2)) => {
      val s_i = translate(i)
      val s_t1 = translate(t1)
      s"(($x : $s_t1) => $s_i)"
    }
    case TIAp(i1, i2, _) => {
      val s_i1 = translate(i1)
      val s_i2 = translate(i2)
      s"$s_i1($s_i2)"
    }
    case TINew(m, t) => {
      val s_t = translate(t)
      val s_m = translate_members(m)
      s"new $s_t {\n$s_m\n}"
    }
    case TIPrj(label, i, _) => {
      val s_i = translate(i)
      s"$s_i.$label"
    }
    case TIInj(c, i, Named(name)) => {
      val s_i = translate(i)
      s"$name.$c($s_i)"
    }
    case TICase(i, irules, _) => {
      val s_i = translate(i)
      val s_irules = translate_rules(irules, translate(i.t))
      s"(($s_i) match {\n$s_irules\n})"
    }
    case TIToast(i, _) => throw new NotImplementedError
    case TIMetadata(name, _) => {
      s"_metadata$$$name"
    }
    case TIParseStream(s) => "\"" + s + "\""
    case TIPSToString(i) => translate(i)
    case TITake(i1, i2) => {
      val s_i1 = translate(i1)
      val s_i2 = translate(i2)
      s"$s_i1.splitAt($s_i2)"
    }
    case TITuple(i1, i2, _) => {
      val s_i1 = translate(i1)
      val s_i2 = translate(i2)
      s"(($s_i1), ($s_i2))"
    }
    case TITupleCase(i1, x1, x2, i2, _) => {
      val s_i1 = translate(i1)
      val s_i2 = translate(i2)
      s"($s_i1) match { case ($x1, $x2) => $s_i2 }"
    }
    case TITriv() => "()"
    case TINum(n) => n.toString()
    case TINumOp(op, i1, i2) => {
      val s_i1 = translate(i1)
      val s_i2 = translate(i2)
      s"($s_i1).$op($s_i2)"
    }
    case TINumIfEq(i1, i2, i3, i4, _) => {
      val s_i1 = translate(i1)
      val s_i2 = translate(i2)
      val s_i3 = translate(i3)
      val s_i4 = translate(i4)
      s"if (($s_i1) == ($s_i2)) {\n$s_i3\n} else {\n$s_i4\n}"
    }
    case TIParseNum(i) => {
      val s_i = translate(i)
      s"try { NumOption$$Some(($s_i).toInt) } catch { case _:NumberFormatException => NumOption$$None(()) }"
    }
    case TIStr(str) => "\"" + str + "\""
    case TIStrOp(op, i1, i2) => {
      val s_i1 = translate(i1)
      val s_i2 = translate(i2)
      s"($s_i1).$op($s_i2)"
    }
    case TIStrIfEq(i1, i2, i3, i4, _) => {
      val s_i1 = translate(i1)
      val s_i2 = translate(i2)
      val s_i3 = translate(i3)
      val s_i4 = translate(i4)
      s"if (($s_i1) == ($s_i2)) {\n$s_i3\n} else {\n$s_i4\n}"
    }
    case TIStrCase(i1, i2, x_hd, x_tl, i3, _) => {
      val s_i1 = translate(i1)
      val s_i2 = translate(i2)
      val s_i3 = translate(i3)
      s"if (($s_i1) == " + "\"\"" + ") {\n$s_i2\n} else {\n{\nval $x_hd = ($s_i1).head\nval $x_tl = ($s_i1).tail\n$s_i3 }\n}"
    }
  }

  def translate_members(im: ListMap[L, TIMemberDef]): String = {
    val builder = new StringBuilder()
    for ((label, defn) <- im) {
      defn match {
        case TIVal(i, t) => {
          val s_i = translate(i)
          s"val $label = ($s_i)"
        }
        case TIDef(i, t) => {
          val s_i = translate(i)
          s"val $label = (($s_i)(this))"
        }
      }
    }
    builder.toString()
  }

  def translate_rules(irules: List[TIRule], t_name: String): String = {
    val builder = new StringBuilder()
    for (TIRule(constructor, x, i, _) <- irules) {
      val s_i = translate(i)
      builder ++= "case $t_name$$$constructor($x) => ($s_i)\n"
    }
    builder.toString()
  }
}