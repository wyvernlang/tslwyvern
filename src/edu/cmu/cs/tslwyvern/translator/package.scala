package edu.cmu.cs.tslwyvern

import scala.collection.immutable.ListMap
import edu.cmu.cs.tslwyvern.ast._

package object translator { // To Scala source code  
	def translate(types : TContext) : String = {
	  val builder = new StringBuilder()
	  for ((name, TypeDesc(Some(decl), Some(metadata))) <- types) {
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
	    val s_metadata = translate(metadata)
	    builder ++= s"val _metadata_$name = $s_metadata\n"
	  }
	  builder.toString()
	}
	
	def translate(t : Type) : String = {
	  t match {
	    case Named(name) => name
	    case Arrow(t1, t2) => {
	      val s_t1 = translate(t1)
	      val s_t2 = translate(t2)
	      s"($s_t1) => $s_t2"
	    }
	    case NumberType() => "Number"
	    case StringType() => "String"
	    case PSType() => "String"
	  }
	}
	
	def translate(i : TIExp) : String = i match {
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
	      s"_metadata_$name"
	    }
	    case TIParseStream(s) => s
	    case TINum(n) => n.toString()
	    case TIStr(str) => "\"" + str + "\""
	  }
	
	def translate_members(im : ListMap[L, TIMemberDef]) : String = {
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
	
	def translate_rules(irules : List[TIRule], t_name : String) : String = {
	  val builder = new StringBuilder()
	  for (TIRule(constructor, x, i, _) <- irules) {
	    val s_i = translate(i)
	    builder ++= "case $t_name.$constructor($x) => ($s_i)\n"
	  }
	  builder.toString()
	}
}