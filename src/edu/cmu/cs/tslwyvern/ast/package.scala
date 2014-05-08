package edu.cmu.cs.tslwyvern

import scala.collection.immutable.ListMap

package object ast {
    type X = String /* variables (x) */
    type L = String /* member labels (\ell) */
    type C = String /* case labels (C) */
    type T = String /* type names (T) */
    
    sealed case class Program(typeBindings : ListMap[T, TypeBinding], expr : EExp) /* programs (\rho) */
    sealed case class TypeBinding(
        decl : TypeDecl,
        metadata : EExp
    )
    sealed abstract class TypeDecl
    sealed case class ObjTypeDecl(
        members : Map[L, MemberDecl]  /* \omega */) extends TypeDecl
    sealed abstract class MemberDecl(val t : Type)
    sealed case class ValDecl(override val t : Type) extends MemberDecl(t)
    sealed case class DefDecl(override val t : Arrow) extends MemberDecl(t)
    sealed case class CaseTypeDecl(
        cases : Map[C, CaseDecl] /* \chi */) extends TypeDecl
    sealed case class CaseDecl(t : Type)
    
    sealed case class TIProgram(types : TContext, i : TIExp)
    
  	sealed abstract class Type /* \tau */
	sealed case class Named(name : T) extends Type
	sealed case class Arrow(t1 : Type, t2 : Type) extends Type
	sealed case class NumberType() extends Type 
	sealed case class StringType() extends Type
	sealed case class PSType() extends Type

	sealed abstract class Exp /* expressions (e, \hat{i}) */
	sealed trait EExp extends Exp
	sealed trait HExp extends Exp
	sealed abstract class TIExp(val t : Type)
	
	case class Var(x : X) extends EExp with HExp
	case class TIVar(x : X, override val t : Type) extends TIExp(t)
	
    case class Asc[E <: Exp](e : E, t : Type) extends EExp with HExp

	case class Lam[E <: Exp](x : X, e : E) extends EExp with HExp
	case class TILam(x : X, i : TIExp, override val t : Arrow) extends TIExp(t)
	case class Ap[E <: Exp](e1 : E, e2 : E) extends EExp with HExp
	case class TIAp(i1 : TIExp, i2 : TIExp, override val t : Type) extends TIExp(t)
    
	case class New[E <: Exp](m : ListMap[L, MemberDef[E]]) extends EExp with HExp
	case class TINew(im : ListMap[L, TIMemberDef], override val t : Named) extends TIExp(t)
	sealed abstract class MemberDef[+E <: Exp] /* d */
	sealed abstract class TIMemberDef(t : Type)
	case class Val[E <: Exp](e : E) extends MemberDef[E]
	case class TIVal(i : TIExp, t : Type) extends TIMemberDef(t)
    case class Def[E <: Exp](e : E) extends MemberDef[E]
    case class TIDef(i : TIExp, t : Arrow) extends TIMemberDef(t)
	case class Prj[E <: Exp](name : L, e : E) extends EExp with HExp
	case class TIPrj(name : L, i : TIExp, override val t : Type) extends TIExp(t)
    
	case class Inj[E <: Exp](constructor : C, e : E) extends EExp with HExp
	case class TIInj(constructor : C, i : TIExp, override val t : Named) extends TIExp(t)
	case class Case[E <: Exp](e : E, rules : List[Rule[E]]) extends EExp with HExp
	case class TICase(i : TIExp, irules : List[TIRule], override val t : Type) extends TIExp(t)
    case class Rule[+E <: Exp](constructor : C, x : X, e : E) /* r */	
    case class TIRule(constructor : C, x : X, i : TIExp, t : Type)
    
	case class Toast[E <: Exp](e : E) extends EExp with HExp
	case class TIToast(i : TIExp, override val t : Type) extends TIExp(t)
    
	case class Metadata(name : T) extends EExp with HExp
	case class TIMetadata(name : T, override val t : Type) extends TIExp(t)
    
	case class Lit(body : String) extends EExp
	
	/* In the theory its more convenient to thread the outer_ctx through,
	 * but in the implementation its more convenient to associate it with the
	 * only form that will use it, spliced[e]:
	 */
	case class Spliced(e : EExp, outer_ctx : Context) extends HExp
	
	/* The intro form is only in the internal language */
	case class TIParseStream(s : String) extends TIExp(PSType())
	/* TODO: operators on parse streams */
    
	/* For efficiency, we build these in */
	case class Num(n : Number) extends EExp with HExp
	case class TINum(n : Number) extends TIExp(NumberType())
    
	case class Str(s : String) extends EExp with HExp
	case class TIStr(s : String) extends TIExp(StringType())
    /* TODO: operations on these */
		
    type Context = Map[X, Type] /* \Gamma */
    type TContext = ListMap[T, TypeDesc] /* \Sigma */
	sealed case class TypeDesc(
	    decl : Option[TypeDecl], /* \delta */
	    metadata : Option[TIExp] /* \mu */)

	val empty_ctx : Context = Map()
	val prelude : TContext = ListMap() /* \Sigma_0 */
	
	/* \vdash_\Sigma \rho \leadsto i */
    def check(p : Program) : TIProgram = {
      val types = p.typeBindings.foldLeft[TContext] (prelude) ({ 
        case (types, (name, TypeBinding(decl, metadata))) => {
          if (types.isDefinedAt(name)) throw new TypeDeclError
          // Know type name, but not decl and metadata
          val types_1 = types + ((name, TypeDesc(None, None)))
          check_decl(types_1, decl)
          // Know type name and decl, but not metadata
          val types_2 = types + ((name, TypeDesc(Some(decl), None)))
          val imetadata = esyn(empty_ctx, types_2, metadata)
          // Now we know everything
          types + ((name, TypeDesc(Some(decl), Some(imetadata))))
        }
      })
      TIProgram(types, esyn(empty_ctx, types, p.expr))
    }
    class StaticError extends Throwable /* base class for errors */
    class TypeDeclError extends StaticError
    class TypeError extends StaticError

    /* \vdash_\Sigma \omega 
     * \vdash_\Sigma \chi
     */
    def check_decl(types : TContext, decl : TypeDecl) : Unit = decl match {
      case ObjTypeDecl(members) => {
        for ((_, decl) <- members) {
          decl match { 
            case ValDecl(t) => type_ok(types, t)
            case DefDecl(t) => type_ok(types, t)
          }
        }
      }
      case CaseTypeDecl(cases) => {
        for ((_, CaseDecl(t)) <- cases) {
          type_ok(types, t)
        }
      }
    }
    
    /* \vdash_\Sigma \tau */
    def type_ok(types : TContext, t : Type) : Unit = t match {
      case Named(name) => if (!types.isDefinedAt(name)) throw new TypeDeclError()
      case Arrow(t1, t2) => {
        type_ok(types, t1)
        type_ok(types, t2)
      }
      case NumberType() => ()
      case StringType() => ()
      case PSType() => ()
    }
    class TypeFormationError extends StaticError
    
    /* \Gamma \vdash_\Sigma e \leadsto i \Leftarrow \tau
     * \Gamma \vdash_\Sigma \hat{i} \leadsto i \Leftarrow \tau
     */
    def ana[E_in <: Exp](ctx : Context, types : TContext, 
        e : E_in, t : Type) : TIExp = ((e, t) match {
      case (Num(n), NumberType()) => TINum(n)
      case (Num(_), _) => throw new TypeError
      case (Str(s), StringType()) => TIStr(s)
      case (Str(_), _) => throw new TypeError
      case (Lam(x, e1), Arrow(t1, t2)) => TILam(x, ana(ctx + ((x, t1)), types, e1, t1), Arrow(t1, t2))
      case (Lam(_, _), _) => throw new TypeError
      case (New(d), Named(name)) => {
        val imembers = types.get(name) match {
          case Some(TypeDesc(Some(ObjTypeDecl(members)), _)) => ana_members(ctx, types, d, members, t)
          case _ => throw new TypeError
        }
        TINew(imembers, Named(name))
      }
      case (New(d), _) => throw new TypeError
      case (Inj(c, e), Named(name)) => {
        val i = types.get(name) match {
          case Some(TypeDesc(Some(CaseTypeDecl(cases)), _)) => {
            val CaseDecl(t) = cases.getOrElse(c, () => throw new TypeError)
            ana(ctx, types, e, t)
          }
          case _ => throw new TypeError
        }
        TIInj(c, i, Named(name))
      }
      case (Inj(c, e), _) => throw new TypeError
      case (Lit(s), Named(name)) => throw new TypeError /* TODO */
      case (Lit(s), _) => throw new TypeError
      case (Spliced(e, outer_ctx), t) => ana(outer_ctx, types, e, t)
      case (e, ana_t) => {
        val i = syn(ctx, types, e)
        if (!i.t.eq(ana_t)) throw new TypeError
        i
      }
    })
    
    def ana_members[E_in <: Exp](ctx : Context, types : TContext,
        m : ListMap[L, MemberDef[E_in]], members : Map[L, MemberDecl], t_self : Type) : ListMap[L, TIMemberDef] = { 
      if (m.size != members.size) throw new TypeError
      m.foldLeft[ListMap[L, TIMemberDef]](ListMap())({
        case (im, (label, defn)) => {
          val decl = members.getOrElse(label, () => throw new TypeError)
          (defn, decl) match {
            case (Val(e), ValDecl(t)) => im + ((label, TIVal(ana(ctx, types, e, t), t)))
            case (Val(_), _) => throw new TypeError
            case (Def(e), DefDecl(t)) => im + ((label, TIDef(ana(ctx, types, e, Arrow(t_self, t)), Arrow(t_self, t))))
            case (Def(_), _) => throw new TypeError
          }
        }
      })
    }
    
    /* \Gamma \vdash_\Sigma e \leadsto i \Rightarrow \tau
     * \Gamma \vdash_\Sigma \hat{i} \leadsto i \Leftarrow \tau
     */
    def syn[E_in <: Exp](ctx : Context, types : TContext, e : E_in) : TIExp = e match {
	  	case Var(x) => TIVar(x, ctx.get(x) match {
	  	  case Some(t) => t
	  	  case None => throw new TypeError
	  	})
	  	case Asc(e1, t) => {
	  	  type_ok(types, t)
	  	  ana(ctx, types, e1, t)
	  	}
	  	case Ap(e1, e2) => {
	  	  val i1 = syn(ctx, types, e1)
	  	  i1.t match {
	  	    case Arrow(t1, t2) => {
	  	      val i2 = ana(ctx, types, e2, t1)
	  	      TIAp(i1, i2, t2)
	  	    }
	  	    case _ => throw new TypeError
	  	  }
	  	}
	  	case Prj(l, e1) => {
	  	  val i1 = syn(ctx, types, e1)
	  	  i1.t match {
	  	    case Named(name) => types.get(name) match {
	  	      case Some(TypeDesc(Some(ObjTypeDecl(members)), _)) => members.get(l) match {
	  	        case Some(mdecl) => TIPrj(l, i1, mdecl.t)
	  	        case _ => throw new TypeError
	  	      }
	  	      case _ => throw new TypeError
	  	    }
	  	    case _ => throw new TypeError
	  	  }
	  	}
	  	case Case(e1, rules) => {
	  	  val i1 = syn(ctx, types, e1)
	  	  i1.t match {
	  	    case Named(name) => types.get(name) match {
	  	      case Some(TypeDesc(Some(CaseTypeDecl(cases)), _)) => {
	  	        val (irules, t) = syn_rules(ctx, types, rules, cases)
	  	        TICase(i1, irules, t)
	  	      }
	  	      case _ => throw new TypeError
	  	    }
	  	    case _ => throw new TypeError
	  	  }
	  	}
	  	case Toast(e) => {
	  	  val i = syn(ctx, types, e)
	  	  TIToast(i, Named("Exp"))
	  	}
	  	case Metadata(name) => types.get(name) match {
	  	  case Some(TypeDesc(_, Some(i))) => TIMetadata(name, i.t)
	  	  case _ => throw new TypeError
	  	}
	  	case Spliced(e, outer_ctx) => syn(outer_ctx, types, e)	  	
	  	case _ => throw new TypeError
	  }
      
    def syn_rules[E_in <: Exp](ctx : Context, types : TContext, rules : List[Rule[E_in]], cases : Map[C, CaseDecl]) : (List[TIRule], Type) = {
      val (irules, t) = rules.foldLeft[(List[TIRule], Option[Type])]((List(), None))({
        case ((irules, previous_t), Rule(c, x, e)) =>
          val (irules_, t) = cases.get(c) match {
            case Some(CaseDecl(t)) => {
              val i = syn(ctx + ((x, t)), types, e)
              ((TIRule(c, x, i, i.t) :: irules), i.t)
            }
            case _ => throw new TypeError
          }
          previous_t match {
            case Some(previous_t) => if (!previous_t.eq(t)) throw new TypeError
            case _ => ()
          }
          (irules_, Some(t))
      })
      t match { 
        case Some(t) => (irules.reverse, t) 
        case None => throw new TypeError
      } 
    }
         
    /* convenience method for 
     * Gamma \vdash_\Sigma e \leadsto i \Leftarrow \tau
     */
    def eana(ctx : Context, types : TContext, e : EExp, t : Type) : TIExp = 
      ana[EExp](ctx, types, e, t)
      
    /* convenience method for 
     * Gamma \vdash_\Sigma e \leadsto i \Rightarrow \tau
     */
    def esyn(ctx : Context, types : TContext, e : EExp) : TIExp = 
      syn[EExp](ctx, types, e)
    
    /* convenience method for 
    * Gamma \vdash_\Sigma \hat{i} \leadsto i \Leftarrow \tau
    */
    private def hana(ctx : Context, types : TContext, e : HExp, t : Type) : TIExp = 
      ana[HExp](ctx, types, e, t)
      
    /* convenience method for 
     * Gamma \vdash_\Sigma \hat{i} \leadsto i \Rightarrow \tau
     */  
    private def hsyn(ctx : Context, types : TContext, e : HExp) : TIExp = 
      syn(ctx, types, e)
     
}