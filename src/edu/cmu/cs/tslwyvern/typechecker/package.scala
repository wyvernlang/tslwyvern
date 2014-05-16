package edu.cmu.cs.tslwyvern

import scala.collection.immutable.ListMap
import scala.collection.immutable.Map
import scala.collection.mutable
import ast._

package object typechecker {
  type Context = Map[X, (Type, X)] /* \Gamma */
  val empty_ctx: Context = Map()
  private val fresh_var_map: mutable.Map[X, Int] = mutable.Map()
  private def fresh_var(x: X): X = {
    val j = fresh_var_map.getOrElseUpdate(x, 0)
    if (j == 0) {
      return x
    } else {
      fresh_var_map.update(x, j + 1)
      return s"$x$$$j"
    }
  }

  class StaticError extends Throwable /* base class for errors */
  class TypeDeclError extends StaticError
  class TypeError extends StaticError

  /* \vdash_\Theta \rho \leadsto i */
  def check(p: Program): TIProgram = {
    val types = p.typeBindings.foldLeft[TContext](prelude.prelude_ctx)({
      case (types, TypeBinding(name, decl, metadata)) => {
        // TODO: recursive declarations
        if (types.isDefinedAt(name)) throw new TypeDeclError
        // Know type name, but not decl and metadata
        val types_1 = types + ((name, TypeDesc(None, None)))
        check_decl(types_1, decl)
        // Know type name and decl, but not metadata
        val types_2 = types + ((name, TypeDesc(Some(decl), None)))
        val imetadata = syn(empty_ctx, types_2, metadata)
        // Now we know everything
        types + ((name, TypeDesc(Some(decl), Some(imetadata))))
      }
    })
    TIProgram(types, syn(empty_ctx, types, p.expr))
  }

  /* \vdash_\Theta \omega 
   * \vdash_\Theta \chi
   */
  def check_decl(types: TContext, decl: TypeDecl): Unit = decl match {
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

  /* \vdash_\Theta \tau */
  def type_ok(types: TContext, t: Type): Unit = t match {
    case Named(name) => if (!types.isDefinedAt(name)) throw new TypeDeclError()
    case Arrow(t1, t2) => {
      type_ok(types, t1)
      type_ok(types, t2)
    }
    case ParseStreamType() => ()
    case TupleType(t1, t2) => {
      type_ok(types, t1)
      type_ok(types, t2)
    }
    case UnitType() => ()
    case NumberType() => ()
    case StringType() => ()
  }

  /* \Gamma \vdash_\Theta e \leadsto i \Leftarrow \tau
   * \Gamma; \Gamma \vdash_\Theta \hat{e} \leadsto i \Leftarrow \tau
   */
  private def ana[E <: Exp](
    outer_ctx: Context, ctx: Context, types: TContext,
    e: E, t: Type): TIExp = ((e, t) match {
    case (Lam(x, e1), Arrow(t1, t2)) => {
      val i_x = fresh_var(x)
      val i1 = ana(outer_ctx, ctx + ((x, (t1, i_x))), types, e1, t1)
      TILam(i_x, i1, Arrow(t1, t2))
    }
    case (Lam(_, _), _) => throw new TypeError
    case (New(d), Named(name)) => {
      val imembers = types.get(name) match {
        case Some(TypeDesc(Some(ObjTypeDecl(members)), _)) =>
          ana_members(outer_ctx, ctx, types, d, members, t)
        case _ => throw new TypeError
      }
      TINew(imembers, Named(name))
    }
    case (New(d), _) => throw new TypeError
    case (Inj(c, e), Named(name)) => {
      val i = types.get(name) match {
        case Some(TypeDesc(Some(CaseTypeDecl(cases)), _)) => {
          val CaseDecl(t) = cases.getOrElse(c, () => throw new TypeError)
          ana(outer_ctx, ctx, types, e, t)
        }
        case _ => throw new TypeError
      }
      TIInj(c, i, Named(name))
    }
    case (Inj(c, e), _) => throw new TypeError
    case (Lit(s), Named(name)) => types.get(name) match {
      case Some(TypeDesc(_, Some(metadata))) => metadata.t match {
        case Named("HasTSL") => {
          val i_ps = TIParseStream(s)
          val i_parser = TIPrj("parser", metadata, Named("Parser"))
          val i_parse = TIPrj("parse", i_parser,
            Arrow(ParseStreamType(), TupleType(Named("Exp"), ParseStreamType())))
          val i_ap = TIAp(i_parse, i_ps, TupleType(Named("Exp"), ParseStreamType()))
          val p = TIProgram(types, i_ap)
          val (r_ast, _) = interpretter.eval(p)
          val h_ast : HExp = throw new TypeError // TODO: dereification
          ana(ctx, empty_ctx, types, h_ast, t)
        }
        case _ => throw new TypeError
      }
      case _ => throw new TypeError()
    }
    case (Lit(s), _) => throw new TypeError
    case (Spliced(e), t) => ana(null, outer_ctx, types, e, t)
    case (Tuple(e1, e2), TupleType(t1, t2)) => {
      val i1 = ana(outer_ctx, ctx, types, e1, t1)
      val i2 = ana(outer_ctx, ctx, types, e2, t2)
      TITuple(i1, i2, TupleType(t1, t2))
    }
    case (Triv(), UnitType()) => TITriv()
    case (Num(n), NumberType()) => TINum(n)
    case (Num(_), _) => throw new TypeError
    case (Str(s), StringType()) => TIStr(s)
    case (Str(_), _) => throw new TypeError
    case (e, ana_t) => {
      val i = syn(outer_ctx, ctx, types, e)
      if (!i.t.eq(ana_t)) throw new TypeError
      i
    }
  })

  /* \Gamma \vdash_\Theta m \leadsto \dot{m} \Leftarrow \omega
   * \Gamma; \Gamma \vdash_Theta \hat{m} \leadsto \dot{m} \Leftarrow \omega
   */
  private def ana_members[E_in <: Exp](
    outer_ctx: Context, ctx: Context, types: TContext,
    m: ListMap[L, MemberDef[E_in]], member_decls: Map[L, MemberDecl],
    t_self: Type): ListMap[L, TIMemberDef] = {
    if (m.size != member_decls.size) throw new TypeError
    m.foldLeft[ListMap[L, TIMemberDef]](ListMap())({
      case (im, (label, defn)) => {
        val decl = member_decls.getOrElse(label, () => throw new TypeError)
        (defn, decl) match {
          case (Val(e), ValDecl(t)) => {
            val i = ana(outer_ctx, ctx, types, e, t)
            im + ((label, TIVal(i, t)))
          }
          case (Val(_), _) => throw new TypeError
          case (Def(e), DefDecl(t)) => {
            val i = ana(outer_ctx, ctx, types, e, Arrow(t_self, t))
            im + ((label, TIDef(i, Arrow(t_self, t))))
          }
          case (Def(_), _) => throw new TypeError
        }
      }
    })
  }

  /* \Gamma \vdash_\Theta e \leadsto i \Rightarrow \tau
   * \Gamma; \Gamma \vdash_\Theta \hat{e} \leadsto i \Leftarrow \tau
   */
  private def syn[E_in <: Exp](
    outer_ctx: Context, ctx: Context, types: TContext,
    e: E_in): TIExp = e match {
    case Var(x) => ctx.get(x) match {
      case Some((t, i_x)) => TIVar(i_x, t)
      case None => throw new TypeError
    }
    case Asc(e1, t) => {
      type_ok(types, t)
      ana(outer_ctx, ctx, types, e1, t)
    }
    case Ap(e1, e2) => {
      val i1 = syn(outer_ctx, ctx, types, e1)
      i1.t match {
        case Arrow(t1, t2) => {
          val i2 = ana(outer_ctx, ctx, types, e2, t1)
          TIAp(i1, i2, t2)
        }
        case _ => throw new TypeError
      }
    }
    case Prj(l, e1) => {
      val i1 = syn(outer_ctx, ctx, types, e1)
      i1.t match {
        case Named(name) => types.get(name) match {
          case Some(TypeDesc(Some(ObjTypeDecl(members)), _)) =>
            members.get(l) match {
              case Some(mdecl) => TIPrj(l, i1, mdecl.t)
              case _ => throw new TypeError
            }
          case _ => throw new TypeError
        }
        case _ => throw new TypeError
      }
    }
    case Case(e1, rules) => {
      val i1 = syn(outer_ctx, ctx, types, e1)
      i1.t match {
        case Named(name) => types.get(name) match {
          case Some(TypeDesc(Some(CaseTypeDecl(cases)), _)) => {
            val (irules, t) = syn_rules(outer_ctx, ctx, types, rules, cases)
            TICase(i1, irules, t)
          }
          case _ => throw new TypeError
        }
        case _ => throw new TypeError
      }
    }
    case Toast(e) => {
      val i = syn(outer_ctx, ctx, types, e)
      TIToast(i, Named("Exp"))
    }
    case Metadata(name) => types.get(name) match {
      case Some(TypeDesc(_, Some(i))) => TIMetadata(name, i.t)
      case _ => throw new TypeError
    }
    case Spliced(e) => syn(null, outer_ctx, types, e)
    case PSToString(e) => TIPSToString(ana(outer_ctx, ctx, types, e, ParseStreamType()))
    case Take(e1, e2) => {
      val i1 = ana(outer_ctx, ctx, types, e1, ParseStreamType())
      val i2 = ana(outer_ctx, ctx, types, e2, NumberType())
      TITake(i1, i2)
    }
    case TupleCase(e1, x1, x2, e2) => {
      val i1 = syn(outer_ctx, ctx, types, e)
      i1.t match {
        case TupleType(t1, t2) => {
          val i_x1 = fresh_var(x1)
          val i_x2 = fresh_var(x2)
          val new_ctx: Context = ctx + ((x1, (t1, i_x1))) + ((x2, (t2, i_x2)))
          val i2 = syn(outer_ctx, new_ctx, types, e2)
          TITupleCase(i1, i_x1, i_x2, i2, i2.t)
        }
        case _ => throw new TypeError
      }
    }
    case NumOp(op, e1, e2) => {
      val i1 = ana(outer_ctx, ctx, types, e1, NumberType())
      val i2 = ana(outer_ctx, ctx, types, e2, NumberType())
      TINumOp(op, i1, i2)
    }
    case NumIfEq(e1, e2, e3, e4) => {
      val i1 = ana(outer_ctx, ctx, types, e1, NumberType())
      val i2 = ana(outer_ctx, ctx, types, e2, NumberType())
      val i3 = syn(outer_ctx, ctx, types, e3)
      val i4 = syn(outer_ctx, ctx, types, e4)
      if (i3.t.eq(i4.t)) {
        TINumIfEq(i1, i2, i3, i4, i3.t)
      } else {
        throw new TypeError()
      }
    }
    case ParseNum(e) => TIParseNum(ana(outer_ctx, ctx, types, e, StringType()))
    case StrOp(op, e1, e2) => {
      val i1 = ana(outer_ctx, ctx, types, e1, StringType())
      val i2 = ana(outer_ctx, ctx, types, e2, StringType())
      TIStrOp(op, i1, i2)
    }
    case StrIfEq(e1, e2, e3, e4) => {
      val i1 = ana(outer_ctx, ctx, types, e1, StringType())
      val i2 = ana(outer_ctx, ctx, types, e2, StringType())
      val i3 = syn(outer_ctx, ctx, types, e3)
      val i4 = syn(outer_ctx, ctx, types, e4)
      if (i3.t.eq(i4.t)) {
        TIStrIfEq(i1, i2, i3, i4, i3.t)
      } else {
        throw new TypeError()
      }
    }
    case StrCase(e1, e2, hd, tl, e3) => {
      val i1 = ana(outer_ctx, ctx, types, e1, StringType())
      val i2 = syn(outer_ctx, ctx, types, e2)
      val i_hd = fresh_var(hd)
      val i_tl = fresh_var(tl)
      val new_ctx = ctx + ((hd, (StringType(), i_hd))) + ((tl, (StringType(), i_tl)))
      val i3 = syn(outer_ctx, new_ctx, types, e3)
      if (i2.t.eq(i3.t)) {
        TIStrCase(i1, i2, i_hd, i_tl, i3, i2.t)
      } else {
        throw new TypeError()
      }
    }
    case _ => throw new TypeError
  }

  private def syn_rules[E_in <: Exp](
    outer_ctx: Context, ctx: Context, types: TContext,
    rules: List[Rule[E_in]], cases: Map[C, CaseDecl]): (List[TIRule], Type) = {
    val (irules, t) = rules.foldLeft[(List[TIRule], Option[Type])]((List(), None))({
      case ((irules, previous_t), Rule(c, x, e)) =>
        val (irules_new, t) = cases.get(c) match {
          case Some(CaseDecl(t)) => {
            val i_x = fresh_var(x)
            val i = syn(outer_ctx, ctx + ((x, (t, i_x))), types, e)
            ((TIRule(c, i_x, i, i.t) :: irules), i.t)
          }
          case _ => throw new TypeError
        }
        previous_t match {
          case Some(previous_t) => if (!previous_t.eq(t)) throw new TypeError
          case _ => ()
        }
        (irules_new, Some(t))
    })
    t match {
      case Some(t) => (irules.reverse, t)
      case None => throw new TypeError
    }
  }

  /* convenience method for 
   * Gamma \vdash_\Theta e \leadsto i \Leftarrow \tau
   */
  def ana(ctx: Context, types: TContext, e: EExp, t: Type): TIExp =
    ana[EExp](null, ctx, types, e, t)

  /* convenience method for 
   * Gamma \vdash_\Theta e \leadsto i \Rightarrow \tau
   */
  def syn(ctx: Context, types: TContext, e: EExp): TIExp =
    syn[EExp](null, ctx, types, e)
}