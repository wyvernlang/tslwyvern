package edu.cmu.cs.tslwyvern

import scala.collection.immutable.ListMap
import scala.collection.immutable.Map

package object ast {
  // Identifiers
  // NOTE: We do not check to see if these are valid (Scala) identifiers
  type X = String /* variables (x) */
  type L = String /* member labels (\ell) */
  type C = String /* case labels (C) */
  type T = String /* type names (T) */

  // Programs
  sealed case class Program(
    typeBindings: List[TypeBinding] /* \theta */ ,
    expr: EExp) /* \rho */
  case class TypeBinding(name: T, decl: TypeDecl, metadata: EExp)
  sealed abstract class TypeDecl
  case class ObjTypeDecl(members: ListMap[L, MemberDecl] /* \omega */ ) extends TypeDecl
  sealed abstract class MemberDecl(val t: Type)
  case class ValDecl(override val t: Type) extends MemberDecl(t)
  case class DefDecl(override val t: Arrow) extends MemberDecl(t)
  case class CaseTypeDecl(cases: ListMap[C, CaseDecl] /* \chi */ ) extends TypeDecl
  case class CaseDecl(t: Type)
  case class TIProgram(types: TContext, i: TIExp)
  type TContext = ListMap[T, TypeDesc] /* \Theta */
  sealed case class TypeDesc(
    decl: Option[TypeDecl], /* \delta */
    metadata: Option[TIExp] /* \mu */ )

  // Types
  sealed abstract class Type /* \tau */
  case class Named(name: T) extends Type
  case class Arrow(t1: Type, t2: Type) extends Type
  /* For efficiency/simplicity, we build these in: */
  case class ParseStreamType() extends Type
  case class TupleType(t1: Type, t2: Type) extends Type
  case class UnitType() extends Type
  case class NumberType() extends Type
  case class StringType() extends Type

  // Expressions
  sealed abstract class Exp /* expressions (e, \hat{e}) */
  sealed trait EExp extends Exp /* e */
  sealed trait HExp extends Exp /* \hat{e} */
  sealed abstract class TIExp(val t: Type) /* typed internal expression (i : \tau) */

  case class Var(x: X) extends EExp with HExp
  case class TIVar(x: X, override val t: Type) extends TIExp(t)

  case class Asc[E <: Exp](e: E, t: Type) extends EExp with HExp

  case class Lam[E <: Exp](x: X, e: E) extends EExp with HExp
  case class TILam(x: X, i: TIExp, override val t: Arrow) extends TIExp(t)
  case class Ap[E <: Exp](e1: E, e2: E) extends EExp with HExp
  case class TIAp(i1: TIExp, i2: TIExp, override val t: Type) extends TIExp(t)

  case class New[E <: Exp](m: ListMap[L, MemberDef[E]]) extends EExp with HExp
  case class TINew(im: ListMap[L, TIMemberDef], override val t: Named) extends TIExp(t)
  sealed abstract class MemberDef[+E <: Exp] /* m */
  sealed abstract class TIMemberDef(t: Type)
  case class Val[E <: Exp](e: E) extends MemberDef[E]
  case class TIVal(i: TIExp, t: Type) extends TIMemberDef(t)
  case class Def[E <: Exp](e: E) extends MemberDef[E]
  case class TIDef(i: TIExp, t: Arrow) extends TIMemberDef(t)
  case class Prj[E <: Exp](name: L, e: E) extends EExp with HExp
  case class TIPrj(name: L, i: TIExp, override val t: Type) extends TIExp(t)

  case class Inj[E <: Exp](constructor: C, e: E) extends EExp with HExp
  case class TIInj(constructor: C, i: TIExp, override val t: Named) extends TIExp(t)
  case class Case[E <: Exp](e: E, rules: List[Rule[E]]) extends EExp with HExp
  case class TICase(i: TIExp, irules: List[TIRule], override val t: Type) extends TIExp(t)
  case class Rule[+E <: Exp](constructor: C, x: X, e: E) /* r */
  case class TIRule(constructor: C, x: X, i: TIExp, t: Type)

  case class Toast[E <: Exp](e: E) extends EExp with HExp
  case class TIToast(i: TIExp, override val t: Type) extends TIExp(t)

  case class Metadata(name: T) extends EExp with HExp
  case class TIMetadata(name: T, override val t: Type) extends TIExp(t)

  case class Lit(body: String) extends EExp

  case class Spliced(e: EExp) extends HExp

  /* The intro form of parse streams is only in the internal language */
  case class TIParseStream(s: String) extends TIExp(ParseStreamType())
  case class PSToString[E <: Exp](e: E) extends EExp with HExp
  case class TIPSToString(i: TIExp) extends TIExp(StringType())
  case class Take[E <: Exp](e1: E, e2: E) extends EExp with HExp
  case class TITake(i1: TIExp, i2: TIExp) extends 
    TIExp(TupleType(ParseStreamType(), ParseStreamType()))

  case class Tuple[E <: Exp](e1: E, e2: E) extends EExp with HExp
  case class TITuple(i1: TIExp, i2: TIExp, override val t: TupleType) extends TIExp(t)
  case class TupleCase[E <: Exp](e1: E, x1: X, x2: X, e2: E) extends EExp with HExp
  case class TITupleCase(i1: TIExp, x1: X, x2: X, i2: TIExp, 
      override val t: Type) extends TIExp(t)

  case class Triv() extends EExp with HExp
  case class TITriv() extends TIExp(UnitType())

  case class Num(n: Number) extends EExp with HExp
  case class TINum(n: Number) extends TIExp(NumberType())
  // NOTE: We do not check if these are valid (Scala) binary numeric methods
  case class NumOp[E <: Exp](op: String, e1: E, e2: E) extends EExp with HExp
  case class TINumOp(op: String, i1: TIExp, i2: TIExp) extends TIExp(NumberType())
  case class NumIfEq[E <: Exp](e1: E, e2: E, e3: E, e4: E) extends EExp with HExp
  case class TINumIfEq(i1: TIExp, i2: TIExp, i3: TIExp, i4: TIExp, override val t: Type)
    extends TIExp(t)
  case class ParseNum(e: Exp) extends EExp with HExp
  case class TIParseNum(i: TIExp) extends TIExp(Named("NumOption"))

  case class Str(s: String) extends EExp with HExp
  case class TIStr(s: String) extends TIExp(StringType())
  // NOTE: We do not check if these are valid (Scala) binary string methods
  case class StrOp[E <: Exp](op: String, e1: E, e2: E) extends EExp with HExp
  case class TIStrOp(op: String, i1: TIExp, i2: TIExp) extends TIExp(StringType())
  case class StrIfEq[E <: Exp](e1: E, e2: E, e3: E, e4: E) extends EExp with HExp
  case class TIStrIfEq(i1: TIExp, i2: TIExp, i3: TIExp, i4: TIExp, override val t: Type)
    extends TIExp(t)
  case class StrCase[E <: Exp](e1: E, e2: E, x_hd: X, x_tl: X, e3: E)
    extends EExp with HExp
  case class TIStrCase(i1: TIExp, i2: TIExp, x_hd: X, x_tl: X, i3: TIExp,
    override val t: Type) extends TIExp(t)
}