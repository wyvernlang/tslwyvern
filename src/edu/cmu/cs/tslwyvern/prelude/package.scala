package edu.cmu.cs.tslwyvern

import scala.collection.immutable.ListMap
import ast._

package object prelude {
  val IDType = StringType()
  val ExpType = Named("Exp")
  val TypeType = Named("Type")
  val MemberDefListType = Named("MemberDefList")
  val MemberDefType = Named("MemberDef")
  val RuleListType = Named("RuleList")
  val RuleType = TupleType(IDType, TupleType(IDType, ExpType))
  val ExpDecl = CaseTypeDecl(ListMap(
    "Var" -> CaseDecl(IDType),
    "Asc" -> CaseDecl(TupleType(TypeType, ExpType)),
    "Lam" -> CaseDecl(TupleType(IDType, ExpType)),
    "Ap" -> CaseDecl(TupleType(ExpType, ExpType)),
    "New" -> CaseDecl(MemberDefListType),
    "Prj" -> CaseDecl(ExpType),
    "Inj" -> CaseDecl(TupleType(IDType, ExpType)),
    "Case" -> CaseDecl(TupleType(ExpType, RuleListType)),
    "Toast" -> CaseDecl(ExpType),
    "Metadata" -> CaseDecl(TypeType),
    "Spliced" -> CaseDecl(ExpType),
    "Tuple" -> CaseDecl(TupleType(ExpType, ExpType)),
    "Triv" -> CaseDecl(UnitType()),
    "Num" -> CaseDecl(NumberType()),
    "NumOp" -> CaseDecl(TupleType(StringType(), TupleType(ExpType, ExpType))),
    "NumIfEq" -> CaseDecl(
      TupleType(ExpType, TupleType(ExpType, TupleType(ExpType, ExpType)))),
    "ParseNum" -> CaseDecl(ExpType),
    "Str" -> CaseDecl(StringType()),
    "StrOp" -> CaseDecl(TupleType(StringType(), TupleType(ExpType, ExpType))),
    "StrIfEq" -> CaseDecl(
      TupleType(ExpType, TupleType(ExpType, TupleType(ExpType, ExpType)))),
    "StrCase" -> CaseDecl(
      TupleType(ExpType,
        TupleType(ExpType,
          TupleType(IDType, TupleType(IDType, ExpType)))))))
  private def listTypeDecl(list: Named, elem: Type) = CaseTypeDecl(ListMap(
    "Nil" -> CaseDecl(UnitType()),
    "Cons" -> CaseDecl(TupleType(elem, list))))
  val MemberDefListDecl = listTypeDecl(MemberDefListType, MemberDefType)
  val MemberDefDecl = CaseTypeDecl(ListMap(
    "Val" -> CaseDecl(TupleType(IDType, ExpType)),
    "Def" -> CaseDecl(TupleType(IDType, ExpType))))
  val RuleListDecl = listTypeDecl(RuleListType, RuleType)
  val TypeDecl = CaseTypeDecl(ListMap(
    "Arrow" -> CaseDecl(TupleType(TypeType, TypeType)),
    "Named" -> CaseDecl(IDType),
    "ParseStream" -> CaseDecl(UnitType()),
    "Tuple" -> CaseDecl(TupleType(TypeType, TypeType)),
    "Unit" -> CaseDecl(UnitType()),
    "Number" -> CaseDecl(UnitType()),
    "String" -> CaseDecl(UnitType())))

  def optionTypeDecl(option: Named, elem: Type) = CaseTypeDecl(ListMap(
    "Some" -> CaseDecl(elem),
    "None" -> CaseDecl(UnitType())))
  val NumOptionType = Named("NumOption")
  val NumOptionDecl = optionTypeDecl(NumOptionType, NumberType())

  val HasTSLType = Named("HasTSL")
  val ParserType = Named("ParserType")
  val HasTSLDecl = ObjTypeDecl(ListMap(
    "parser" -> ValDecl(ParserType)))
  val parseFnType = Arrow(ParseStreamType(), TupleType(ExpType, ParseStreamType()))
  val ParserDecl = ObjTypeDecl(ListMap(
    "parse" -> DefDecl(parseFnType)))
  val prelude_ctx: TContext = ListMap(
    "Exp" -> TypeDesc(Some(ExpDecl), None), // TODO: quasiquotes
    "Type" -> TypeDesc(Some(TypeDecl), None),
    "MemberDefList" -> TypeDesc(Some(MemberDefListDecl), None),
    "MemberDef" -> TypeDesc(Some(MemberDefDecl), None),
    "RuleList" -> TypeDesc(Some(RuleListDecl), None),
    "NumOption" -> TypeDesc(Some(NumOptionDecl), None),
    "HasTSL" -> TypeDesc(Some(HasTSLDecl), None), // TODO: parser generator
    "Parser" -> TypeDesc(Some(ParserDecl), None))
}