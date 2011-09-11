//===--- ASTContext.cpp - Context to hold long-lived AST nodes ------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the ASTContext interface.
//
//===----------------------------------------------------------------------===//

#include "flang/AST/ASTContext.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/ErrorHandling.h"
using namespace flang;

ASTContext::ASTContext(llvm::SourceMgr &SM)
  : SrcMgr(SM), LastSDM(0) {
  TUDecl = TranslationUnitDecl::Create(*this);
  InitBuiltinTypes();
}

ASTContext::~ASTContext() {
  // Release the DenseMaps associated with DeclContext objects.
  // FIXME: Is this the ideal solution?
  ReleaseDeclContextMaps();
}

void ASTContext::InitBuiltinType(QualType &R, BuiltinType::TypeSpec K) {
  BuiltinType *Ty = new (*this, TypeAlignment) BuiltinType(K);
  R = QualType(Ty, 0);
  Types.push_back(Ty);
}

void ASTContext::InitBuiltinTypes() {
  // [R404]
  InitBuiltinType(IntegerTy,         BuiltinType::Integer);
  InitBuiltinType(RealTy,            BuiltinType::Real);
  InitBuiltinType(DoublePrecisionTy, BuiltinType::DoublePrecision);
  InitBuiltinType(ComplexTy,         BuiltinType::Complex);
  InitBuiltinType(CharacterTy,       BuiltinType::Character);
  InitBuiltinType(LogicalTy,         BuiltinType::Logical);
}

QualType ASTContext::getBuiltinQualType(BuiltinType::TypeSpec TS) const {
  switch (TS) {
  case BuiltinType::Invalid: assert(false && "Invalid type spec!"); break;
  case BuiltinType::Integer:         return IntegerTy;
  case BuiltinType::Real:            return RealTy;
  case BuiltinType::DoublePrecision: return DoublePrecisionTy;
  case BuiltinType::Character:       return CharacterTy;
  case BuiltinType::Logical:         return LogicalTy;
  case BuiltinType::Complex:         return ComplexTy;
  }
  return QualType();
}

//===----------------------------------------------------------------------===//
//                   Type creation/memoization methods
//===----------------------------------------------------------------------===//

QualType ASTContext::getExtQualType(const Type *BaseType, Qualifiers Quals,
                                    Expr *KindSel, Expr *LenSel) const {
  // Check if we've already instantiated this type.
  llvm::FoldingSetNodeID ID;
  ExtQuals::Profile(ID, BaseType, Quals, KindSel, LenSel);
  void *InsertPos = 0;
  if (ExtQuals *EQ = ExtQualNodes.FindNodeOrInsertPos(ID, InsertPos)) {
    assert(EQ->getQualifiers() == Quals);
    return QualType(EQ, 0);
  }

  // If the base type is not canonical, make the appropriate canonical type.
  QualType Canon;
  if (!BaseType->isCanonicalUnqualified()) {
    SplitQualType CanonSplit = BaseType->getCanonicalTypeInternal().split();
    CanonSplit.second.addConsistentQualifiers(Quals);
    Canon = getExtQualType(CanonSplit.first, CanonSplit.second, KindSel,
                           LenSel);

    // Re-find the insert position.
    (void) ExtQualNodes.FindNodeOrInsertPos(ID, InsertPos);
  }

  ExtQuals *EQ = new (*this, TypeAlignment) ExtQuals(BaseType, Canon, Quals,
                                                     KindSel);
  ExtQualNodes.InsertNode(EQ, InsertPos);
  return QualType(EQ, 0);
}

/// getPointerType - Return the uniqued reference to the type for a pointer to
/// the specified type.
PointerType *ASTContext::getPointerType(const Type *Ty, unsigned NumDims) {
  // Unique pointers, to guarantee there is only one pointer of a particular
  // structure.
  llvm::FoldingSetNodeID ID;
  PointerType::Profile(ID, Ty, NumDims);

  void *InsertPos = 0;
  if (PointerType *PT = PointerTypes.FindNodeOrInsertPos(ID, InsertPos))
    return PT;

  PointerType *New = new (*this) PointerType(Ty, NumDims);
  Types.push_back(New);
  PointerTypes.InsertNode(New, InsertPos);
  return New;
}

/// getConstantArrayType - Return the unique reference to the type for an
/// array of the specified element type.
QualType ASTContext::getConstantArrayType(QualType EltTy,
                                          const llvm::APInt &ArySizeIn) const {
  // Convert the array size into a canonical width matching the pointer size for
  // the target.
  llvm::APInt ArySize(ArySizeIn);
  ArySize = ArySize.zextOrTrunc(8); // FIXME: Need to get target pointer width!

  llvm::FoldingSetNodeID ID;
  ConstantArrayType::Profile(ID, EltTy, ArySize);

  void *InsertPos = 0;
  if (ConstantArrayType *ATP =
      ConstantArrayTypes.FindNodeOrInsertPos(ID, InsertPos))
    return QualType(ATP, 0);

  // If the element type isn't canonical or has qualifiers, this won't be a
  // canonical type either, so fill in the canonical type field.
  QualType Canon;
  if (!EltTy.isCanonical() || EltTy.hasLocalQualifiers()) {
    SplitQualType canonSplit = getCanonicalType(EltTy).split();
    Canon = getConstantArrayType(QualType(canonSplit.first, 0), ArySize);
    Canon = getQualifiedType(Canon, canonSplit.second);

    // Get the new insert position for the node we care about.
    ConstantArrayType *NewIP =
      ConstantArrayTypes.FindNodeOrInsertPos(ID, InsertPos);
    assert(NewIP == 0 && "Shouldn't be in the map!"); (void)NewIP;
  }

  ConstantArrayType *New = new (*this,TypeAlignment)
    ConstantArrayType(EltTy, Canon, ArySize);
  ConstantArrayTypes.InsertNode(New, InsertPos);
  Types.push_back(New);
  return QualType(New, 0);
}

/// getTypeDeclTypeSlow - Return the unique reference to the type for the
/// specified type declaration.
QualType ASTContext::getTypeDeclTypeSlow(const TypeDecl *Decl) const {
  assert(Decl && "Passed null for Decl param");
  assert(!Decl->TypeForDecl && "TypeForDecl present in slow case");

  const RecordDecl *Record = dyn_cast<RecordDecl>(Decl);
  if (!Record) {
    llvm_unreachable("TypeDecl without a type?");
    return QualType(Decl->TypeForDecl, 0);
  }

  return getRecordType(Record);
}

QualType ASTContext::getRecordType(const RecordDecl *Decl) const {
  return QualType();
#if 0
  if (Decl->TypeForDecl) return QualType(Decl->TypeForDecl, 0);

  RecordType *newType = new (*this, TypeAlignment) RecordType(Decl);
  Decl->TypeForDecl = newType;
  Types.push_back(newType);
  return QualType(newType, 0);
#endif
}
