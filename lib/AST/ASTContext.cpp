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
using namespace fortran;

ASTContext::~ASTContext() {
  // Deallocate all the types.
  while (!Types.empty()) {
    Types.back()->Destroy(*this);
    Types.pop_back();
  }
}

/// getBuiltinType - Return the uniqued reference to the type for an intrinsic
/// type.
BuiltinType *ASTContext::getBuiltinType(BuiltinType::TypeSpec TS,
                                        Selector Kind) {
  // Unique pointers, to guarantee there is only one pointer of a particular
  // structure.
  llvm::FoldingSetNodeID ID;
  BuiltinType::Profile(ID, TS, Kind);

  void *InsertPos = 0;
  if (BuiltinType *BT = BuiltinTypes.FindNodeOrInsertPos(ID, InsertPos))
    return BT;

  BuiltinType *New = new (*this) BuiltinType(TS, Kind);
  Types.push_back(New);
  BuiltinTypes.InsertNode(New, InsertPos);
  return New;
}

/// getCharacterBuiltinType - Return the uniqued reference to the type for a
/// character type.
BuiltinType *ASTContext::getCharacterBuiltinType(Selector Len, Selector Kind) {
  // Unique pointers, to guarantee there is only one pointer of a particular
  // structure.
  llvm::FoldingSetNodeID ID;
  CharacterBuiltinType::Profile(ID, Len, Kind);

  void *InsertPos = 0;
  if (BuiltinType *BT = BuiltinTypes.FindNodeOrInsertPos(ID, InsertPos))
    return BT;

  CharacterBuiltinType *New = new (*this) CharacterBuiltinType(Len, Kind);
  Types.push_back(New);
  BuiltinTypes.InsertNode(New, InsertPos);
  return New;
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

/// getArrayType - Return the uniqued reference to the type for an array to the
/// specified type.
ArrayType *ASTContext::getArrayType(const Type *Ty,
                                    const llvm::SmallVectorImpl<unsigned>&Dims){
  // Unique pointers, to guarantee there is only one pointer of a particular
  // structure.
  llvm::FoldingSetNodeID ID;
  ArrayType::Profile(ID, Ty, Dims);

  void *InsertPos = 0;
  if (ArrayType *AT = ArrayTypes.FindNodeOrInsertPos(ID, InsertPos))
    return AT;

  ArrayType *New = new (*this) ArrayType(Ty, Dims);
  Types.push_back(New);
  ArrayTypes.InsertNode(New, InsertPos);
  return New;
}

StructType *ASTContext::getStructType(llvm::ArrayRef<Type*> Elems) {
  // Unique pointers, to guarantee there is only one pointer of a particular
  // structure.
  llvm::FoldingSetNodeID ID;
  StructType::Profile(ID, Elems);

  void *InsertPos = 0;
  if (StructType *ST = StructTypes.FindNodeOrInsertPos(ID, InsertPos))
    return ST;

  StructType *New = new (*this) StructType(Elems);
  Types.push_back(New);
  StructTypes.InsertNode(New, InsertPos);
  return New;
}

const VarDecl *ASTContext::getOrCreateVarDecl(llvm::SMLoc Loc,
                                              const DeclSpec *DS,
                                              const IdentifierInfo *Info) {
  // Unique pointers, to guarantee there is only one pointer of a particular
  // structure.
  llvm::FoldingSetNodeID ID;
  VarDecl::Profile(ID, Info);

  void *InsertPos = 0;
  if (VarDecl *VD = VariableDecls.FindNodeOrInsertPos(ID, InsertPos))
    return VD;

  VarDecl *New = new (*this) VarDecl(Loc, DS, Info);
  VariableDecls.InsertNode(New, InsertPos);
  return New;
}

const VarDecl *ASTContext::getOrCreateVarDecl(const IdentifierInfo *Info) {
  // Unique pointers, to guarantee there is only one pointer of a particular
  // structure.
  llvm::FoldingSetNodeID ID;
  VarDecl::Profile(ID, Info);

  void *InsertPos = 0;
  if (VarDecl *VD = VariableDecls.FindNodeOrInsertPos(ID, InsertPos))
    return VD;

  VarDecl *New = new (*this) VarDecl(Info);
  VariableDecls.InsertNode(New, InsertPos);
  return New;
}

const VarDecl *ASTContext::getVarDecl(const IdentifierInfo *Info) {
  // Unique pointers, to guarantee there is only one pointer of a particular
  // structure.
  llvm::FoldingSetNodeID ID;
  VarDecl::Profile(ID, Info);

  void *InsertPos = 0;
  return VariableDecls.FindNodeOrInsertPos(ID, InsertPos);
}
