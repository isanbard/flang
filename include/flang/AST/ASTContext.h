//===--- ASTContext.h - Context to hold long-lived AST nodes ----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the ASTContext interface.
//
//===----------------------------------------------------------------------===//

#ifndef FORTRAN_AST_ASTCONTEXT_H__
#define FORTRAN_AST_ASTCONTEXT_H__

#include "flang/AST/Decl.h"
#include "flang/AST/Type.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/SourceMgr.h"
#include <new>
#include <set>
#include <vector>

namespace llvm {
template <typename T> class ArrayRef;
} // end llvm namespac

namespace fortran {

class StoredDeclsMap;

// Decls
class DeclContext;
class Decl;
class FieldDecl;
class RecordDecl;
class TagDecl;
class TranslationUnitDecl;
class TypeDecl;

class ASTContext {
  ASTContext &this_() { return *this; }

  mutable std::vector<Type*>            Types;
  mutable llvm::FoldingSet<ExtQuals>    ExtQualNodes;
  mutable llvm::FoldingSet<BuiltinType> BuiltinTypes;
  mutable llvm::FoldingSet<PointerType> PointerTypes;
  mutable llvm::FoldingSet<ArrayType>   ArrayTypes;
  mutable llvm::FoldingSet<RecordType>  RecordTypes;

  /// VariableDecls - The various variables in a program.
  mutable llvm::FoldingSet<VarDecl>     VariableDecls;

  /// \brief The allocator used to create AST objects.
  ///
  /// AST objects are never destructed; rather, all memory associated with the
  /// AST objects will be released when the ASTContext itself is destroyed.
  mutable llvm::BumpPtrAllocator        BumpAlloc;

  TranslationUnitDecl *TUDecl;

  /// SourceMgr - The associated SourceMgr object.
  llvm::SourceMgr &SrcMgr;

  //===--------------------------------------------------------------------===//
  //                           Type Constructors
  //===--------------------------------------------------------------------===//

private:
  /// getExtQualType - Return a type with extended qualifiers.
  QualType getExtQualType(const Type *Base, Qualifiers Quals) const;

  QualType getTypeDeclTypeSlow(const TypeDecl *Decl) const;

  void InitBuiltinTypes();
  void InitBuiltinType(QualType &R, BuiltinType::TypeSpec K);

public:
  ASTContext(llvm::SourceMgr &SM);
  ~ASTContext();

  TranslationUnitDecl *getTranslationUnitDecl() const { return TUDecl; }

  llvm::SourceMgr &getSourceManager() { return SrcMgr; }
  const llvm::SourceMgr &getSourceManager() const { return SrcMgr; }

  void *Allocate(unsigned Size, unsigned Align = 8) const {
    return BumpAlloc.Allocate(Size, Align);
  }
  void Deallocate(void *Ptr) const {
    BumpAlloc.Deallocate(Ptr);
  }

  // Builtin Types: [R404]
  QualType IntegerTy;
  QualType RealTy;
  QualType DoublePrecisionTy;
  QualType CharacterTy;
  QualType LogicalTy;
  QualType ComplexTy;

  /// getBuiltinType - Return the uniqued reference to the type for an intrinsic
  /// type.
  BuiltinType *getBuiltinType(BuiltinType::TypeSpec TS, Selector Kind);

  /// getCharacterBuiltinType - Return the uniqued reference to the type for a
  /// character type.
  BuiltinType *getCharacterBuiltinType(Selector Len, Selector Kind);

  /// getPointerType - Return the uniqued reference to the type for a pointer to
  /// the specified type.
  PointerType *getPointerType(const Type *Ty, unsigned NumDims);

  /// getArrayType - Return the uniqued reference to the type for an array of
  /// the specified type.
  ArrayType *getArrayType(const Type *Ty,
                          const llvm::SmallVectorImpl<Expr*> &Dims);

  /// getRecordType - Return the uniqued reference to the type for a structure
  /// of the specified type.
  QualType getRecordType(const RecordDecl *Decl) const;

  const VarDecl *getVarDecl(const IdentifierInfo *Info);
  const VarDecl *getOrCreateVarDecl(llvm::SMLoc Loc, const DeclSpec *DTS,
                                    const IdentifierInfo *Info);

  /// getTypeDeclType - Return the unique reference to the type for
  /// the specified type declaration.
  QualType getTypeDeclType(const TypeDecl *Decl,
                           const TypeDecl *PrevDecl = 0) const {
    assert(Decl && "Passed null for Decl param");
    if (Decl->TypeForDecl) return QualType(Decl->TypeForDecl, 0);

    if (PrevDecl) {
      assert(PrevDecl->TypeForDecl && "previous decl has no TypeForDecl");
      Decl->TypeForDecl = PrevDecl->TypeForDecl;
      return QualType(PrevDecl->TypeForDecl, 0);
    }

    return getTypeDeclTypeSlow(Decl);
  }

  const std::vector<Type*> &getTypes() const { return Types; }

private:
  // FIXME: This currently contains the set of StoredDeclMaps used
  // by DeclContext objects. This probably should not be in ASTContext,
  // but we include it here so that ASTContext can quickly deallocate them.
  StoredDeclsMap *LastSDM;

  void ReleaseDeclContextMaps();
  friend class DeclContext;
};

} // end fortran namespace

// operator new and delete aren't allowed inside namespaces. The throw
// specifications are mandated by the standard.

/// @brief Placement new for using the ASTContext's allocator.
///
/// This placement form of operator new uses the ASTContext's allocator for
/// obtaining memory. It is a non-throwing new, which means that it returns null
/// on error. (If that is what the allocator does. The current does, so if this
/// ever changes, this operator will have to be changed, too.)
///
/// Usage looks like this (assuming there's an ASTContext 'Context' in scope):
/// 
/// @code
/// // Default alignment (8)
/// IntegerLiteral *Ex = new (Context) IntegerLiteral(arguments);
/// // Specific alignment
/// IntegerLiteral *Ex2 = new (Context, 4) IntegerLiteral(arguments);
/// @endcode
/// 
/// Please note that you cannot use delete on the pointer; it must be
/// deallocated using an explicit destructor call followed by
/// @c Context.Deallocate(Ptr).
///
/// @param Bytes The number of bytes to allocate. Calculated by the compiler.
/// @param C The ASTContext that provides the allocator.
/// @param Alignment The alignment of the allocated memory (if the underlying
///                  allocator supports it).
/// @return The allocated memory. Could be NULL.
inline void *operator new(size_t Bytes, const fortran::ASTContext &C,
                          size_t Alignment = 8) throw () {
  return C.Allocate(Bytes, Alignment);
}

/// @brief Placement delete companion to the new above.
///
/// This operator is just a companion to the new above. There is no way of
/// invoking it directly; see the new operator for more details. This operator
/// is called implicitly by the compiler if a placement new expression using the
/// ASTContext throws in the object constructor.
inline void operator delete(void *Ptr, const fortran::ASTContext &C, size_t)
              throw () {
  C.Deallocate(Ptr);
}

/// This placement form of operator new[] uses the ASTContext's allocator for
/// obtaining memory. It is a non-throwing new[], which means that it returns
/// null on error.
///
/// Usage looks like this (assuming there's an ASTContext 'Context' in scope):
/// 
/// @code
/// // Default alignment (8)
/// char *data = new (Context) char[10];
/// // Specific alignment
/// char *data = new (Context, 4) char[10];
/// @endcode
/// 
/// Please note that you cannot use delete on the pointer; it must be
/// deallocated using an explicit destructor call followed by
/// @c Context.Deallocate(Ptr).
///
/// @param Bytes The number of bytes to allocate. Calculated by the compiler.
/// @param C The ASTContext that provides the allocator.
/// @param Alignment The alignment of the allocated memory (if the underlying
///                  allocator supports it).
/// @return The allocated memory. Could be NULL.
inline void *operator new[](size_t Bytes, const fortran::ASTContext& C,
                            size_t Alignment = 8) throw () {
  return C.Allocate(Bytes, Alignment);
}

/// @brief Placement delete[] companion to the new[] above.
///
/// This operator is just a companion to the new[] above. There is no way of
/// invoking it directly; see the new[] operator for more details. This operator
/// is called implicitly by the compiler if a placement new[] expression using
/// the ASTContext throws in the object constructor.
inline void operator delete[](void *Ptr, const fortran::ASTContext &C, size_t)
              throw () {
  C.Deallocate(Ptr);
}

#endif
