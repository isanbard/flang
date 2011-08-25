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
#include "flang/Basic/Type.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/Support/Allocator.h"
#include <new>
#include <set>
#include <vector>

namespace llvm {
template <typename T> class ArrayRef;
} // end llvm namespac

namespace fortran {

class ASTContext {
  std::vector<Type*> Types;
  llvm::FoldingSet<BuiltinType> BuiltinTypes;
  llvm::FoldingSet<PointerType> PointerTypes;
  llvm::FoldingSet<ArrayType>   ArrayTypes;
  llvm::FoldingSet<StructType>  StructTypes;

  /// VariableDecls - The various variables in a program.
  llvm::FoldingSet<VarDecl>     VariableDecls;

  /// MallocAlloc - The allocator objects used to create AST objects.
  llvm::MallocAllocator MallocAlloc;
public:
  ASTContext() {}
  ~ASTContext();

  void *Allocate(unsigned Size, unsigned Align = 8) {
    return MallocAlloc.Allocate(Size, Align);
  }
  void Deallocate(void *Ptr) {
    MallocAlloc.Deallocate(Ptr);
  }

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
                          const llvm::SmallVectorImpl<unsigned> &Dims);

  /// getStructType - Return the uniqued reference to the type for a structure of
  /// the specified type.
  StructType *getStructType(llvm::ArrayRef<Decl*> Elems);

  const VarDecl *getVarDecl(const IdentifierInfo *Info);
  const VarDecl *getOrCreateVarDecl(const IdentifierInfo *Info);
  const VarDecl *getOrCreateVarDecl(llvm::SMLoc Loc, const DeclSpec *DTS,
                                    const IdentifierInfo *Info);
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
inline void *operator new(size_t Bytes, fortran::ASTContext &C,
                          size_t Alignment = 8) throw () {
  return C.Allocate(Bytes, Alignment);
}

/// @brief Placement delete companion to the new above.
///
/// This operator is just a companion to the new above. There is no way of
/// invoking it directly; see the new operator for more details. This operator
/// is called implicitly by the compiler if a placement new expression using the
/// ASTContext throws in the object constructor.
inline void operator delete(void *Ptr, fortran::ASTContext &C, size_t)
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
inline void *operator new[](size_t Bytes, fortran::ASTContext& C,
                            size_t Alignment = 8) throw () {
  return C.Allocate(Bytes, Alignment);
}

/// @brief Placement delete[] companion to the new[] above.
///
/// This operator is just a companion to the new[] above. There is no way of
/// invoking it directly; see the new[] operator for more details. This operator
/// is called implicitly by the compiler if a placement new[] expression using
/// the ASTContext throws in the object constructor.
inline void operator delete[](void *Ptr, fortran::ASTContext &C, size_t)
              throw () {
  C.Deallocate(Ptr);
}

#endif
