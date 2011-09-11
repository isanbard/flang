//===--- IdentifierTable.h - Hash table for identifier lookup ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the IdentifierInfo, IdentifierTable, and Selector
// interfaces.
//
//===----------------------------------------------------------------------===//

#ifndef FLANG_BASIC_IDENTIFIERTABLE_H__
#define FLANG_BASIC_IDENTIFIERTABLE_H__

#include "TokenKinds.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/Twine.h"
#include <string>

namespace flang {

class LangOptions;
class IdentifierInfo;
class IdentifierTable;

/// IdentifierInfo - One of these records is kept for each identifier that
/// is lexed.  This contains information about whether the token was #define'd,
/// is a language keyword, or if it is a front-end token of some sort (e.g. a
/// variable or function name).  The preprocessor keeps this information in a
/// set, and all tok::identifier tokens have a pointer to one of these.
class IdentifierInfo {
  // Note: DON'T make TokenID a 'tok::TokenKind'; MSVC will treat it as a
  //       signed char and TokenKinds > 127 won't be handled correctly.
  unsigned TokenID            : 8; // Front-end token ID or tok::identifier.
  // 24 bits left in 32-bit word.

  void *FETokenInfo;               // Managed by the language front-end.
  llvm::StringMapEntry<IdentifierInfo*> *Entry;

  IdentifierInfo(const IdentifierInfo&);  // NONCOPYABLE.
  void operator=(const IdentifierInfo&);  // NONASSIGNABLE.

  friend class IdentifierTable;

public:
  IdentifierInfo();

  /// isStr - Return true if this is the identifier for the specified string.
  /// This is intended to be used for string literals only: II->isStr("foo").
  template <std::size_t StrLen>
  bool isStr(const char (&Str)[StrLen]) const {
    return getLength() == StrLen-1 && !memcmp(getNameStart(), Str, StrLen-1);
  }

  /// getNameStart - Return the beginning of the actual string for this
  /// identifier.  The returned string is properly null terminated.
  const char *getNameStart() const {
    if (Entry) return Entry->getKeyData();
    // FIXME: This is gross. It would be best not to embed specific details
    // of the PTH file format here.
    // The 'this' pointer really points to a
    // std::pair<IdentifierInfo, const char*>, where internal pointer
    // points to the external string data.
    typedef std::pair<IdentifierInfo, const char*> actualtype;
    return ((const actualtype*) this)->second;
  }

  /// getLength - Efficiently return the length of this identifier info.
  unsigned getLength() const {
    if (Entry) return Entry->getKeyLength();
    // FIXME: This is gross. It would be best not to embed specific details
    // of the PTH file format here.
    // The 'this' pointer really points to a
    // std::pair<IdentifierInfo, const char*>, where internal pointer
    // points to the external string data.
    typedef std::pair<IdentifierInfo, const char*> actualtype;
    const char* p = ((const actualtype*) this)->second - 2;
    return (((unsigned) p[0]) | (((unsigned) p[1]) << 8)) - 1;
  }

  /// getName - Return the actual identifier string.
  llvm::StringRef getName() const {
    return llvm::StringRef(getNameStart(), getLength());
  }

  /// get/setTokenID - If this is a source-language token (e.g. 'do'), this API
  /// can be used to cause the lexer to map identifiers to source-language
  /// tokens.
  tok::TokenKind getTokenID() const { return (tok::TokenKind)TokenID; }
  void setTokenID(tok::TokenKind ID) { TokenID = ID; }

  /// getFETokenInfo/setFETokenInfo - The language front-end is allowed to
  /// associate arbitrary metadata with this token.
  template<typename T>
  T *getFETokenInfo() const { return static_cast<T*>(FETokenInfo); }
  void setFETokenInfo(void *T) { FETokenInfo = T; }
};

/// IdentifierInfoLookup - An abstract class used by IdentifierTable that
/// provides an interface for performing lookups from strings (const char *) to
/// IdentiferInfo objects.
class IdentifierInfoLookup {
public:
  virtual ~IdentifierInfoLookup();

  /// get - Return the identifier token info for the specified named identifier.
  /// Unlike the version in IdentifierTable, this returns a pointer instead of a
  /// reference.  If the pointer is NULL then the IdentifierInfo cannot be
  /// found.
  virtual IdentifierInfo *get(std::string &Name) = 0;
};

/// IdentifierTable - This table implements an efficient mapping from strings to
/// IdentifierInfo nodes. It has no other purpose, but this is an extremely
/// performance-critical piece of the code, as each occurrance of every
/// identifier goes through here when lexed.
class IdentifierTable {
  // Shark shows that using MallocAllocator is *much* slower than using this
  // BumpPtrAllocator!
  typedef llvm::StringMap<IdentifierInfo*, llvm::BumpPtrAllocator> HashTableTy;
  HashTableTy IdentifierHashTable;
  HashTableTy KeywordHashTable;
  HashTableTy BuiltinHashTable;
  IdentifierInfoLookup *ExternalLookup;

public:
  /// IdentifierTable ctor - Create the identifier table, populating it with
  /// info about the language keywords for the language specified by LangOpts.
  IdentifierTable(const LangOptions &LangOpts,
                  IdentifierInfoLookup *externalLookup = 0);

  /// \brief Set the external identifier lookup mechanism.
  void setExternalIdentifierLookup(IdentifierInfoLookup *IILookup) {
    ExternalLookup = IILookup;
  }

  IdentifierInfo &get(const char *NameStart, const char *NameEnd) {
    std::string Name = llvm::StringRef(NameStart, NameEnd-NameStart).str();
    return get(Name);
  }

  IdentifierInfo &get(const char *NameStart, size_t NameLen) {
    std::string Name = llvm::StringRef(NameStart, NameLen).str();
    return get(Name);
  }

  IdentifierInfo &getKeyword(const char *NameStart, const char *NameEnd,
                             tok::TokenKind TokenCode) {
    return getKeyword(llvm::StringRef(NameStart, NameEnd-NameStart), TokenCode);
  }
  IdentifierInfo &getKeyword(const char *Name, size_t NameLen,
                             tok::TokenKind TokenCode) {
    return getKeyword(llvm::StringRef(Name, NameLen), TokenCode);
  }

  IdentifierInfo &getBuiltin(const char *NameStart, const char *NameEnd,
                             tok::TokenKind TokenCode) {
    return getBuiltin(llvm::StringRef(NameStart, NameEnd-NameStart), TokenCode);
  }
  IdentifierInfo &getBuiltin(const char *Name, size_t NameLen,
                             tok::TokenKind TokenCode) {
    return getBuiltin(llvm::StringRef(Name, NameLen), TokenCode);
  }

  /// get - Return the identifier token info for the specified named identifier.
  IdentifierInfo &get(std::string &Name) {
    std::string UCName = Name;
    for (size_t I = 0, E = UCName.size(); I != E; ++I)
      UCName[I] = ::toupper(UCName[I]);

    llvm::StringMapEntry<IdentifierInfo*> &Entry =
      IdentifierHashTable.GetOrCreateValue(UCName);

    IdentifierInfo *II = Entry.getValue();
    if (II) return *II;

    // No entry; if we have an external lookup, look there first.
    if (ExternalLookup) {
      II = ExternalLookup->get(UCName);
      if (II) {
        // Cache in the StringMap for subsequent lookups.
        Entry.setValue(II);
        return *II;
      }
    }

    // Lookups failed, make a new IdentifierInfo.
    void *Mem = IdentifierHashTable.getAllocator().Allocate<IdentifierInfo>();
    II = new (Mem) IdentifierInfo();
    Entry.setValue(II);

    // Make sure getName() knows how to find the IdentifierInfo
    // contents.
    II->Entry = &Entry;
    return *II;
  }

  /// getKeyword - Returns the keyword token for the specified name.
  IdentifierInfo &getKeyword(llvm::StringRef Name, tok::TokenKind TokenCode) {
    std::string UCName(Name);
    for (size_t I = 0, E = UCName.size(); I != E; ++I)
      UCName[I] = ::toupper(UCName[I]);

    llvm::StringMapEntry<IdentifierInfo*> &Entry =
      KeywordHashTable.GetOrCreateValue(UCName);

    IdentifierInfo *II = Entry.getValue();
    if (II) return *II;

    // No entry; if we have an external lookup, look there first.
    if (ExternalLookup) {
      II = ExternalLookup->get(UCName);
      if (II) {
        // Cache in the StringMap for subsequent lookups.
        Entry.setValue(II);
        return *II;
      }
    }

    // Lookups failed, make a new IdentifierInfo.
    void *Mem = KeywordHashTable.getAllocator().Allocate<IdentifierInfo>();
    II = new (Mem) IdentifierInfo();
    II->setTokenID(TokenCode);
    Entry.setValue(II);

    // Make sure getName() knows how to find the IdentifierInfo
    // contents.
    II->Entry = &Entry;
    return *II;
  }

  /// getBuiltin - Returns the builtin token for the specified name.
  IdentifierInfo &getBuiltin(llvm::StringRef Name, tok::TokenKind TokenCode) {
    std::string UCName(Name);
    for (size_t I = 0, E = UCName.size(); I != E; ++I)
      UCName[I] = ::toupper(UCName[I]);

    llvm::StringMapEntry<IdentifierInfo*> &Entry =
      BuiltinHashTable.GetOrCreateValue(UCName);

    IdentifierInfo *II = Entry.getValue();
    if (II) return *II;

    // No entry; if we have an external lookup, look there first.
    if (ExternalLookup) {
      II = ExternalLookup->get(UCName);
      if (II) {
        // Cache in the StringMap for subsequent lookups.
        Entry.setValue(II);
        return *II;
      }
    }

    // Lookups failed, make a new IdentifierInfo.
    void *Mem = BuiltinHashTable.getAllocator().Allocate<IdentifierInfo>();
    II = new (Mem) IdentifierInfo();
    II->setTokenID(TokenCode);
    Entry.setValue(II);

    // Make sure getName() knows how to find the IdentifierInfo
    // contents.
    II->Entry = &Entry;
    return *II;
  }

  /// lookupIdentifier - Return the iterator pointing to the identifier if
  /// found.
  IdentifierInfo *lookupIdentifier(std::string &Name) const {
    std::string UCName = Name;
    for (size_t I = 0, E = UCName.size(); I != E; ++I)
      UCName[I] = ::toupper(UCName[I]);

    HashTableTy::const_iterator Iter = IdentifierHashTable.find(UCName);
    return Iter != IdentifierHashTable.end() ? Iter->getValue() : 0;
  }

  /// lookupKeyword - Return the iterator pointing to the keyword if found.
  IdentifierInfo *lookupKeyword(std::string &Name) const {
    std::string UCName = Name;
    for (size_t I = 0, E = UCName.size(); I != E; ++I)
      UCName[I] = ::toupper(UCName[I]);

    HashTableTy::const_iterator Iter = KeywordHashTable.find(UCName);
    return Iter != KeywordHashTable.end() ? Iter->getValue() : 0;
  }

  /// lookupBuiltin - Return the iterator pointing to the builtin if found.
  IdentifierInfo *lookupBuiltin(std::string &Name) const {
    std::string UCName = Name;
    for (size_t I = 0, E = UCName.size(); I != E; ++I)
      UCName[I] = ::toupper(UCName[I]);

    HashTableTy::const_iterator Iter = BuiltinHashTable.find(UCName);
    return Iter != BuiltinHashTable.end() ? Iter->getValue() : 0;
  }

  /// isaIdentifier - Return 'true' if the name is in the identifier hashtable.
  bool isaIdentifier(llvm::StringRef Name) const {
    std::string NameStr = Name.str();
    return lookupIdentifier(NameStr) ? true : false;
  }

  /// isaKeyword - Return 'true' if the name is in the keyword hashtable. I.e.,
  /// it can be treated as a keyword in the correct context.
  bool isaKeyword(const llvm::StringRef Name) const {
    std::string NameStr = Name.str();    
    return lookupKeyword(NameStr) ? true : false;
  }

  /// isaBuiltin - Return 'true' if the name is in the builtin hashtable. I.e.,
  /// it can be treated as a builtin function in the correct context.
  bool isaBuiltin(const llvm::StringRef Name) const {
    std::string NameStr = Name.str();
    return lookupBuiltin(NameStr) ? true : false;
  }

  /// \brief Creates a new IdentifierInfo from the given string.
  ///
  /// This is a lower-level version of get() that requires that this identifier
  /// not be known previously and that does not consult an external source for
  /// identifiers. In particular, external identifier sources can use this
  /// routine to build IdentifierInfo nodes and then introduce additional
  /// information about those identifiers.
  IdentifierInfo &CreateIdentifierInfo(llvm::StringRef Name) {
    llvm::StringMapEntry<IdentifierInfo*> &Entry =
      IdentifierHashTable.GetOrCreateValue(Name);

    IdentifierInfo *II = Entry.getValue();
    assert(!II && "IdentifierInfo already exists");

    // Lookups failed, make a new IdentifierInfo.
    void *Mem = IdentifierHashTable.getAllocator().Allocate<IdentifierInfo>();
    II = new (Mem) IdentifierInfo();
    Entry.setValue(II);

    // Make sure getName() knows how to find the IdentifierInfo
    // contents.
    II->Entry = &Entry;

    return *II;
  }

  /// PrintStats - Print some statistics to stderr that indicate how well the
  /// hashing is doing.
  void PrintStats() const;

  void AddPredefineds(const LangOptions &LangOpts);
};

}  // end namespace flang

#endif
