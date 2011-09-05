//===-- DeclNodes.td - Declaration Nodes -------------------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Fortran declaration nodes.
//
//===----------------------------------------------------------------------===//

class AttrSubject;

class Decl<bit abstract = 0> : AttrSubject {
  bit Abstract = abstract;
}

class DDecl<Decl base, bit abstract = 0> : Decl<abstract> {
  Decl Base = base;
}

class DeclContext { }

def TranslationUnit : Decl, DeclContext;
def Named : Decl<1>;
  def Type : DDecl<Named, 1>;
    def Record : DDecl<Type>, DeclContext;
  def Value : DDecl<Named, 1>;
    def EnumConstant : DDecl<Value>;
    def Declarator : DDecl<Value, 1>;
      def MainProgram : Decl, DeclContext;
      def Function : DDecl<Declarator>, DeclContext;
      def Subroutine : DDecl<Declarator>, DeclContext;
      def Module : DDecl<Declarator>, DeclContext;
      def Submodule : DDecl<Declarator>, DeclContext;
      def Field : DDecl<Declarator>;
      def Var : DDecl<Declarator>;
def FileScopeAsm : Decl;
