! RUN: %flang < %s | FileCheck %s
! CHECK:      <PROGRAM::TEST>
! CHECK-NEXT: <END PROGRAM::TEST>
PROGRAM test
END PROGRAM test
