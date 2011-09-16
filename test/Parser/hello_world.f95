! RUN: %flang < %s
PROGRAM HELLO_WORLD
  CHARACTER (LEN=11) :: C
  C = 'hello world'
  PRINT *, C
  PRINT *, 'hello world'
END PROGRAM HELLO_WORLD
