! RUN: %flang < %s
P&
&R&
&O&
&G&
&R&
&A&
&M&
& TORTURE
  ! Torture the lexer and parser.
  CHARACTER (LEN=256) :: A, B, C, D, E, F

  A = '&
&'
  B = '"&
&"'
  C = 'Hello & ! world &
& ooh! here'&
&'s a &
  ! Yo! comment here!
  &fugly contiua&
 &tion'''
  ! An '&' is not allowed on a line by itself or with only a comment.
  D = '''&
&"'
END PROGRAM TORTURE
