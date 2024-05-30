Module m_utils

   Implicit None
   Private
   Public :: GetArgs

Contains
   
   Subroutine GetArgs (RunFilePath)
     use  constants_and_Variables, ONLY: maxFileLength
      ! Read arguments from the PRZM execution line
      ! Arg-1: Path of input file 'przm3.run'

      ! RunFilePath: Path of input file 'przm3.run'.
      !           On output:
      !           * Either blank (if no argument was present)
      !           * Or contains a trailing delimiter (":" or "\").

!      Use General_Vars
!      Use F2kCLI
      Implicit None
      Character(Len=*), Intent(Out) :: RunFilePath

      Integer :: nargs, j0, j1, flen
      Character(Len=maxFileLength) :: xarg

      RunFilePath = ''

      ! nargs contains the number of arguments present in the
      ! command line. No arguments -- exit.
      nargs = Command_Argument_Count()
      If (nargs <= 0) Return

      ! The first argument contains the input file path.
      ! * Remove balanced double quotes, if present
      ! * If the path looks like "x:", i.e., a drive
      !   letter followed by a colon, Do no more.
      ! * Otherwise make sure the path has a trailing backslash "\".
      Call Get_Command_Argument(1, xarg)
      j1 = Len_trim(xarg)
      j0 = 1
      If (xarg(j0:j0) == '"') Then
         If (xarg(j1:j1) == '"') Then
            j0 = j0 + 1
            j1 = j1 - 1
         End If
      End If

      flen = Len(RunFilePath)
      If ((j1-j0+1) > flen) Then
         ! This is ugly: the path is too long to store in the user's variable,
         ! and the log file has not been open yet. Issue a message to the screen
         ! and abort.
         Stop 'File path in command line argument-1 is too long.'
      End If
      RunFilePath = xarg(j0:j1)
      j1 = Len_trim(RunFilePath)

      ! Empty string -- exit.
      If (j1 <= 0) Return

      ! Add trailing directory delimiter?
      Select Case (RunFilePath(j1:j1))
      Case (':', '\', '/')
         ! Last characters is a trailing directory delimiter: Leave things as they are.
      Case Default
         ! anything else, append trailing backslash
         j1 = j1 + 1
         RunFilePath(j1:j1) = '\'
      End Select

   End Subroutine GetArgs





End Module m_utils

