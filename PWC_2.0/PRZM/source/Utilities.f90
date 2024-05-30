module utilities
    
    
    contains 
    
        SUBROUTINE tridiagonal_solution(A,B,C,X,F,N)
        implicit none
        integer,intent(in) :: n
        real,intent(in)    :: A(n), B(n),C(n) , F(n)
        real,intent(out)   :: X(n)
        
        real :: Y(n), ALPHA(n), BETA(n)
        integer :: nu, i, j
        
        ALPHA(1) = B(1)
        BETA(1) = C(1)/ALPHA(1)
        Y(1) = F(1)/ALPHA(1)
        

    
        do I=2, N
            ALPHA(I) = B(I) - A(I)*BETA(I-1)
            BETA(I) = C(I)/ALPHA(I)
            Y(I) = (F(I)-A(I)*Y(I-1))/ALPHA(I)
        end do

        
        
        X(N) = Y(N)
        NU=N-1

        do I=1, NU
            J=N-I
            X(J) = Y(J) - BETA(J)*X(J+1)
            
        end do

        
        
  end subroutine tridiagonal_solution
    
    

    
    
         subroutine To_uppercase(str)
         ! from http://rosettacode.org/wiki/String_case#Fortran
               
            implicit none
            character(*),intent(in out) :: str
            integer :: i
            
            do i=1, len(str)
                select case (str(i:i))
                case("a":"z")
                    str(i:i) = achar(ichar(str(i:i))  -32  )
                end select
            end do          
         end subroutine To_uppercase    
    
         
               
  !*****************************************************************************
   pure elemental integer function jd (YEAR,MONTH,DAY)
     !calculate the days since 1/1/1900 given year,month, day, from Fliegel and van Flandern (1968)
    !Fliegel, H. F. and van Flandern, T. C. (1968). Communications of the ACM, Vol. 11, No. 10 (October, 1968). 

     implicit none
     integer, intent(in) :: year,month,day

      JD= day-32075+1461*(year+4800+(month-14)/12)/4+367*(month-2-(month-14)/12*12) /12-3*((year+4900+(month-14)/12)/100)/4 -2415021

   end function jd
   !*****************************************************************************
   
   pure subroutine get_date (date1900, YEAR,MONTH,DAY)
 !computes THE GREGORIAN CALENDAR DATE (YEAR,MONTH,DAY) given days since 1900
   implicit none

   integer,intent(out) :: YEAR,MONTH,DAY

   integer,intent(in) :: date1900  !days since 1900
   integer :: L,n,i,j

   L= 2483590 + date1900

   n= 4*L/146097

   L= L-(146097*n+3)/4
   I= 4000*(L+1)/1461001
   L= L-1461*I/4+31
   J= 80*L/2447

   day= L-2447*J/80
   L= J/11
   month = J+2-12*L
   year = 100*(N-49)+I+L

 !   YEAR= I
 !  MONTH= J
 !  DAY= K

   end subroutine get_date
      
   
!*********Some Older PRZM Utilities Could use Reworking***************************
      SUBROUTINE COMRD(KFILE,LINE)
!     + + + PURPOSE + + +
!     checks whether an input line is a comment or data;
!     if not a comment, return line for data input.  line must be
!     defined as CHARACTER*80 in the calling program.
!     Modification date: 2/18/92 JAM

          implicit none
      CHARACTER(len=80), intent(out) :: LINE
      INTEGER, intent(in)  ::    KFILE
!
!     + + + ARGUMENT DEFINITIONS + + +
!     KFILE - unit number to read from
!     LINE  - record read from file
!     EOF   - end of file reached
!
!     + + + LOCAL VARIABLES + + +
      CHARACTER(len=80) OUTMSG
      LOGICAL      FATAL
      INTEGER     IERR

      do 
        !bypass comment lines with 3 *
        READ(KFILE,'(A80)',IOSTAT=IERR) LINE
           if (ierr > 0) then
               outmsg = "Problem reading input file"
               Fatal = .true.
               CALL ERRCHK(OUTMSG, FATAL)           
           elseif (ierr == -1) then  !end of file
               outmsg = "End of File Encountered"
               Fatal = .true.
               CALL ERRCHK(OUTMSG, FATAL) 
           end if    

        IF (INDEX(LINE,'***') == 0) exit 
      end do

      END  SUBROUTINE COMRD

      
      SUBROUTINE check_for_comment(KFILE)
      ! checks whether an input line is a comment or data;
      ! if not a comment, return line for data input.
      implicit none
      INTEGER, intent(in)  :: KFILE  !file unit
      CHARACTER(len=3)LINE
      CHARACTER(len=80) OUTMSG
      LOGICAL      FATAL
      INTEGER     IERR

      do 
        !bypass comment lines with 3 *
        READ(KFILE,'(A3)',IOSTAT=IERR) LINE

           if (ierr > 0) then
               outmsg = "Problem reading input file"
               Fatal = .true.
               CALL ERRCHK5(OUTMSG, FATAL)   
               
               !UNECESSARY CHECK AND CAUSES SHUTDOWN if input file is exactly 8192 bytes
           elseif (ierr == -1) then  !end of file
               
               outmsg = "End of File Encountered"
               Fatal = .true.
               CALL ERRCHk5(OUTMSG, FATAL) 
           end if    

        IF (INDEX(LINE,'***') == 0) exit 
      end do

      Backspace(Kfile) !if the line is not a comment, repposition at start and return
      
      END  SUBROUTINE check_for_comment
!*******************************************************
      SUBROUTINE ERRCHK5(MESAGE,FATAL)
        use  constants_and_Variables, ONLY: EchoFileUnit 
        implicit none

      CHARACTER(len=80),intent(in) :: MESAGE
      LOGICAL,intent(in) ::      FATAL

!     IERROR - numeric code for error
!     MESAGE - text of error message
!     FATAL  - fatal error flag


!       check for fatal error
        IF (FATAL) THEN   !  error is fatal      
          WRITE(EchoFileUnit,*) MESAGE  
          WRITE(EchoFileUnit,*) 'FATAL ERROR !! - Execution terminated'
          STOP 
        ELSE
          WRITE(EchoFileUnit,*) MESAGE
        ENDIF

      END SUBROUTINE ERRCHK5

      
      !*******************************************************
      SUBROUTINE ERRCHK(MESAGE,FATAL)
        use  constants_and_Variables, ONLY: EchoFileUnit 
        implicit none

      CHARACTER(len=80) MESAGE
      LOGICAL      FATAL

!     IERROR - numeric code for error
!     MESAGE - text of error message
!     FATAL  - fatal error flag


!       check for fatal error
        IF (FATAL) THEN   !  error is fatal      
          WRITE(EchoFileUnit,*) MESAGE  
          WRITE(EchoFileUnit,*) 'FATAL ERROR !! - Execution terminated'
          STOP 
        ELSE
          WRITE(EchoFileUnit,*) MESAGE
        ENDIF

      RETURN
      END SUBROUTINE ERRCHK

      
      
      !*************************************************************
      REAL FUNCTION EXPCHK(argument)
      implicit none
!     checks arguments for exponential limits


      REAL argument
      
      !REAL*8, PARAMETER :: REALMX = 1.0D30 
      !REAL*8,PARAMETER ::  EXPMN = 1.0D-30
      !REAL*8,PARAMETER ::  EXNMX = -53.0
      !REAL*8,PARAMETER ::  EXPMX = 53.0
      
      
  !    REAL(KIND=8) ::  REALMX = huge(arg)
      REAL ::  EXPMN  = tiny(argument)
      REAL ::  EXNMX  = minexponent(argument)
      REAL ::  EXPMX  = maxexponent(argument)
      

!     + + + LOCAL VARIABLES + + +
      CHARACTER(len=200)MESAGE
      LOGICAL      FATAL

      
      

 2000 FORMAT('Argument [',D11.4,'] too large for EXP')

!     + + + END SPECIFICATIONS + + +

      IF (argument .LT. EXNMX) THEN
        EXPCHK = 0.0
      ELSE IF (ABS(argument) .LT. EXPMN) THEN
        EXPCHK = 1.0
      ELSE IF (argument .GT. EXPMX) THEN
        WRITE(MESAGE,2000) argument
        FATAL = .TRUE.
        CALL ERRCHK(MESAGE,FATAL)
     !   EXPCHK = REALMX  !this is a place holder, since it would never actually be used
      ELSE
        EXPCHK = EXP(argument)
      ENDIF

      
      END FUNCTION EXPCHK

  


      
!!******************************************************** 
!      REAL(KIND=8)  FUNCTION RELTST(ARG)
!!     check to see double precision argument is valid as single precision
!      implicit none
!      REAL(KIND=8)       ARG
!
!!     ARG - variable to test
!!
!      REAL(KIND=8), PARAMETER :: REALMX = 1.0D30
!      REAL(KIND=8), PARAMETER :: REALMN = 1.0D-30
!!     + + + LOCAL VARIABLES + + +
!      REAL(KIND=8)       DARG
!      CHARACTER(len=80) MESAGE
!      INTEGER      IERROR
!      LOGICAL      FATAL
!
!      DARG = DABS(ARG)
!!
!      IF (DARG .LT. REALMN) THEN
!!       too small
!        RELTST = 0.0
!      ELSE IF (DARG .GT. REALMX) THEN
!!       too big
!        MESAGE = 'Single precision overflow'
!        IERROR = 1350
!        FATAL  = .TRUE.
!        CALL ERRCHK(IERROR,MESAGE,FATAL)
!        RELTST = REALMX
!      ELSE
!!       just right
!        RELTST = ARG
!      ENDIF
!      END  FUNCTION   RELTST
!   
         
         
         
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
     
         
   

     pure integer function find_depth_node(n,depth,desired) 
     !Given an array "depth" of size "n" that is ordered from low to high values, this 
     !function will give the index of "depth" that is closest to the value "desired"
    
      implicit none
      integer,intent(in)            :: n       !size of depth vector 
      real,dimension(n), intent(in) :: depth   !vector holding incremental depths)
      real,intent(in)               :: desired !desired depth
      
      integer :: i, index


      do i=1, n 
          index = i  !store value for the case where we go to the max n and i would be incremented another 1 value
          if (depth(i) > desired) exit
      end do

      
      i = index     !if i falls out the loop above it will have a value of n+1, so we use index to capture real value
      if  (i==1) then 
          find_depth_node = 1
      else if (abs(depth(i) - desired) < abs (depth(i-1) - desired)) then
          find_depth_node = i
      else
          find_depth_node = i-1
      end if

     end function find_depth_node  
         
         
     
     
 

    
    
end module utilities