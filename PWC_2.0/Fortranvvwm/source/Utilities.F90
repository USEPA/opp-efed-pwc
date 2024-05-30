module utilities
implicit none

contains


   !*****************************************************************************
   pure elemental integer function jd (YEAR,MONTH,DAY)
     !calculate the days since 1/1/1900 given year,month, day, from Fliegel and van Flandern (1968)
    !Fliegel, H. F. and van Flandern, T. C. (1968). Communications of the ACM, Vol. 11, No. 10 (October, 1968). 

     implicit none
     integer, intent(in) :: year,month,day

      JD= day-32075+1461*(year+4800+(month-14)/12)/4+367*(month-2-(month-14)/12*12) /12-3*((year+4900+(month-14)/12)/100)/4 -2415021

    end function jd
   !*****************************************************************************
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
    

   !******************************************************************************
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


!###################################################################################


!###################################################################################
subroutine window_average(list, m, n, listout)
!This subroutine finds the running average of the current data plus prevoius "m-1" data
! of the input array "list".

!the average for the day before "m" are calculated as the average up to each day.  eg for day 10, sum (day 1 to day 10)/10
 
implicit none
    integer, intent(in) ::  n            !size of the list
    integer, intent(in) ::  m            !averaging window
    real, intent(in) :: list(n)        !data to be averaged
    real, intent(out):: listout(n)    !output of averages
    integer:: i
    
    
    !revised 12/15/14
    if (n < m) then 
        listout = 0.0  !return nothing if the window is longer tyhan the data
    else
         do concurrent (i=m:n)
           listout(i) = sum(list(i-m+1:i))/m
         end do 
         
        !calculation for first days with insufficient data for complete window
         do concurrent (i=1:m-1)
           listout(i) = sum(list(1:i))/dble(i)
         end do  
    end if
    
    
    
    
    
  !  do concurrent (i=m:n)
  !       listout(i) = sum(list(i-m+1:i))/m
  !  end do
  !  
  !
  !!  forall(i=m:n)   listout(i) = sum(list(i-m+1:i))/dble(m) !calc for days with enough previous data
  !  forall(i=1:m-1) listout(i) = sum(list(1:i))/dble(i)   !calculation for first days with insufficient data for complete window
end subroutine window_average
!###################################################################################







end module utilities