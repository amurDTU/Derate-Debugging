module basic_controller_fcns
use misc_mod
! Types
type basicdr

integer :: strat , fullload
real*8 :: dr, pset, kval , Kopt
end type basicdr

type(basicdr) basicst


! ** 
type table_KDR
    
integer :: nentry
real(mk) tabkdr(1000,2)  

end type table_KDR 
type(table_KDR) kdr


!*****************************************************************************************
    contains
!*****************************************************************************************

function average_error(stepno,sizemoving,val)

real(mk) :: average_error(1)
! ******
real(mk) :: arrayval(1000000)
real(mk) :: arraymoving(1000000)
real(mk) :: val, errval , errout
integer :: stepno, sizemoving
! *** main program ** 
! ** 
if (stepno.lt.2) then
! start parameters with stepno  / arraymoving and arrayval
arraymoving(1:sizemoving) = 200        
arrayval(1:sizemoving) = 0
arrayval(1) =  val
else
! moving average 
!       
! **move average
arrayval(2:sizemoving) = arrayval(1:sizemoving-1)
arrayval(1) = val
! ** move value 
errval = val-arrayval(2)
arraymoving(2:sizemoving) = arraymoving(1:sizemoving)
arraymoving(1) = abs(errval)

! ** error out moving average
errout = sum(arraymoving(1:sizemoving))/sizemoving

endif
! ** out
! **
average_error(1) = errout

end function average_error

function interpolate2(x, x1, x2, y1, y2)
       
real(mk) interpolate2, x, x1, x2, y1, y2
    if (x1 .eq. x2) then
          interpolate2 = y1
    else
          interpolate2 = ((y2-y1)/(x2-x1))*(x-x1)+y1
    endif
       
       return
end function interpolate2 

    
end module basic_controller_fcns