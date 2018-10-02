module basic_controller_fcns
use misc_mod
! Types
type basicdr
    integer :: strat , fullload
    real*8 :: dr, pset, kval , Kopt, omega_rated
    real*8 :: wsp_r, wsp_dr, TSR, R
end type basicdr

type(basicdr) basicst


! ** 
type table_KDR
    integer :: nentry
    real(mk) tabkdr(1000,2)  
end type table_KDR 
type(table_KDR) kdr


 type Tpidvar
      real(mk) Kpro,Kdif,Kint,outmin,outmax,velmax,error1,outset1,outres1
      integer stepno1
      real(mk) outset,outpro,outdif,error1_old,outset1_old,outres1_old,outres
end type Tpidvar


type(Tpidvar), save        :: PID_pitch_var
integer :: stepno = 0
!*****************************************************************************************
    contains
!*****************************************************************************************



function interpolate2(x, x1, x2, y1, y2)
       
real(mk) interpolate2, x, x1, x2, y1, y2
    if (x1 .eq. x2) then
          interpolate2 = y1
    else
          interpolate2 = ((y2-y1)/(x2-x1))*(x-x1)+y1
    endif
       
       return
end function interpolate2 

function PID(stepno, dt, kgain, PIDvar, error)
   ! PID controller with one input.
   integer stepno
   real(mk) PID, dt, kgain(3), error
   type(Tpidvar) PIDvar
   ! Local vars
   real(mk) eps
   parameter(eps = 0.000001_mk)
   ! Initiate
   if (stepno.eq.1) then
      PIDvar%outset1 = 0.0_mk
      PIDvar%outres1 = 0.0_mk
      PIDvar%error1 = 0.0_mk
      PIDvar%error1_old = 0.0_mk
      PIDvar%outset1_old = 0.0_mk
      PIDvar%outres1_old = 0.0_mk
   endif
   ! Save previous values
   if (stepno.gt.PIDvar%stepno1) then
      PIDvar%outset1_old = PIDvar%outset1
      PIDvar%outres1_old = PIDvar%outres1
      PIDvar%error1_old = PIDvar%error1
   endif
   ! Update the integral term
   PIDvar%outset = PIDvar%outset1_old + 0.5_mk*(error + PIDvar%error1)*Kgain(2)*PIDvar%Kint*dt
   ! Update proportional term
   PIDvar%outpro = Kgain(1)*PIDvar%Kpro*0.5_mk*(error + PIDvar%error1)
   ! Update differential term
   PIDvar%outdif = Kgain(3)*PIDvar%Kdif*(error - PIDvar%error1_old)/dt
   ! Sum to up
   PIDvar%outres = PIDvar%outset+PIDvar%outpro + PIDvar%outdif
   ! Satisfy hard limits
   if (PIDvar%outres .lt. PIDvar%outmin) then
      PIDvar%outres = PIDvar%outmin
   elseif (PIDvar%outres .gt. PIDvar%outmax) then
      PIDvar%outres = PIDvar%outmax
   endif
   ! Satisfy max velocity
   if (PIDvar%velmax .gt. eps) then
      if ((abs(PIDvar%outres-PIDvar%outres1_old)/dt) .gt. PIDvar%velmax) then
         PIDvar%outres = PIDvar%outres1_old + dsign(PIDvar%velmax*dt, PIDvar%outres-PIDvar%outres1_old)
      endif
   endif
   ! Anti-windup on integral term and save results
   PIDvar%outset1 = PIDvar%outres - PIDvar%outpro - PIDvar%outdif
   PIDvar%outres1 = PIDvar%outres
   PIDvar%error1 = error
   PIDvar%stepno1 = stepno
   ! Set output
   if (stepno .eq. 0) then
      PID = 0.0_mk
   else
      PID = PIDvar%outres
   endif
   return
end function PID





end module basic_controller_fcns