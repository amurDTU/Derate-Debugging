module yaw_mod
   use basic_controller_fcns
   
   contains
!**************************************************************************************************
   subroutine init_controller(array1,array2)
      implicit none
      !DEC$ IF .NOT. DEFINED(__LINUX__)
      !DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'init_controller'::init_controller
      !DEC$ END IF
      real(mk) array1(100), array2(1), arrayK(50), arraypitch(30)
      integer :: i , i2 , Ind0 , Ind1
      real*8 :: a , a2
      

      ! Input array1 must contain
      !    1: constant 1 ; Strategy 
      !    2: constant 2 ; Derate percentage
      !    3: constant 3 ; Power setpoint 
      ! * get R turbine
      basicst%strat = array1(1)
      basicst%Kopt = array1(2)
      basicst%dr = array1(3)
      basicst%pset = array1(4)
      PID_pitch_var%Kpro = array1(5)
      PID_pitch_var%Kint = array1(6)
      PID_pitch_var%Kdif = array1(7)
      
      PID_pitch_var%outmin = -100
      PID_pitch_var%outmax = 100
      PID_pitch_var%velmax = 100
      write(0,*) 'Access basic DTU-Controller Derate....'
      
      
      ! Pre-process data if needed
      select case (basicst%strat)
          
      case(1)           ! ** reading table DR vs K 
          write(0,*) 'Reading file -- Dr vs K ' 
          open(22,file='./control/K_DR.txt') 
          read(22,*) ! BLANK LINE
          read(22,*) kdr%nentry
          do i=1,kdr%nentry
          read(22,*) kdr%tabkdr(i,1), kdr%tabkdr(i,2)
          enddo
     
          Ind0 = 0                                              ! Init counter
          do while(basicst%dr.gt.kdr%tabkdr(Ind0,1))
          Ind0 = Ind0 +1                                        ! Update counter 
          if (Ind0.eq.kdr%nentry) then
                basicst%dr = basicst%dr-50000
          endif
          enddo
          
         Ind0 = min(Ind0,kdr%nentry)
         Ind0 = Ind0 -1 
         Ind1 = min(Ind0+1,kdr%nentry) 
   
          
         basicst%kval = interpolate2(basicst%dr,kdr%tabkdr(Ind0,1),kdr%tabkdr(Ind1,1),kdr%tabkdr(Ind0,2),kdr%tabkdr(Ind1,2))
          
     
          
      case(2)
          
          
      end select
 end subroutine init_controller
!**************************************************************************************************
!**************************************************************************************************
!**************************************************************************************************
   subroutine update_controller(array1, array2)
      implicit none
      !DEC$ IF .NOT. DEFINED(__LINUX__)
      !DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'update_controller'::update_controller
      !DEC$ END IF
      real(mk) array1(100) , array2(100)
      real(mk) :: time , omega , wsp 
      real(mk) :: pitch_out, tq_out , drK , K 
      real(mk) :: wsp_dr, omegadr
      real(mk) :: eomega
      integer :: partial_full
      real(mk) :: dummy, Kgain_pitch(3)
      real(mk) :: deltat = 0.02

      stepno = stepno + 1
      ! *** get update values in dt *** ! 
      time = array1(1)
      omega = array1(2)
      wsp = array1(3)
      
      wsp_dr = ((1-basicst%dr)**(0.333333333))*12
      
      
      
      if(wsp.gt.wsp_dr) then            ! we change from partial to full based on wsp and a wsp derate
          
      partial_full = 1                  ! we are in full load     
      else
      partial_full = 0                  ! we are in partial load
      endif
      
      ! ***     
      
      select case (basicst%strat) 
          
      ! De-rate strategy 1                              // K Omega**2  //  
      case(1) 
      
          
          
          select case (partial_full)
              
          case(0)                               ! partial load region 
        
           K = basicst%kval*basicst%Kopt
           tq_out = K*omega**2
           pitch_out = -1
               
          case(1)                               ! full load region 
          
          ! PID for Pitch  ( needs omegadr) 
          ! Torque ? 
          ! let's calculate the omegadr 
          
          omegadr = (2.3e6*(1-basicst%dr)/basicst%Kopt)**(0.33333)
          
          eomega = omega-omegadr
          Kgain_pitch = 1
          dummy = PID(stepno, deltat, Kgain_pitch, PID_pitch_var, eomega)
          write(0, *)  PID_pitch_var%outset, PID_pitch_var%outpro 
            
          
          
          
          
          ! program PID  
          
          
          pitch_out = dummy
          tq_out = basicst%Kopt*omegadr**2
          !write(0, *) tq_out
          end select
          
          
          
      case(2)
          
          
          write(0,*) 'case 2' 

          
          
      
      end select 
      
      
      
   
    array2(1) = tq_out
    array2(2) = pitch_out*(pi/180)
    
    
   
    ! **** Output DLL *** !
        
   end subroutine update_controller
!**************************************************************************************************


end module yaw_mod
