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
        !    constant 1 ; Strategy 
        !    constant 2 ; Kopt      
        !    constant 3 ; Derate percentage
        !    constant 4 ; Power setpoint 
        !    constant 5 ; Porportional gain for pitch control
        !    constant 6 ; Integral gain for pitch control
        !    constant 7 ; Derivative gain for pitch control
        ! * get R turbine
      
      
        basicst%strat = array1(1)
        basicst%Kopt = array1(2)
        
        if (basicst%strat .eq. 0) then
            basicst%dr = 0
        else
            basicst%dr = array1(3)
        endif
        
        basicst%pset = array1(4)
        
        
        basicst%omega_rated = (2.3e6/basicst%Kopt)**(1.0/3)
        PID_pitch_var%Kpro = array1(5)
        PID_pitch_var%Kint = array1(6)
        PID_pitch_var%Kdif = array1(7)
      
        PID_pitch_var%outmin = -1
        PID_pitch_var%outmax = 100
        PID_pitch_var%velmax = 100
        basicst%wsp_r = 10.657 ! rated wind speed calculated using(2300000/(0.94*0.49*0.5*1.225*np.pi*92.6^2/4))^(1.0/3)
        basicst%wsp_dr = basicst%wsp_r*(1 - basicst%dr)**(1.0/3)
        basicst%TSR = 8.85 ! confirmed in simulation for K = 256240
        basicst%R = 46.3
        
        write(0,*) 'Access basic DTU-Controller Derate....'
        write(0,*) 'Derated wind speed', basicst%wsp_dr
        write(0,*) 'Rated wind speed', basicst%wsp_r
      
        ! Pre-process data if needed
        select case (basicst%strat)
        case(0) ! No derating
        case(1) ! max omega strategy
        case(2) ! constant omega strategy
        case(3) ! constant tip speed ratio strategy
            
        case(9)           ! ** reading table DR vs K 
            write(0, *) 'Reading file -- Dr vs K ' 
            open(22, file='./control/K_DR.txt') 
            read(22, *) ! BLANK LINE
            read(22, *) kdr%nentry
            do i=1, kdr%nentry
                read(22, *) kdr%tabkdr(i, 1), kdr%tabkdr(i, 2)
            enddo
     
          
            ! interpolate the required value of K to achieve the desired derate percentage
            ! To do - encapsulate this into a single interpolation function.
            Ind0 = 0                                              ! Initialize counter
            do while(basicst%dr.gt.kdr%tabkdr(Ind0,1))
                Ind0 = Ind0 +1                                        ! Update counter 
                if (Ind0.eq.kdr%nentry) then
                    basicst%dr = basicst%dr-50000
                endif
            enddo
          
            Ind0 = min(Ind0, kdr%nentry)
            Ind0 = Ind0 - 1 
            Ind1 = min(Ind0 + 1, kdr%nentry) 
   
            basicst%kval = interpolate2(basicst%dr,kdr%tabkdr(Ind0,1),kdr%tabkdr(Ind1,1),kdr%tabkdr(Ind0,2),kdr%tabkdr(Ind1,2))
          
     
          
        
          
          
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
        real(mk) :: wsp_dr, omegadr, omega_target
        real(mk) :: eomega
        logical :: partial_load
        real(mk) :: dummy, Kgain_pitch(3)
        real(mk) :: deltat = 0.02

      
        time = array1(1)
        omega = array1(2)
        wsp = array1(3)
      
        
        !A naughty way of checking full or partial load region
        
        if(wsp .gt. basicst%wsp_dr) then            ! we change from partial to full based on wsp and a wsp derate
            partial_load = .FALSE.                  ! we are in full load     
        else
            partial_load = .TRUE.                  ! we are in partial load
        endif
      
    
        select case (basicst%strat)  
        case(0) ! No derating
            if (partial_load) then
                tq_out = basicst%Kopt*omega**2
                pitch_out = -1
            else
                if (time .ge. 0) then
                    omega_target = basicst%omega_rated
                
                    eomega = omega - omega_target
                    Kgain_pitch = 1
                    stepno = stepno + 1
                    dummy = PID(stepno, deltat, Kgain_pitch, PID_pitch_var, eomega)
              
                    ! program PID    
                    pitch_out = dummy
                    tq_out = basicst%Kopt*omega_target**2
                    !write(0, *) tq_out
                else
                    pitch_out = -1
                    tq_out = 0
                endif
          
            endif
            
            
        case(1) ! max omega strategy
            if (partial_load) then
                tq_out = basicst%Kopt*omega**2
                pitch_out = -1
            else
                omega_target = basicst%omega_rated
                eomega = omega - omega_target
                Kgain_pitch = 1
                stepno = stepno + 1
                dummy = PID(stepno, deltat, Kgain_pitch, PID_pitch_var, eomega)
              
                ! program PID    
                pitch_out = dummy
                tq_out = 2.3e6*(1 - basicst%dr)/omega_target
            endif
            
            
        case(2) ! constant omega strategy
            if (partial_load) then
                tq_out = basicst%Kopt*omega**2
                pitch_out = -1
            else
                omega_target = (2.3e6*(1 - basicst%dr)/basicst%Kopt)**(1.0/3)
                eomega = omega - omega_target
                Kgain_pitch = 1
                stepno = stepno + 1
                dummy = PID(stepno, deltat, Kgain_pitch, PID_pitch_var, eomega)
              
                ! program PID    
                pitch_out = dummy
                tq_out = basicst%Kopt*omega_target**2

            endif

            
            
        case(3) ! constant tip speed ratio strategy
            ! check if the wind speed is between derated and rated wind speed
            if ((wsp .GT. basicst%wsp_dr) .AND. (wsp .LT. basicst%wsp_r)) then
                omega_target = basicst%TSR * wsp/basicst%R
                eomega = omega - omega_target
                Kgain_pitch = 1
                stepno = stepno + 1
                dummy = PID(stepno, deltat, Kgain_pitch, PID_pitch_var, eomega)         
                ! program PID    
                pitch_out = dummy
                tq_out = basicst%Kopt*omega_target**2
                    
            elseif (partial_load) then
                tq_out = basicst%Kopt*omega**2
                pitch_out = -1
              
            else

                omega_target = (2.3e6*(1 - basicst%dr)/basicst%Kopt)**(1.0/3)
                
                eomega = omega - omega_target
                Kgain_pitch = 1
                stepno = stepno + 1
                dummy = PID(stepno, deltat, Kgain_pitch, PID_pitch_var, eomega)
              
                ! program PID    
                pitch_out = dummy
                tq_out = basicst%Kopt*omega_target**2

            endif
            
            
            
        case(9)   ! De-rate strategy 9                              // K Omega**2  //  
            if (partial_load) then ! 
                K = basicst%kval*basicst%Kopt
                tq_out = K*omega**2
                pitch_out = -1
                
             else
                ! PID for Pitch  ( needs omegadr) 
                ! Torque ? 
                ! let's calculate the omegadr 
          
                omegadr = (2.3e6*(1-basicst%dr)/basicst%Kopt)**(0.33333)
          
                eomega = omega-omegadr
                Kgain_pitch = 1
                stepno = stepno + 1
                dummy = PID(stepno, deltat, Kgain_pitch, PID_pitch_var, eomega)

                ! program PID    
                pitch_out = dummy
                tq_out = basicst%Kopt*omegadr**2
                
            endif
      
        end select 
      
        array2(1) = tq_out
        array2(2) = pitch_out*(pi/180)
        array2(3) = eomega
        
    end subroutine update_controller
    !**************************************************************************************************


end module yaw_mod
