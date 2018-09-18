module yaw_mod
   use yaw_controller_fcns
   
   contains
!**************************************************************************************************
   subroutine init_yaw(array1,array2)
!      use write_version_mod
      implicit none
      !DEC$ IF .NOT. DEFINED(__LINUX__)
      !DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'init_yaw'::init_yaw
      !DEC$ END IF
      real(mk) array1(100), array2(1)
      real(mk) ,dimension(yawst%larray) :: arrayaux
      ! Input array1 must contain
      !    1: constant 1 ; Time Start 
      !    2: constant 2 ; Array distance (mean deficit /dt)
      !    3: constant 3 ; Threshold (deg)

      
      yawst%tstart = array1(1)
      yawst%larray = array1(2)
      yawst%threshold = array1(3)
      yawst%memory = array1(4)    
      yawst%larray2 = array1(5) 
      array2 = 0.0_mk
      yawst%arrayaux(1,1:yawst%larray) = 0.0
      yawst%lastyaw=  0.0
      yawst%ct = 0 
    
      write(0,*) ' Exit init'
   end subroutine init_yaw
!**************************************************************************************************
!**************************************************************************************************
!**************************************************************************************************
   subroutine update_yaw(array1, array2)
      implicit none
      !DEC$ IF .NOT. DEFINED(__LINUX__)
      !DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'update_yaw'::update_yaw
      !DEC$ END IF
        real*8 array1(100), array2(100), arrayaux(10000000)
        real*8 :: yaw_cur,time,val ! current yaw angle , time 
        real*8 :: yaw_err, yaw_err2 , lastyaw
        real, DIMENSION(yawst%larray) :: aux_var
        real*8 :: angle_mis
        real*8 :: nac_ang
        real*8 :: yaw_flag, thresholdap, err_memr
        
        ! ** 3 component for velocity (wind + direction) 
        ! ** Get wind direction based on wind speed (degrees)
        angle_mis = atan(array1(2)/array1(3))*(180/pi)
        ! * Get current nacelle angle 
        nac_ang = array1(5)
        ! * Helps initialization of moving average vector in case wrong settings has been defined 
        ! if moving average is not complete when yawing is done it is completed
        
       
        if(yawst%memory.gt.0) then  
        !** Option for memory allocation / erase after yawing !    
        write(0,*) 'acces here'
        
        if (yawst%flagyaw.eq.0) then                                                         ! generating arrays 
          
            yawst%arrayaux(1,2:yawst%larray) =  yawst%arrayaux(1,1:yawst%larray-1)
            yawst%arrayaux(1,1) = angle_mis
            ! update counter    
            yawst%ct = yawst%ct +1                                                      ! normal mean + update counters
            
            
        elseif (yawst%flagyaw.eq.1) then 
                        
            if (yawst%ct.lt.yawst%larray) then                                          ! if the array has not completed before evaluating yaw, we complete it with the mean
                 arrayaux(1:yawst%ct) = yawst%arrayaux(1,1:yawst%ct)
                err_memr = sum(arrayaux) /yawst%ct            
                yawst%arrayaux(1,1:yawst%larray) =  err_memr
                write(0,*) 'memory has been completed with previous values'
                write(0,*) , 'new array is : ', yawst%arrayaux(1,1:yawst%larray)
                yawst%ct = yawst%larray
                
            
            else    ! means that we have updated it already / normal mean average ! 
                
                yawst%arrayaux(1,2:yawst%larray) =  yawst%arrayaux(1,1:yawst%larray-1)
                yawst%arrayaux(1,1) = angle_mis
        
            endif
        endif
        endif
            
       
        !** Moving average is not reset  
        
       if(yawst%memory.eq.0) then 
            yawst%arrayaux(1,2:yawst%larray) =  yawst%arrayaux(1,1:yawst%larray-1)
            yawst%arrayaux(1,1) = angle_mis
       endif
        
        ! compute average mean 
        ! 
        aux_var = yawst%arrayaux(1,1:yawst%larray)
        yaw_err = sum(aux_var) / yawst%larray
        yaw_err2 = sum(aux_var(1:yawst%larray2)) / yawst%larray2
        ! go for lower threshold when yawing / avoid in-out close to stop 
        
        if (yaw_flag.eq.1) then 
        thresholdap = yawst%threshold /2
        else
       thresholdap = yawst%threshold
        endif
        
        
        if (((array1(1)-yawst%lastyaw).gt.yawst%tstart).and.(abs(yaw_err).gt.thresholdap)) then        ! if time is greater than tinit + threshold 1 
              yawst%flagyaw = 1                                                                         ! start yawing 
              array2(1) = 100*yaw_err*(pi/180)                                                          ! we are asking the controller to continue yawing until threshold 
        
        elseif ((yawst%flagyaw.eq.1).and.(abs(yaw_err2).gt.thresholdap)) then                                  ! If we are yawing lower than second  threshold
             
             yawst%flagyaw = 0                                                                           ! stop    
             array2(1) = nac_ang
             yawst%lastyaw = array1(1)
             
             if(yawst%memory.gt.0) then 
                 yawst%arrayaux(1,1:yawst%larray) = 0
                 
                 write(0,*) ' Yaw has been performed'
                 write(0,*) 'Moving average memory is removed' 
                 write(0,*) yawst%arrayaux(1,1:yawst%larray) 
             
             endif
        
        else
            array2(1) = nac_ang
            yawst%flagyaw =  0 
        
        endif
        
        
        write(0,*) 'nacelle position is: ', nac_ang*(180/pi)
        write(0,*) ' Vx', array1(2)
        write(0,*) 'Vy' , array1(3)
        write(0,*) 'angle miss' , angle_mis
        
   end subroutine update_yaw
!**************************************************************************************************
! *** Interpolate function ***

end module yaw_mod
