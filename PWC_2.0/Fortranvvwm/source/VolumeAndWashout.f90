module volumeAndwashout

contains

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    subroutine constant_volume_calc
        !This subroutine calculates volume and washout rate
        use utilities
        use variables, ONLY: area, depth_0,  flow_averaging
        use nonInputVariables, Only: num_records, & 
                                     q,           &  !input (m3/sec) array of daily runoff
                                     k_flow,      &  !output array of daily wash out rates (per second)
                                     volume1,     &  !for this case, v=constant, but keep array to be consistent with program
                                     daily_depth, &
                                     Daily_avg_flow_out
        implicit none


        real,dimension(num_records):: q_avg        !Adjustable-day flow average
        real:: v_0                                 ! water body volume
        Daily_avg_flow_out = 0.0
        
        
        v_0 = area*depth_0
        
        if (flow_averaging ==0) then
              q_avg = sum(q)/num_records 

        else
             call window_average(q,flow_averaging,num_records,q_avg) !calculate 30-day previous average  
        endif 

        k_flow = q_avg/v_0   !array of washout rates
        volume1= v_0
        
        
        daily_depth = depth_0 
        
        
         Daily_avg_flow_out = sum(q_avg)/num_records    !used for output characterization only
        
    end subroutine constant_volume_calc
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    subroutine volume_calc
        !This subroutine calculates volume and washout rate
        
        use variables, ONLY: area, depth_0, depth_max, minimum_depth, DELT
        use nonInputVariables, Only: num_records, &
                                     k_flow,      & !output array of daily wash out rates (per second)
                                     q,           & !input (m3/sec) array of daily runoff
                                     volume1,           & !output, 
                                  evap,           & !daily evaporation (m)
                                 precip,          & !daily precipitation (m)
                                 daily_depth,     & !OUTPUT (m)
                                 Daily_avg_flow_out
              
        implicit none
        integer:: day
        real:: v_0                                  !initial water body volume [m3]
        real:: v_max                                !maximum water body volume [m3]
        real:: v_min                                !minimum water body volume [m3]
        real:: v_previous
        real:: check
        real,dimension(num_records)::vol_net
        real,dimension(num_records)::evap_area
        real,dimension(num_records)::precip_area
        Daily_avg_flow_out = 0.0
        
        
        v_0 = area*depth_0
        v_max = area*depth_max
        v_min = area*minimum_depth
        k_flow = 0.        !sets all values of the array to zero
        v_previous = v_0

        precip_area = precip*area /86400.    !m3/s
        evap_area = evap*.7*area /86400.    !m3/s, evap factor (.7) 

        vol_net = (q-evap_area+precip_area)*delt  !volume of water added in day; whole array operations

        do day = 1,num_records
            check = v_previous + vol_net(day)
            if (check > v_max) then
                volume1(day) = v_max
                k_flow(day) = (check-v_max)/delt/v_max   !day # and washout VOLUME
            else if (check < v_min) then
                volume1(day) = v_min
            else
                volume1(day) = check
            end if
            v_previous = volume1(day)
            !*******************************************************
            !ADDED FOR MARK CORBIN*********************************
              !  write(60,*)day,v(day),k_flow(day)*delt*v_max
            !********************************************************
        end do
        
        
        Daily_avg_flow_out = sum(k_flow)*v_max/num_records  !used for output characterization only

        daily_depth = volume1/area !whole array operation

    end subroutine volume_calc
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    subroutine burial_calc(koc)
    use variables, ONLY: froc2, burialFlag
    use nonInputVariables, Only:  capacity_2, & !input 
                                 k_burial, &   !output(kg/sec)
                                 burial        !input sediment rate
    implicit none
    real,intent(in) :: koc
    
    real :: kd_sed_2  


    if (burialFlag ) then
      kd_sed_2 = KOC*FROC2*.001       !Kd of sediment  [m3/kg]
      k_burial=  burial* kd_sed_2/capacity_2
       else
        k_burial = 0.
    end if
   
  
    end subroutine burial_calc
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%







end module volumeAndwashout