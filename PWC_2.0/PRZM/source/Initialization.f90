module initialization
implicit none
contains

    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!     
SUBROUTINE INITL
  use constants_and_Variables, ONLY: min_evap_depth,                                    &
        SoilWater,fieldcap_water, wiltpoint_water,delx,                        &
        theta_zero_input,      theta_zero,                                     &
        sand_input,            sand,                                           &
        clay_input,            clay,                                           &
        bd_input,              bulkdensity,                                    &
        N_f_2_input,           N_freundlich_2,                                 &
        k_f_2_input,           k_freundlich_2,                                 & 
        k_f_input,             k_freundlich,                                   &
        N_f_input,             N_freundlich,                                   &
        fc_input,              theta_fc,                                       &
        wp_input,              theta_wp,                                       &
        soil_temp_input,       soil_temp,                                      &
        theta_sat, juslec,NUSLEC,CN_moisture_ref,RNCMPT,ncom2,                           &
        TAPP,  application_rate, num_applications,                             &
        old_Henry,new_henry, HENRYK,orgcarb, oc_input, NCHEM,NHORIZ,thickness, &
        OUTPUJ,OUTPJJ,Num_delx,startday, soil_depth,                                                       &
        Num_Days_into_crop,theta_end,CN_index,cfac,n1,USLEC,mngn,EchoFileUnit,                             &
        cn_moist_node, runoff_effic, runoff_decline,runoff_intensity,runoff_extr_depth,                    &
        CAM,cam123_soil_depth,DEPI, &
        CN_moisture_depth,erosion_intensity,erosion_decline,erosion_depth,erosion_effic , erosion_compt, &
        GYUSLEC,GMUSLEC,GDUSLEC,use_usleyears,number_washoff_nodes, washoff_incorp_depth, &
        user_irrig_depth_node, UserSpecifiesDepth,user_irrig_depth,   Kd_new ,Kd_old,  &
        conc_total_per_water, mass_in_compartment, conc_porewater, number_subdelt, subdelt, delt, &
        Sorbed2, aq_rate_input, sorb_rate_input, gas_rate_input, &
        dwrate_atRefTemp, dsrate_atRefTemp, dgrate_atRefTemp, dwrate, dsrate, dgrate, &
        MolarConvert_aq12,MolarConvert_aq13,MolarConvert_aq23,                &
        MolarConvert_s12, MolarConvert_s13, MolarConvert_s23, &
        MolarConvert_aq12_input, MolarConvert_aq13_input, MolarConvert_aq23_input, &
        MolarConvert_s12_input ,MolarConvert_s13_input , MolarConvert_s23_input,  &
        dispersion_input,dispersion, ainf, GAMMA1,uptkf, vel, KD_2,  first_year,Min_Evap_node,soil_applied_washoff
      
     
  use utilities
  use allocations
 ! use oldPRZM3
  implicit none
 ! integer  :: ISDAY,ISMON,ISTYR !INPUT from  weather file, start date
  INTEGER           i,k

  LOGICAL           FATAL

  real            :: delx_avg_depth
  integer         :: startday_doy !  startday day of year  number of days past Jan 1, used for erosion
  integer         :: day_difference, smallest_difference
  real, parameter :: tol = 0.01  !some kind of tolerance for compartment size, Needs checking

  integer :: start, xend

    CHARACTER(len = 80):: note  !used for sending error messages
   
    !For sub delt calculations
    subdelt = delt/number_subdelt
    
   ! is_harvest_day = .FALSE.  will now be allocated in the crop routine
    
    !********Allocations of Soil Profile Variables**********************
    NCOM2 = sum(num_delx(1:nhoriz))  !Total Number of Compartments
    
    
    
    call  allocate_soil_compartments
    
    ainf = 0.0
   
    vel = 0.0
    
     GAMMA1  = 0.0
     do i = 1, nchem
           GAMMA1(i,:)= UPTKF(i)

     end do
     
     
    ! *** Populate the Delx Vector and Kf & N *******
    start = 1
    Xend = 0
     
    do i=1, nhoriz
        xend = start +num_delx(i)-1
        
        delx(start:xend)           = thickness(i)/num_delx(i) 
        bulkdensity(start:xend)    = bd_input(i)
        clay(start:xend)           = clay_input(i)
        sand(start:xend)           = sand_input(i)
        orgcarb(start:xend)        = oc_input(i)
        theta_zero(start:xend)     = theta_zero_input(i)
        theta_fc(start:xend)       = fc_input(i)
        theta_wp(start:xend)       = wp_input(i)
        soil_temp(start:xend)      = soil_temp_input(i)
        
        MolarConvert_aq12(start:xend)  = MolarConvert_aq12_input(i)
        MolarConvert_aq13(start:xend)  = MolarConvert_aq13_input(i)
        MolarConvert_aq23(start:xend)  = MolarConvert_aq23_input(i)
        MolarConvert_s12(start:xend)   = MolarConvert_s12_input(i)
        MolarConvert_s13(start:xend)   = MolarConvert_s13_input(i)
        MolarConvert_s23(start:xend)   = MolarConvert_s23_input(i)
        
        dispersion(start:xend)    = dispersion_input(i)

         do K=1, NCHEM
             k_freundlich(k, start:xend) = k_f_input(k,i)
             N_freundlich(k, start:xend) = N_f_input(k,i)
             
             k_freundlich_2(k, start:xend) = k_f_2_input(k,i)
             N_freundlich_2(k, start:xend) = N_f_2_input(k,i)
                        
             !KD(k, start:xend)= k_f_input(k,i)                !Used in Freundlich linearization for tridiagonal
 
  
             dwrate_atRefTemp(k,start:xend) =     exp(aq_rate_input(K,i)) -1.  !fraction removed per day
             dsrate_atRefTemp(k,start:xend) =     exp(sorb_rate_input(K,i)) -1.
             dgrate_atRefTemp(k,start:xend) =     exp(gas_rate_input(K,i)) -1.
         end do                       

        start = xend+1
    end do
    
    !As a default set the degradation rate to equal the input values
      dwrate = dwrate_atRefTemp
      dsrate = dsrate_atRefTemp
      dgrate = dgrate_atRefTemp


  
   theta_sat = 1.0 - bulkdensity/2.65
    if (any(theta_fc > theta_sat)) then
          WRITE(EchoFileUnit,* ) 'Water capacity exceeds saturation.'
    end if
    
    !If Linear Isotherms are used the reading Freundlich coefficient are used as Kd
    Kd_new  = k_freundlich   !used for Freundlich routines in tridiagonal 
    Kd_old  = k_freundlich 
    
    KD_2   = k_freundlich_2
    
    soil_applied_washoff  = 0.0
    conc_total_per_water = 0.0    
    mass_in_compartment = 0.0
    
    conc_porewater = 0.0
    Sorbed2 = 0.0   !nonequilibrium phase concentration
    

    !*** Populate Soil Depth Vector *********
    soil_depth = 0.0
    soil_depth(1) = delx(1)
    
    do i=2, NCOM2
       soil_depth(i) = soil_depth(i-1) + delx(i) 
    end do
 
    !*** Calculate Runoff Depth   ****************
    
    cn_moist_node = find_depth_node(ncom2,soil_depth,CN_moisture_depth)
    
    !*** Find the node for the  min_evap_depth (formerly) ANETD***************
    Min_Evap_node =  find_depth_node(ncom2,soil_depth,min_evap_depth)
    
    !Find the node for user-specified irrigation depth
      If (UserSpecifiesDepth) then
          user_irrig_depth_node =  find_depth_node(ncom2,soil_depth,user_irrig_depth)
      end if
    

    !initialize the water content in the profile to the THETO (previously initailized in SUBROUTINE INIACC)
    theta_end = theta_zero 
 
    OUTPUJ=0.0   !output accumulators, only used in output, could be initialzed with save attribute there
    OUTPJJ=0.0

    !initializehenry's law constant for first time here
    !These will also be the values used when temperature is not used 
    do K = 1, NCHEM
        old_Henry(K,:) = HENRYK(K)
        new_henry(K,:) = HENRYK(K)
    end do

     

    !REWRITE CROP INITIALIZATION
     !      is_cropgrowing = .False.
           Num_Days_into_crop = 0
    
     !dfy September 19, 2012
     !changed to less than or equal to prevent jan 1 error for emergence dats                   
     ! IF ((ISTYR. LT. IYREM(1)) .OR. (ISTYR .EQ. IYREM(1)
     !1     .AND. (SJDAY .LT. IEMER(1))     )) THEN  !******************

     !
     !if (startday <= emergence_date(1)) then
     !   !removed multiple crops  9/27/16 by dfy
     !   !this part of code executed if simulation start date is before first crop emergence date  
     !    
     !   is_cropgrowing = .False.
     !   Num_Days_into_crop = 0
     !ELSE          
     !   !this part of code is executed if simulation start date is after first crop emergence date
     !   !find crop number of current crop at beginning of simulation
     !    
     !   is_cropgrowing  = .True.
     !   
     !
     !   i=1
     !   do  !Find the first emergence date after the the startdate.        
     !      if (startday <= emergence_date(i)) then
     !        NCP = I-1
     !
     !        
     !        NCROP=1
     !      ENDIF
     !      I= I+ 1
     !      IF (NCP /= 0 .OR. I .GT. num_crop_periods) exit
     !   end do 
     !   
     !   IF (NCP .EQ. 0) THEN
     !     NCP = num_crop_periods
     !     NCROP = INCROP(NCP)
     !   ENDIF
     !      
     !   !find the total number of days from emergence date to crop maturity for beginning crop
     !
     !   !Replacement for the previoyus confusing code.
     !   !untested, so needs to be checked if used
     !   if (startday >= harvest_date(ncp)) then 
     !     is_cropgrowing = .False.  
     !   !  RZI= 0
     !     Num_Days_into_crop= 0
     !   else    
     !    Num_Days_into_crop = emergence_date(NCP) - startday +1
     !   end if
     !
     !ENDIF 

      
     
     
     


     !**** Initialization of Curve Number and Erosion parameters*****  
     select case (use_usleyears) 
     case (1)  !Erosion Parameters and Curve Numbers are Year Specific
           CN_index = 1  
           !For case 1, JUSLEC is referenced to Jan 1, 1900     
           do I=1,NUSLEC  
              JUSLEC(I)= jd(GYUSLEC(I),GMUSLEC(I),GDUSLEC(I))       
           end do 

           smallest_difference = 1e6
           do i= 1,NUSLEC
               day_difference =  abs(startday - JUSLEC(i))
               if (day_difference < smallest_difference) then
                  smallest_difference = day_difference
                  CN_index = i   !This sets the index for initial CN and erosion parameters
               end if    
           end do


           
           
     case default  !Erosion Parameters and Curve Numbers  repeat every year
           !For default case JUSLEC is referenced to the Jan 1 of any year
        
           startday_doy = startday - jd(first_year,1,1)+1  
                 
           do I=1,NUSLEC 
              JUSLEC(I)= jd(1,GMUSLEC(I),GDUSLEC(I)) -jd(1,1,1) +1        
           end do 
   
           smallest_difference = 1000
                  
           do i= 1,NUSLEC               
               if (JUSLEC(i) > startday_doy) then
                 day_difference =  startday_doy - (JUSLEC(i)-365)
               else
                 day_difference =  startday_doy - JUSLEC(i)   
               end if
               
               if (day_difference < smallest_difference) then
                   smallest_difference = day_difference  
                   CN_index = i   !This sets the index for initial CN and erosion parameters
               end if
           enddo
                    
     end select
     
      cfac = USLEC(CN_index)
      n1 = MNGN(CN_index)
       !**********************
     
  
!****************************************
      !Calculate Average Soil moisture in moisture zone (spec'd as a parameter) 
      !altered on 8/24/17 to average over depth rather than nodes so that it is consitent
      !when used to compared to actual soil moisture in HYDROLGY routine
      
      CN_moisture_ref  = 0.0
      
      do I=1,cn_moist_node
        CN_moisture_ref = CN_moisture_ref + 0.5*(theta_fc(I)+theta_wp(I))*delx(i)
      end do
       
      CN_moisture_ref = CN_moisture_ref /soil_depth(cn_moist_node)
   
!***********************************************************      
  

      soilwater       = theta_zero*DELX
      fieldcap_water  = theta_fc*  DELX
      wiltpoint_water = theta_wp*  DELX

      
      
      !!***  Find the Maximum Root Node **********
      !rzd = maxval(max_root_depth(1:num_crops))
      !
      !NCOMRZ = find_depth_node(ncom2,soil_depth,rzd)

      
      !Calculate number of compartments which make upwashoff incorporation
      number_washoff_nodes =  find_depth_node(ncom2,soil_depth,washoff_incorp_depth) 
 

      !*** EXTRACTION of RUNOFF AND EROSION FROM SOIL ****************************
         !** Runoff Extraction Intensity
         RNCMPT =  find_depth_node(ncom2,soil_depth,runoff_extr_depth) !Calculate number of compartments which make up runoff
         runoff_intensity=0.0   

         
     if (runoff_decline > 0.0001) then
      do I=1,RNCMPT
        delx_avg_depth =  soil_depth(i) - DELX(I)/2.
        runoff_intensity(I)=runoff_effic*runoff_decline/(1-exp(-runoff_decline*runoff_extr_depth)) &
                            *exp(-runoff_decline*delx_avg_depth)
      end do  
    else 
       runoff_intensity(1:rncmpt)=runoff_effic/runoff_extr_depth
    end if
    

         
         !** Erosion Extraction Intensity    
         erosion_compt =  find_depth_node(ncom2,soil_depth,erosion_depth) !Calculate number of compartments which make up runoff
         erosion_intensity=0.0  
         !Using soil_depth(erosion_compt) instead of erosion_depth to prevent some glitchy things like when depths are less than delx

         if (erosion_decline > 0.0001) then
             do i=1,erosion_compt
                delx_avg_depth =  soil_depth(i) - DELX(I)/2.
                erosion_intensity(i)=erosion_effic*erosion_decline/(1.0-exp(-erosion_decline*soil_depth(erosion_compt)))*exp(-erosion_decline*delx_avg_depth)
             end do   
         else !essentially no decline uniform extraction
             
             !need to limit this when depth is less than compartment !!!!!!
             !perhaps set it to the nodes
             !fixed it with erosion_numerical_depth

             erosion_intensity(1:erosion_compt) = erosion_effic/soil_depth(erosion_compt)
         end if
         
   

!     perform units conversions for input variables
!     COVMAX..PERCENT--->FRACTION
!     WFMAX...KG/M**2--->G/CM**2
!     TAPP... KG/HA  --->G/CM**2

      !COVMAX = COVMAX/100.
      !WFMAX = WFMAX/10.   
         
      TAPP = application_rate/1.0E5
     

     
      DO I = 1, num_applications

            Select Case(CAM(I))
            Case(1:3)
                DEPI(I) = cam123_soil_depth
            Case(4:10)
                  If (DEPI(I) < delx(1)) Then
                      DEPI(I) = delx(1)
                      write (EchoFileUnit,*) 'Note: Minimum incorporation = ', Delx(1)
                  End If
            Case Default
                  note = 'CAM value is unheard of.'
                  FATAL = .True.
                  CALL ERRCHK(note,FATAL)
            End Select

     End Do
     
     


end subroutine INITL
      






end module initialization   