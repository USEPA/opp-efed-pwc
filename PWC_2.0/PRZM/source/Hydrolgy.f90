module Hydrolgy
    implicit none
    contains
    
       
    SUBROUTINE Runoff_PRZM5
    !PRZM5: Calculate runoff first and then using any left over for the canopy & infiltration 
    !In conformance to NEH-4, initial abstraction is no longer altered by canopy
    
      use constants_and_Variables, ONLY: EchoFileUnit, precipitation, precip_rain, air_TEMP,sfac,snowfl,THRUFL, &
        cint,smelt,runof, curve_number_daily,ainf,potential_canopy_holdup,snow, &
        under_canopy_irrig, over_canopy_irrig,  canopy_flow, &
        effective_rain , julday1900, data_date,  model_runoff ,Calibrated_curve_Number, &
        SoilWater,cn_moist_node,soil_depth,cn_moisture
        
      implicit none
!     This subroutine calculates snowmelt, crop interception, runoff, and infiltration from the soil surface

      REAL   CURVN

      real :: canopy_capture
      
      RUNOF = 0.0
      SMELT = 0.0
      SNOWFL= 0.0
      effective_rain = 0.0
      canopy_flow= 0.0
      precip_rain = precipitation 
      
      IF (SFAC .GT. 0.0) THEN
!       Compute snowmelt and accumulation
        IF (air_TEMP .LE. 0.0) THEN
              precip_rain  = 0.0              !this is now rain only  
              SNOWFL       = precipitation    
              SNOW         = SNOW + SNOWFL
        ELSE
              precip_rain = precipitation     !this is now rain only
              SMELT       = AMIN1(SFAC*air_TEMP,SNOW)
              SNOW        = SNOW- SMELT
        ENDIF
      ENDIF

      effective_rain  = under_canopy_irrig + over_canopy_irrig + precip_rain + smelt  !used for runoff calc only  (canopy not included)
              
      call Curve_Number_Adjustment(curvn)

      
      call Calculate_Runoff_PRZM5(curvn,Effective_Rain)


      !Here runoff has preference since according to CN method canopy would already be accounted for:
      canopy_capture = min(potential_canopy_holdup-CINT, (precip_rain + over_canopy_irrig - runof)  )
      canopy_capture = max (0.0, canopy_capture) !for under canopy irrigation the above could be negative
       
      cint        = cint + canopy_capture
      canopy_flow = max (0.0, over_canopy_irrig + precip_rain -canopy_capture ) !used for pesticide washoff calcs
      
      !Thrufl is now only the amount of water actually hitting the ground  used for erosion calcs
      !Might need rethinkin to remove canopy if already accounted for in C factors
      !used effective rain for time of conc in erosion routine
      THRUFL = over_canopy_irrig + precip_rain -canopy_capture + under_canopy_irrig
      
      ! Compute infiltration for first soil compartment
      AINF(1) = AINF(1) + Effective_Rain- RUNOF -canopy_capture
        
      curve_number_daily = CURVN  !store curve number for later output display
          
      !the following used for calibration only:

      where (data_date == julday1900)  model_runoff = runof
      where  (data_date == julday1900) Calibrated_curve_Number=curvn      
      where  (data_date == julday1900) cn_moisture = sum(SoilWater(1:cn_moist_node)) / soil_depth(cn_moist_node)   
      
    END SUBROUTINE Runoff_PRZM5
    
    
 
     !****************************************************************************************** 
    Subroutine Curve_Number_Adjustment(curvn)
    !moisture adjustments to the curve number
    !CN_index is determined based on the julian day and sets the curve number until the next change
    !Nov 5 2014, dfy changed curve number 2 to a real number and allowed interpolation between whole curve numbers   
    
        use constants_and_Variables, ONLY:CN_moisture_ref, CN_index, CN_2, SoilWater,cn_moist_node,ADJUST_CN,soil_depth
        use curve_number_table
        implicit none
        real,intent(out) :: curvn
        integer :: i
        
        real :: curve_number1, curve_number2,curve_number3, twlvl
         
        IF (ADJUST_CN) then
            !Water amount in top 10 cm used for CN manipulation
            TWLVL = sum(SoilWater(1:cn_moist_node)) / soil_depth(cn_moist_node)  !average water content per depth in top 10 cm

            !Interpolation
            i= int (CN_2(CN_index)) !integer portion of CN_2
            curve_number2  =  CN_2(CN_index)
            curve_number1  =  cn_1(i) + (curve_number2- int(curve_number2)) *(cn_1(i+1)-cn_1(i))
            curve_number3  =  cn_3(i) + (curve_number2- int(curve_number2)) *(cn_3(i+1)-cn_3(i))   
            CURVN = curve_number2 + (curve_number3 -  curve_number2) *  (TWLVL-CN_moisture_ref)/ CN_moisture_ref       
            if (twlvl < CN_moisture_ref ) CURVN = curve_number1  + (curve_number2 - curve_number1)* twlvl/CN_moisture_ref
               
        else
            CURVN =  CN_2(CN_index)
        end if
               
     
         
    End Subroutine Curve_Number_Adjustment
    
     
  
    !****************************************************************rf*************************
    Subroutine Calculate_Runoff_PRZM5(curvn,Effective_Rain)
       use constants_and_Variables, ONLY:RUNOF,inabs, EchoFileUnit
       implicit none
       real, intent(in) :: curvn
       real,intent(in)  :: Effective_Rain
    
       !the constant 0.508 is derived from 0.2 * 2.54 cm/in and 0.2 is from INABS = 0.2 * S, where S is (1000./CURVN-10.).
       !INABS: Initial Abstraction INABS = AMAX1(0.508* (1000./CURVN-10.),PRECIP-THRUFL)
       !THe Original NRCS Curve Number nmethod already accounts for canopy holdup.  -DFY

       INABS = 0.508* (1000./CURVN-10.)  !also used in erosion calculation
       IF (Effective_Rain .GT. INABS) then
           RUNOF= (Effective_Rain-INABS)**2/ (Effective_Rain + (4.0* INABS))  !OK the 4 is from 0.8 = 4 * 0.2
       else 
           runof = 0.0
       end if

    end  Subroutine Calculate_Runoff_PRZM5  
     
     

    
   SUBROUTINE Leaching
      use  constants_and_Variables, ONLY: soilwater,EvapoTran,theta_zero,DELX,theta_end,ainf,ncom2, & 
            vel, thair_old, THAIR_new,theta_sat,theta_fc
      implicit none
!     Performs hydraulic calculations assuming a uniform soil profile with unrestricted drainage
!     (drainage occurs instantaneously)
      !Also calculates the Air space at start and end of time step

      INTEGER      I


      do I=1,NCOM2
        

        theta_zero(I) = soilwater(I)/DELX(I)

        theta_end(I) = (soilwater(I)+ AINF(I)- EvapoTran(I))/ DELX(I)

        AINF(I+1)= 0.0



        
         IF (theta_end(I) .GT. theta_fc(I)) THEN
             

                AINF(I+1)= (theta_end(I)- theta_fc(I))* DELX(I)
                theta_end(I) = theta_fc(I)
         ENDIF
         

   
   
         VEL(I)= AINF(I+1)/theta_end(I)
         soilwater(I) = theta_end(I)*DELX(I)
         
         
      end do
  
   
      
      
      !***** Calculate Air space and gas diffusion coefficient
        thair_old = theta_sat - theta_zero
        where (thair_old < 0.0) thair_old = 0.0
 
        THAIR_new = theta_sat - theta_end
        where (THAIR_new < 0.0) THAIR_new = 0.0

   
        
        
   END SUBROUTINE Leaching
      

end module Hydrolgy
    