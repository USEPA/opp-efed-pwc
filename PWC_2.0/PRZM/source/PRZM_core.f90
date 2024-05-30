module PRZM_CORE
   implicit none
   contains
   
   SUBROUTINE PRZM
   use  constants_and_Variables, ONLY: is_temperature_simulated,soil_temp, nonequilibrium_flag,              & 
    THRUFL,really_not_thrufl,COVER, USLEC,MNGN,N1,cfac,theta_end,bulkdensity,runof,  & 
    ainf,juslec,nuslec,erflag,ncom2,IRFLAG,sedl, &
    conc_total_per_water, mass_in_compartment,mass_in_compartment2, conc_porewater,    &
    ENPY,new_henry, old_Henry,Q_10,NCHEM,    some_applications_were_foliar,                         & 
     CN_index, application_date,under_canopy_irrig,over_canopy_irrig,julday1900, &
    use_usleyears,num_applications,FLAG1,enriched_eroded_solids,HENRYK, henry_ref_temp, &
    Kd_old,    kd_new, delx,   &
    thair_old, thair_new, & 
    theta_zero, delt, number_subdelt, subdelt, Sorbed2,   harvest_day , irrr,slp
   
   use Tridiagonal
   use Pesticide_Applications  
   use Temperatue_Calcs
   use Output_File
   use Hydrolgy
   use evapotranspiration
   use erosion
   use irrigation
   !use plantgrowth
   use utilities
   use nonideal_sorption
   use volatilization
   use plant_pesticide_processes
   implicit none
   

   INTEGER      :: I,K, J
   REAL         :: OLDKH(NCOM2)
   real         :: old_conc(NCOM2), new_conc(NCOM2)
   real         :: predicted_conc(NCOM2)
   real         :: conc1_neq(NCOM2)         !mobile region post nonequilibrium phase  conc
   real         :: S2_neq(NCOM2)            !region2 post nonequilibrium phase conc
   real         :: S2_old(ncom2)            !local sorbed2 concentration
   
   integer      :: julday  !this is the day of the year starting with Jan 1

   real :: theta_new_subday(NCOM2)
   real::  theta_old_subday(NCOM2)
   real :: delta_watercontent(NCOM2)  !local water contents for use with subdaily time steps
   real :: theta_air_new_subday(NCOM2)
   real :: theta_air_old_subday(NCOM2)
   real :: delta_aircontent(NCOM2)  !local water contents for use with subdaily time steps

   integer ::  current_year, current_month, current_day


!   real halftime !1/2 the time step for use initialization the predictor step
   
   !******************************************
  ! CALL PLGROW(julday1900) !PLANT GROWTH 

   AINF(1)= 0.0
   THRUFL = 0.0
   really_not_thrufl = .FALSE.
 
   !**********IRRIGATION ************************ 
   IF (IRFLAG ==1) then                         !irrigate any time
      CALL IRRIG_PRZM5
   else IF (IRFLAG ==2 .AND. cover > 0.0) then  !irrigate if there is a crop

      CALL IRRIG_PRZM5
   else                                         !no irrigation
      under_canopy_irrig = 0.0
      over_canopy_irrig = 0.0
      irrr = 0.0               ! added 8/19/16 fixed bug that extended a harvest day irrigation throughout noncrop period
   end if


   !Find out if there is a change in the curve number or erosion factors
   !Simplified routine. Now does not depend on order of erorsion factors.  And wont get stuck if first date is not emergence
  
   !this version of julian day is needed for the curve number and erosion routines
   call get_date (julday1900, current_year, current_month, current_day)
   
  
   julday = julday1900 -jd(current_year,1,1) +1
   
   select case (use_usleyears)
   case (1)
      do i = 1, nuslec
         if (julday1900  ==juslec(i))then
        
             cfac=USLEC(i)
             n1 = MNGN(i)
             CN_index = i  !this is used for curve number
             exit
         end if
      end do       
   case default
      do i = 1, nuslec
         if (julday ==juslec(i))then
  
           cfac=USLEC(i)
           n1 = MNGN(i)
           CN_index = i  !this is used for curve number
           exit
         end if
      end do
   end select

   Call Runoff_PRZM5  
   CALL EVPOTR                  !EVAPORATION 


   CALL Leaching                !determine water content of soil and air content (old and new)


   !*** EROSION *********************************
   !added slp condition 1/14/2020, bypass erosion if slope is zero, otherwise routine will fail
   IF (RUNOF .GT. 0.0 .AND. ERFLAG > 1 .AND. slp > 0.0) THEN  
     CALL EROSN(JULDAY)  !calc loss of chem due to erosion
   else 
       SEDL= 0.0
       enriched_eroded_solids= 0.0 
   END IF

   ! ****** Soil Temperature Calculations ************** 
   IF (is_temperature_simulated) CALL SLTEMP()                         
   IF (is_temperature_simulated  .AND. Q_10 > 0.0  ) CALL Q10DK ! ****** degradation Temperature adjustments **************  

   ! Application of Pesticide into System
   do i = 1,  num_applications   !check to see if this is an application day      
       if (application_date(i)==julday1900) CALL PESTAP(i)  
   end do
   
   !Note for application day:
       !Currently there is no iterative adjustment to Kd (in Freundlich case) following application. 
       !However, it is adjusted on the subsequent day
   
   !The pesticide from washoff is taken into account in the "F" term of tridiagonal, somewhat in a manner
   !that it is production over time rather than an intial condition
   if (some_applications_were_foliar)                          CALL plant_pesticide_washoff 
   if (some_applications_were_foliar .and. harvest_day )       CALL plant_pesticide_harvest_application    
   IF (some_applications_were_foliar )                         CALL plant_pesticide_degradation

   
   
   
   
   do K=1, NCHEM   !******Pesticide Simulation ********************        

       !After application, get a new pore water concentration for beginning of time step: old_conc to be fed to tridiagonal
       do concurrent (I=1:NCOM2)
          old_conc(i) = conc_total_per_water(k,i)*theta_zero(i)/ &
                        (theta_zero(i)+kd_new(k,i)*bulkdensity(i)+ thair_old(i)*old_henry(k,i))  
          
          S2_old(i) =sorbed2(k,i)  !create a local array to help things run smoothly 
       end do
       
       ! ****** HENRY LAW COEFFICIENT CALCULATION Temperature Adjustments**************   
       IF (is_temperature_simulated) THEN   
           !Now calculate a new Henry's Constant
           CALL Henry_Temp_Correction (soil_temp, HENRYK(K),ENPY(K), henry_ref_temp, NCOM2,OLDKH)  !move to volatilization PLEASE
           new_henry(K,:) = OLDKH
       ENDIF
       
       call volatilization_setup(k)  !calculates boundary layer conductance and dair

       !Added these variables to explore Freundlich nonlinearity by different apporoximation methods and subdaily time steps
       
       !locally define these for sub daily
       delta_watercontent = (theta_end - theta_zero)/number_subdelt
       theta_old_subday = theta_zero
       theta_air_old_subday = THAIR_old
       delta_aircontent = (thair_new- thair_old)/number_subdelt
                    
       do j = 1, number_subdelt  !********  Start sub daily time step here
            
          !This creeps water & air contents through the subdaily steps*****         
           
          theta_new_subday = theta_old_subday + delta_watercontent              
          theta_air_new_subday = theta_air_old_subday + delta_aircontent      
          
          Kd_old(K, :) = kd_new(K,:)   !Store Old Kd for use in F coefficients of tridiagonal
          
         !***************************************************************************   
         !******************** Nonequilibrium ***************************************     
         !***************************************************************************   
         if (nonequilibrium_flag) then
            ! 1.  Use analytical solution to transfer mass between mobile region and Noneq region. Use current Kd_new.
            !     Get new pre-predicted mobile concentration.
               
            call nonequilibrium(subdelt,k, old_conc , S2_old, theta_old_subday , theta_air_old_subday, conc1_neq, S2_neq )
            !This opeartion is done outside the P-C routine using explicit Kd values. Possible could ibnclude in P-C by 
            !calculating an average Kd during the time step but is it worthwhile?   
            !use conc output from noneq routine (conc1_neq) as input for mobile region conc
 
            !conc1_neq and Sorbed2 is the output
   
            S2_old   = S2_neq       !update the sorbed 2 region and we are done with it for this time step
            old_conc = conc1_neq    !update the beginning day concentration to reflect post-nonequilibrium step
         end if    
 
          !***************************************************************************                              
          !************************** Predictor **************************************
          !***************************************************************************
         if (FLAG1) then                  !Freundlich Flag: only need P/C for nonlinear isotherms
           !calculate a "predictor" concentration for calculating predictor Kd:
           !Split operator : Mass transfer to Region 2 is solved independently of Advection Dispersion operation.
           
           !  2. With mobile new concentration (conc1_neq) a& current Kd, take time step and get a predicted mobile concentration.
           call PRZM5_tridiagonal(subdelt, K, theta_new_subday, theta_old_subday,&
                                  theta_air_new_subday, theta_air_old_subday, old_conc, predicted_conc)  
            
           !  3. With predicted future concentration, calculate a future Kd to be used in the corrector step. No concentrations are 
           !     saved during the precictor step.
           Call Freundlich(k, predicted_conc)           !Puts a predicted New Kd into parameter module
         end if 
                     
          !**************************************************************************** 
          !************************  Corrector  ***************************************
          !**************************************************************************** 
          !  4. With predicted Kd the conc1_neq concentration, now calculate                
          !      the Corrected concentration for future mobile region (new_conc)

         
         
         
          call PRZM5_tridiagonal(subdelt, K, theta_new_subday, theta_old_subday,theta_air_new_subday, &
                                 theta_air_old_subday, old_conc, new_conc) 
          
          
                    
                    
          old_conc = new_conc  !update old conc and continue loop     
          
          theta_old_subday = theta_new_subday        !store these for use in tridiagonal
          theta_air_old_subday = theta_air_new_subday
                
       end do    !*******end sub daily here  
       
       !Store new total mass
       do concurrent (i=1:NCOM2)
          conc_total_per_water(K,I)=new_conc(i)*(theta_end(i)+kd_new(K,i)*bulkdensity(i) + thair_new(i)*new_henry(K,i))/theta_end(i)
          
          Sorbed2(k,i)  = S2_neq(i)
          mass_in_compartment(K,I) = new_conc(i) * (theta_end(i) + kd_new(K,i)*bulkdensity(i) + thair_new(i)*new_henry(K,i))*delx(i)
          mass_in_compartment2(K,I) = Sorbed2(k,i)*bulkdensity(i)*delx(i)                        
       end do
                   
       !*****END PREDICTOR CORRECTOR ********************************************************************************************

       
       
       call flux_calculations(K, new_conc)   ! Fluxes should really be a summation of the subdaily--fix this
       conc_porewater(k,:) =  new_conc       ! rename concentration to something of more sense

       !Store new values for start values of next run
       old_Henry(K,:) = new_henry(K,:) !First Store Previous Run's Henry's Constant
   end do
   

   

   CALL write_outputfile 
   
   
                           
   END Subroutine PRZM
   !***************************************************************************
   
   

   !********************************************************************************************* 
   SUBROUTINE Q10DK
      !converts degradation rate by Q10
      use  constants_and_Variables, ONLY:  soil_temp, &
                                           dwrate_atRefTemp, dsrate_atRefTemp, dgrate_atRefTemp, & 
                                           dwrate,dsrate,dgrate, &
                                           Q_10,TBASE,nchem
      implicit none
       integer :: k

      do k=1,NCHEM   
           dwrate(k,:) = dwrate_atRefTemp(k,:)*Q_10**((soil_temp-TBASE)/10.)
           dsrate(k,:) = dsrate_atRefTemp(k,:)*Q_10**((soil_temp-TBASE)/10.)
           dgrate(k,:) = dgrate_atRefTemp(k,:)*Q_10**((soil_temp-TBASE)/10.)         
       end do  
   
   END SUBROUTINE Q10DK
    
end module PRZM_CORE
   
