module Plant_Pesticide_Processes
implicit none
    contains
    
    

!******************************************************************************************************************************
SUBROUTINE plant_pesticide_degradation
      use  constants_and_Variables, ONLY: plant_pesticide_degrade_rate,plant_volatilization_rate,foliar_formation_ratio_12, &
           foliar_formation_ratio_23,Foliar_degrade_loss,FOLPST,plant_volatilization_rate,Foliar_volatile_loss, &
           NCHEM,delt
        

      implicit none
!
!      !Determines amount of pesticide which disappears from plant surface by first order decay and volatilization.  
!      The variable PLDKRT is a pseudo first order decay rate which may include processes of volatilization, oxidation, photolysis, etc.
!      !also determines pesticide washed off during rainfall events.

      real     ::  ex1, ex2, ex3, r,term1, term2, term3, term4, term50,term6,term7,term8,term90,term100
      real     ::  foliar_pesticide_loss(3), fol_deg(3),  foliar_pest_initial(3)

 
     Foliar_Pest_initial = FOLPST      !Save initial masses to use later
     
     fol_deg =  plant_pesticide_degrade_rate + plant_volatilization_rate  !local rate, need to 

    
     
             !Parent degradation for time step
             ex1 = EXP((-fol_deg(1))*DELT)
             Foliar_Pesticide_Loss(1) =  FOLPST(1) - FOLPST(1)*ex1  !store for output
             Foliar_volatile_loss(1)  =  plant_volatilization_rate(1)/ (fol_deg(1))*Foliar_Pesticide_Loss(1)
             Foliar_degrade_loss(1)   =  Foliar_Pesticide_Loss(1) - Foliar_volatile_loss(1)
             FOLPST(1) = Foliar_Pest_initial(1)*ex1               
                   
             If (nchem == 2 .or. nchem == 3) then  !Daughter
                ex2 = EXP((-fol_deg(2))*DELT)
                If (fol_deg(2)==fol_deg(1)) then
                    r = delt*ex2
                else
                    r =    (ex1-ex2)/(fol_deg(2)-fol_deg(1)) 
                end if
                FOLPST(2) = foliar_pest_initial(1)*foliar_formation_ratio_12*fol_deg(1)*r  + foliar_pest_initial(2)*ex2      
             end if
             
             If (nchem == 3) then !grandaughter

                 
                 ex3 = EXP((-fol_deg(3))*DELT)
                 !*************************************************************************************************
                 !I cant find the limit for cases when degradation rates are equal k1=k2=k3 (but there is one, if someone can find it).  
                 !In the mean tine, I just slightly alter the rates to make them slightly unequal
                 
                 IF (fol_deg(2)==fol_deg(1)) then                     
                     fol_deg(2) = fol_deg(1) * 1.0001   
                     ex2 = EXP((-fol_deg(2))*DELT)
                     r = (ex1-ex2)/(fol_deg(2)-fol_deg(1)) 
                     FOLPST(2) = foliar_pest_initial(1)*foliar_formation_ratio_12*fol_deg(1)*r  + foliar_pest_initial(2)*ex2 
                 end if      
                 
                 IF (fol_deg(3)==fol_deg(1)) then
                     fol_deg(3)=fol_deg(1) * 0.9999
                     ex3 = EXP((-fol_deg(3))*DELT)
                 end if
                 
                 IF (fol_deg(2)==fol_deg(3)) then
                     fol_deg(2)=fol_deg(3) * 1.0001
                     ex2 = EXP((-fol_deg(2))*DELT)
                     r = (ex1-ex2)/(fol_deg(2)-fol_deg(1)) 
                     FOLPST(2) = foliar_pest_initial(1)*foliar_formation_ratio_12*fol_deg(1)*r  + foliar_pest_initial(2)*ex2           
                 end if
                !**********************************************************************************************************
                 
                term1 = fol_deg(1)*fol_deg(2)/(fol_deg(2)-fol_deg(1))
                term2 = foliar_formation_ratio_12*foliar_formation_ratio_23*Foliar_Pest_initial(1)
                term3 = (ex1-ex3)/(fol_deg(3)-fol_deg(1)) 
                term4 = (ex3-ex2)/(fol_deg(3)-fol_deg(2))
                term50 = term1*term2*(term3 + term4)
                term6 = fol_deg(2)/(fol_deg(3)-fol_deg(2))
                term7 = foliar_formation_ratio_23* Foliar_Pest_initial(2)
                term8 = ex2-ex3
                term90 = term6*term7*term8
                term100 = Foliar_Pest_initial(3)*ex3
                
                FOLPST(3) =term50 + term90 + term100
             end if
             
                
END SUBROUTINE plant_pesticide_degradation

    
    
    
    
   
    !*************************************************************************************************************
    !NEW SUBROUTINE TO HANDLE PESTICIDE washoff

    subroutine plant_pesticide_Washoff
      use  constants_and_Variables, ONLY:canopy_flow, plant_washoff_coeff, &     
      theta_zero,delx,theta_sat, FOLPST, soil_applied_washoff, number_washoff_nodes, nchem
        
      implicit none

      !Determines amount of pesticide which disappears from plant surface by first order decay.  The variable PLDKRT
      !is a pseudo first order decay rate which may include processes of volatilization, oxidation, photolysis, etc.
      !also determines pesticide washed off during rainfall events.

      INTEGER:: K
      REAL         incremental_pore_space(number_washoff_nodes)
      REAL         available_pore_space
      INTEGER      I
      real    :: foliar_washoff(3)
     
      
      If (canopy_flow < 10e-6) then  
        soil_applied_washoff = 0.0
        return
      end if
      
           

      
      
      available_pore_space =0.0          
        
     !Pesticide is distributed in proportion to the pore volume of the compartments
     !This is previously undocumented.

     incremental_pore_space = (theta_sat(1:number_washoff_nodes)-theta_zero(1:number_washoff_nodes))*DELX(1:number_washoff_nodes)  !Available pore space
     
     do I=1,number_washoff_nodes   !Add up available pore space to 2cm
        available_pore_space = available_pore_space + incremental_pore_space(I)                         
     end do
     
     soil_applied_washoff = 0.0
     do concurrent (k=1:nchem)
         foliar_washoff(k)   = FOLPST(K)*(1.0 - exp(-(plant_washoff_coeff(K)*canopy_flow)))   !pesticide removed from foliage by washoff
         FOLPST(K) = FOLPST(K)- foliar_washoff(k)
         soil_applied_washoff(K,1:number_washoff_nodes) = (incremental_pore_space/available_pore_space)*foliar_washoff(k) !pesticide applied by washoff
     end do


        
    END SUBROUTINE plant_pesticide_Washoff
    
    
    
    
    
    
    end module Plant_Pesticide_Processes