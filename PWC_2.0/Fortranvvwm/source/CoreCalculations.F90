Module coreCalculations
implicit none

contains

!######################################################################################
subroutine MainLoop
    use noninputvariables, ONLY: num_records, A,B,E,F, m1_input,m2_input,m1_store,m2_store,mavg1_store, &
                                 daily_depth,fw1,fw2,       &
                                 aqconc_avg1,aqconc_avg2,   &
                                 aq1_store, aq2_store,fraction_to_benthic
                                  
    use variables, ONLY: area, benthic_depth, porosity, DELT,is_calc_prben

    implicit none
    integer :: day_count

    real:: m1,m2    !daily peak/average mass
    real:: mn1, mn2        !mass at end of time step
    real:: aqconc1, aqconc2  !aqueous concentrations for regions 1 and 2
    real:: new_aqconc1, new_aqconc2  !aqueous concentrations for regions 1 and 2
  
    m1=0.
    m2=0.
    mn1=0.
    mn2=0.

    call reducer2 

    !***** Daily Loop Calculations ************************
    do day_count = 1,num_records    
        if (is_calc_prben) then
            !re-equilibration with incoming sediment and then redistribution
            m1 = (1-   fraction_to_benthic(day_count))*(mn1 + m1_input(day_count))  
            m2 = mn2 + fraction_to_benthic(day_count) *(mn1 + m1_input(day_count))              
        else    
            m1 = mn1 + m1_input(day_count)    !This is the old prben method
            m2 = mn2 + m2_input(day_count)  
        endif
        
        m1_store(day_count)=m1
        m2_store(day_count)=m2
        
        !convert to aqueous concentration
        aqconc1 = m1*fw1(day_count)/daily_depth(day_count)/Area
        aqconc2 = m2*fw2/(benthic_depth*Area*porosity )
        
        !******************************************************
        !store these beginning day aquatic concentrations
        !these variables are only used for delivery to output routines
        aq1_store(day_count)=aqconc1
        aq2_store(day_count)=aqconc2
        !******************************************************
        
        call simuldiff2 (A(day_count),B(day_count),E(day_count),F(day_count), & 
             aqconc1,aqconc2,DELT,new_aqconc1,new_aqconc2,aqconc_avg1(day_count),aqconc_avg2(day_count)) 
        

        !convert back to masses
        mn1 = new_aqconc1/fw1(day_count)*daily_depth(day_count)*Area
        mn2 = new_aqconc2/fw2*benthic_depth*Area*porosity 
        
        mavg1_store(day_count)= aqconc_avg1(day_count)/fw1(day_count)*daily_depth(day_count)*Area   
        
    end do 

end subroutine MainLoop



!######################################################################################
subroutine reducer2
    !THIS SUBROUTINE REDUCES THE MODEL EQUIVALENT PARAMETERS TO
    !THE COEFFICIENTS NECESSARY FOR THE SUBROUTINE SIMULDIFF2
    !VALUES FOR A,B,E,F ARE RETURNED
    
     use nonInputVariables, Only:  gamma_1,gamma_2,OMEGA, theta , &
                                   A,B,E,F  !output    
     implicit none 
    !    CALCULATE CONSTANTS FOR USE IN SIMULDIFF2
    
        A = -gamma_1 - omega*theta
        B = omega * theta
        E = omega
        F = -gamma_2-omega

        
        
end subroutine reducer2


!########################################################################################
    !SUBROUTINE: SIMULDIFF2
    !
    !ANALYTICAL SOLUTION FOR THE TWO SIMULTANEOUS DIFFERNTIAL EQNS:
    !          dm1/dt = Am1 + Bm2 
    !          dm2/dt = Em1 + Fm2
    !WITH INITIAL VALUES m1 AND m2 FOR m1 AND m2
    !mn1 IS OUTPUT VALUE FOR m1 AFTER TIME T
    !mn2 IS OUTPUT VALUE FOR m2 AFTER TIME T
    !mavg1 IS AVERAGE VALUE OF m1 OVER TIME T
    !____________________________________________________________________
subroutine simuldiff2 (A,B,E,F,m1,m2,T_end,mn1,mn2,mavg1,mavg2)
        implicit none
        real,intent(in):: A,B,E,F    !diff eqn coefficients
        real,intent(in):: m1,m2        !initial values for m1 and m2
        real,intent(in):: T_end        !time duration
        
        real,intent(out)::mn1,mn2    !values for m1 and m2 after time T_end
        real,intent(out)::mavg1        !average concentration over T_end
        real,intent(out)::mavg2        !average concentration over T_end
        
        real:: root1,root2,DD,EE,FF,X1,Y1,af,fxa,bxe,dif,bbb,rt1,rt2,exrt1,exrt2,ccc,ddd,gx,hx
        real:: term1,term2,term3,term4

        af=A+F
        fxa=F*A
        bxe=B*E
        dif=4*(fxa-bxe)
        bbb=sqrt(af*af-dif)
        

        root1 = (af+bbb)/2.
        root2 = (af-bbb)/2.
           
        DD = (root1-A)/B
        EE = (root2-A)/B
        FF = EE-DD
        X1 = (EE*m1-m2)/FF
        Y1 = (m2-DD*m1)/FF
        
    !calculate new concentrations for next step
        rt1 = root1*T_end
        rt2 = root2*T_end
        exrt1 = exp(rt1)
        exrt2 = exp(rt2)
        ccc = X1*exrt1
        ddd = Y1*exrt2
        
        mn1 = ccc+ddd
        mn2= DD*ccc+EE*ddd

    !    AVERAGE DAILY CONCENTRATION: 
    !    SET UP FOR DAILY AVERAGE, BUT CAN BE CHANGED BY CHANGING T1 AND T2
        gx=X1/root1
        hx=Y1/root2

  
        
        
        term1 = gx*exrt1                    !term3 = -X1/root1*exp(root1*T1)
        term2 = hx*exrt2                    !term4 = -Y1/root2*exp(root2*T1
        term3 = -gx
        term4 = -hx

        mavg1=(term1+term2+term3+term4)/T_end               !mavg1=(term1+term2+term3+term4)/(T2-T1)
        
        
        !        if (isnan(mavg1)) then
        !    write(*,*)dif, fxa,bxe, f, a, b , e
        !    pause
        !endif
        
        

        
        mavg2=(term1*DD+term2*EE+term3*DD+term4*EE)/T_end  !average compartment 2 concentration
        

        
end subroutine simuldiff2
!########################################################################################





end Module coreCalculations