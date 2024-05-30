module nonideal_sorption
    implicit none
    
    contains
    
    subroutine freundlich(chem, conc)
       ! Calculates the Effective Kd at aqueous concentration of "conc" based on Freundlich Isotherm Properties (K_..., N_...)
       ! "lowest_conc" prevents numerical bumbling about
      
        use constants_and_Variables, ONLY: lowest_conc, Kd_new, k_freundlich, N_freundlich, k_freundlich_2, N_freundlich_2, KD_2, ncom2
        integer,intent(in) :: chem           !identifier of which chemical propert set to use: Parent Daughter, Granddaughter

        real,intent(in)    :: conc(ncom2) !Aqueous Concentration

        where (conc > lowest_conc)
            Kd_new(chem,:) = k_freundlich(chem,:)  *(conc*1000000.)**(N_freundlich(chem,:)  -1.0)  !conc is in g/cm3 , needs to be converted to mg/L, hence 1000000
            KD_2(chem,:)   = k_freundlich_2(chem,:)*(conc*1000000.)**(N_freundlich_2(chem,:)-1.0)  !conc is in g/cm3 , needs to be converted to mg/L, hence 1000000
        elsewhere
            Kd_new(chem,:) = k_freundlich(chem,:)  *(lowest_conc)**(N_freundlich(chem,:)  -1.0)
            KD_2(chem,:)   = k_freundlich_2(chem,:)*(lowest_conc)**(N_freundlich_2(chem,:)-1.0)
        end where
    end subroutine freundlich
  


    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                    
    subroutine nonequilibrium (time, k, conc_1_in,Sorbed2_in, theta, theta_air, conc1_new, Sorbed2_new )
      use  constants_and_Variables, ONLY: k2, bulkdensity,kd_new, kd_2, old_henry, ncom2
      integer, intent(in) :: k !chemicam number
      real, intent(in)    :: conc_1_in(ncom2)         !mobile aqueous concentration
      real, intent(in)    :: Sorbed2_in(ncom2)     !nonequilibrium sorbed pahse conc (g/g), Sorbed2 in main variable module
      real, intent(in)    :: theta(ncom2)  , theta_air(ncom2) 
      real, intent(in)    :: time
      real, intent(out)   :: Sorbed2_new(ncom2) , Conc1_new(ncom2) 
       
       integer :: i
    real ::  A(ncom2),B(ncom2),E(ncom2),F(ncom2)
    

    A = -k2(k) * bulkdensity *kd_2(k,:)/(theta + theta_air*old_henry(k,:) + bulkdensity*kd_new(k,:))
    B =  k2(k)* bulkdensity/(theta + theta_air*old_henry(k,:) + bulkdensity*kd_new(k,:))
    E =  k2(k) * kd_2(k,:)
    F = -k2(k)
    

    !do i=1,ncom2    
    !  call simuldiff2(A(i),B(i),E(i),F(i),conc_1_in(i),Sorbed2_in(i),time, Conc1_new(i), Sorbed2_new(i))
    !end do


  do concurrent (i=1:ncom2)
      call simuldiff2(A,B,E,F,conc_1_in,Sorbed2_in,time, Conc1_new, Sorbed2_new)
  end do
  
  
  

  
    end subroutine nonequilibrium
    
!########################################################################################

elemental subroutine simuldiff2 (A,B,E,F,m1,m2,T_end,mn1,mn2)   !,mavg1,mavg2)
!subroutine simuldiff2 (A,B,E,F,m1,m2,T_end,mn1,mn2)   !,mavg1,mavg2)

    !____________________________________________________________________
    !ANALYTICAL SOLUTION FOR THE TWO SIMULTANEOUS DIFFERNTIAL EQNS:
    !          dm1/dt = Am1 + Bm2 
    !          dm2/dt = Em1 + Fm2
    !WITH INITIAL VALUES m1 AND m2 FOR m1 AND m2
    !mn1 IS OUTPUT VALUE FOR m1 AFTER TIME T
    !mn2 IS OUTPUT VALUE FOR m2 AFTER TIME T
    !mavg1 IS AVERAGE VALUE OF m1 OVER TIME T
    !____________________________________________________________________

        implicit none
        real,intent(in):: A,B,E,F    !diff eqn coefficients
        real,intent(in):: m1,m2        !initial values for m1 and m2
        real,intent(in):: T_end        !time duration
        
        real,intent(out)::mn1,mn2    !values for m1 and m2 after time T_end
        !real,intent(out)::mavg1        !average concentration over T_end
        !real,intent(out)::mavg2        !average concentration over T_end
        
        real:: root1,root2,DD,EE,FF,X1,Y1,af,fxa,bxe,dif,bbb,rt1,rt2,exrt1,exrt2,ccc,ddd  
!        real:: term1,term2,term3,term4,gx,hx
        
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
        
        if (X1==0.0) ccc=0.0
        if (Y1==0.0) ddd=0.0
         
        mn1 = ccc+ddd
        mn2= DD*ccc+EE*ddd
    
    !!    AVERAGE DAILY CONCENTRATION: 
    !!    SET UP FOR DAILY AVERAGE, BUT CAN BE CHANGED BY CHANGING T1 AND T2
    !    gx=X1/root1
    !    hx=Y1/root2
    !
    !
    !    
    !    
    !    term1 = gx*exrt1                    !term3 = -X1/root1*exp(root1*T1)
    !    term2 = hx*exrt2                    !term4 = -Y1/root2*exp(root2*T1
    !    term3 = -gx
    !    term4 = -hx
    !
    !    mavg1=(term1+term2+term3+term4)/T_end               !mavg1=(term1+term2+term3+term4)/(T2-T1)
    !    
    !    
    !    !        if (isnan(mavg1)) then
    !    !    write(*,*)dif, fxa,bxe, f, a, b , e
    !    !    pause
    !    !endif
    !    
    !    
    !
    !    
    !    mavg2=(term1*DD+term2*EE+term3*DD+term4*EE)/T_end  !average compartment 2 concentration
    !    

        
end subroutine simuldiff2
!########################################################################################

    
    
end module nonideal_sorption
    