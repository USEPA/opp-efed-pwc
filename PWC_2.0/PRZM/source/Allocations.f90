module allocations
    implicit none
    
    
    
    contains
    subroutine allocate_soil_compartments
    use constants_and_variables, ONLY: ncom2,soil_depth,wiltpoint_water,fieldcap_water,delx,bulkdensity,clay,sand,orgcarb,theta_zero, &
        theta_end,theta_sat, theta_fc, theta_wp,soil_temp,soilwater, dwrate, dsrate, dgrate, dwrate_atRefTemp,dsrate_atRefTemp,dgrate_atRefTemp,  &
        MolarConvert_aq12, MolarConvert_aq13, MolarConvert_aq23, MolarConvert_s12 ,MolarConvert_s13 , MolarConvert_s23 ,dispersion,thair_old,thair_new,  &
        Kd_new,Kd_old, k_freundlich,  N_freundlich, k_freundlich_2, N_freundlich_2, conc_total_per_water, &
        mass_in_compartment,mass_in_compartment2, &
        conc_porewater,sorbed2,Kd_2, ainf, vel, &
        EvapoTran,GAMMA1,new_henry,old_Henry,runoff_intensity, erosion_intensity,soilap, nchem,  &
        DKFLUX,SRCFLX,PVFLUX,UPFLUX,DFFLUX,ADFLUX, dgair, soil_applied_washoff
    
    
        allocate (soil_depth(ncom2))
        allocate (wiltpoint_water(ncom2))
        allocate (fieldcap_water(ncom2))
        allocate (bulkdensity(ncom2))
        allocate (delx(ncom2))
        allocate (clay(ncom2))
        allocate (sand(ncom2))
        allocate (orgcarb(ncom2))
        allocate (theta_zero(ncom2))
        allocate (theta_end(ncom2))
        allocate (theta_sat(ncom2))
        allocate (theta_fc(ncom2))
        allocate (theta_wp(ncom2))
        allocate (soil_temp(ncom2))
        allocate (soilwater(ncom2))
              
        allocate (dwrate(nchem,ncom2))
        allocate (dsrate(nchem,ncom2)) 
        allocate (dgrate(nchem,ncom2))
        allocate (dwrate_atRefTemp(nchem,ncom2))
        allocate (dsrate_atRefTemp(nchem,ncom2))
        allocate (dgrate_atRefTemp(nchem,ncom2))
        
        allocate (MolarConvert_aq12(ncom2))
        allocate (MolarConvert_aq13(ncom2))
        allocate (MolarConvert_aq23(ncom2))
        allocate (MolarConvert_s12 (ncom2))
        allocate (MolarConvert_s13 (ncom2))
        allocate (MolarConvert_s23 (ncom2))
        
        allocate (Kd_new(nchem,ncom2))
        allocate (Kd_old(nchem,ncom2))
        
        allocate (k_freundlich(nchem,ncom2))
        allocate (N_freundlich(nchem,ncom2))
        
        allocate (k_freundlich_2(nchem,ncom2))
        allocate (N_freundlich_2(nchem,ncom2))        
              
        allocate (conc_porewater      (nchem,ncom2))
        allocate (conc_total_per_water(nchem,ncom2))
        allocate (mass_in_compartment (nchem,ncom2))
        allocate (mass_in_compartment2(nchem,ncom2))   
         
        allocate (Sorbed2(nchem,ncom2))
        allocate (Kd_2   (nchem,ncom2))      !Effective linearized Kd for nonequiliobrium compartment

        allocate (GAMMA1 (nchem,ncom2))  
  
        allocate (dispersion(ncom2))
       
        allocate (thair_old(ncom2))
        allocate (thair_new(ncom2))
       
        allocate (DGAIR(ncom2))
             
        allocate (ainf(ncom2+1))  ! need extra compartment to hold final leaching amount (ainf is leaching INTO compartment)
        allocate (vel(ncom2))
        allocate (EvapoTran(ncom2)) 
       
        allocate (new_henry(nchem,ncom2))  
        allocate (old_Henry(nchem,ncom2))  
         
        allocate (runoff_intensity(ncom2))
        allocate (erosion_intensity(ncom2))
           
        allocate (soilap(nchem,ncom2)) 
        
        allocate (DKFLUX(nchem,ncom2)) 
       
        allocate (PVFLUX(nchem,ncom2)) 
        allocate (UPFLUX(nchem,ncom2)) 
        allocate (DFFLUX(nchem,ncom2)) 
        allocate (ADFLUX(nchem,ncom2)) 
        allocate (SRCFLX(3,ncom2))    !this one needs to be 3 becuz parent can produces 2 degradates   
         
        allocate (soil_applied_washoff(nchem,ncom2))   !plant washoff pesticide 
        
    end subroutine allocate_soil_compartments
    
    
    
    
end module allocations