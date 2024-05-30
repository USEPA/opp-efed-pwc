MODULE ALLOCATIONmodule
IMPLICIT NONE
contains

subroutine allocation
USE noninputvariables
use variables, ONLY: nchem
integer :: status            !array allocation status,  0=success
    
        allocate (wind(num_records), STAT=status)
        allocate (temp_avg(num_records), STAT=status)
        allocate (evap(num_records), STAT=status)
        allocate (precip(num_records), STAT=status)
        allocate (q(num_records), STAT=status)
        allocate (burial(num_records), STAT=status)
        allocate (eroded_solids_mass(num_records), STAT=status)
        allocate (fraction_to_benthic(num_records), STAT=status)
        
        
        allocate (mass(num_records,2,3), STAT=status)
        
        if (nchem > 1) then 
                allocate (degradateProduced1(num_records), STAT=status)
                allocate (degradateProduced2(num_records), STAT=status)    
        end if
     
        allocate (volume1(num_records), STAT=status)
        allocate (daily_depth(num_records), STAT=status)
        allocate (k_flow(num_records), STAT=status)
        allocate (k_burial(num_records), STAT=status)
        allocate (k_aer_aq(num_records), STAT=status)             
        allocate (k_anaer_aq(num_records), STAT=status)         
        allocate (k_aer_s(num_records), STAT=status)             
        allocate (k_anaer_s(num_records), STAT=status)
        allocate (k_volatile(num_records), STAT=status)
        allocate (k_photo(num_records), STAT=status)
        allocate (k_hydro(num_records), STAT=status)
        allocate (gamma_1(num_records), STAT=status)
        allocate (gamma_2(num_records), STAT=status)
        allocate (A(num_records), STAT=status)
        allocate (B(num_records), STAT=status)
        allocate (E(num_records), STAT=status)
        allocate (F(num_records), STAT=status)
        allocate (theta(num_records), STAT=status)
        allocate (capacity_1(num_records), STAT=status)
        allocate (fw1(num_records), STAT=status)
        allocate (m1_input(num_records), STAT=status)
        allocate (m2_input(num_records), STAT=status)
        allocate (m1_store(num_records), STAT=status)
        allocate (m2_store(num_records), STAT=status)
        allocate (mavg1_store(num_records), STAT=status)     
          
        
 !       allocate (v1(num_records), STAT=status)
        allocate (aqconc_avg1(num_records), STAT=status)        
        allocate (aqconc_avg2(num_records), STAT=status)    

        allocate (aq1_store(num_records), STAT=status)        
        allocate (aq2_store(num_records), STAT=status)    

    
        
        
end subroutine allocation



    
    
    
END MODULE ALLOCATIONmodule