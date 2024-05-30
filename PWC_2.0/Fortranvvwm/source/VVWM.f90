module VVWM_module
implicit none   
contains   
!***************************************************************************
! THIS IS THE ANALYTICAL SOLUTION TO THE EFED SUBSETS OF THE EXAMS MODEL. 
!
!
! Attempt was made to put all EXAMS partameters in CAPITAL LETTERS
! errors are reported in fort.11
!___________________________________________________________________________
subroutine VVWM
use variables, ONLY: simtypeflag, nchem,koc_all, hydro_all, photo_all, RFLAT_all, &
                    aer_aq_all,anae_aq_all,temp_ref_aer_all,temp_ref_anae_all, MWT_all,SOL_all,VAPR_all, &
                   DELT, simtypeflag,waterbodytext, &
                   is_hed_files_made,is_add_return_frequency,additional_return_frequency, & 
                   outputfile_parent_esa,outputfile_parent_analysis,outputfile_parent_daily,outputfile_parent_deem,&
                   outputfile_parent_calendex,outputfile_deg1_analysis,outputfile_deg1_daily, &
                   outputfile_deg1_deem,outputfile_deg1_calendex,outputfile_deg1_esa,& 
                   outputfile_deg2_analysis,outputfile_deg2_daily, &
                   outputfile_deg2_deem,outputfile_deg2_calendex,outputfile_deg2_esa, latitude, Text_latitude, &
                   is_minimum_batch_output,Batch_outputfile



use degradation
use solute_capacity
use mass_transfer
use volumeAndwashout
use MassInputs
use ProcessMetfiles
use outputprocessing
use nonInputVariables, only:k_flow  
use ALLOCATIONmodule
use coreCalculations

implicit none              

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logical :: is_new_metfile


integer :: eof_met            !flag to idicate end of met file error
integer :: ierror            !met file open error flag
integer :: stat


!**local chemical properties****
integer :: chem_index
real :: koc,hydro,RFLAT,photo,aer_aq,anae_aq,temp_ref_aer,temp_ref_anae


character(len = 10) stringreturn


!*************  Met file Calculations  *****************************************
! This routine counts the records in the metfile and gives a value to the num_records parameter
! in the module nonInputVariables.  Allocations then follow below using num_records.


    call count_met(ierror) !count met file records for array allocation
        if (ierror /=0) then
            write (11,*) 'can not find the met file'
            return
        end if

  
    call ALLOCATION

    
    !routine to identify whether the weather file is new type or old type
    is_new_metfile = new_weatherfile()
    
    
    if (is_new_metfile) then
         !New metfile format
        call read_new_weatherfile(eof_met)        
        read(Text_latitude,*, IOSTAT=stat) latitude
        if (stat /=0) then
            write(11,*) "Latitude is not in the correct format"
            return
        end if
        
    else
        !old metfile routine
         call read_metfile(eof_met) 
         call get_latitude (latitude)       
    end if
    
    

    
    
    if (eof_met /=0) then
            write (11,*) 'error: met file is messed up'
            return
    end if
    

    call readzts

    call spraydrift
    

    
    !****************************************************************
    !Washout and volume calculations for individual cases
    

    select case (simtypeflag)
        case (3,5) !reservoir constant volume,flow
                call constant_volume_calc 
        case (2,4)  !pond constant volume, no flow
                call constant_volume_calc 
                k_flow=0.  !for this case zero out washout
        case (1) !variable volume, flow
                call volume_calc
    end select
        

               
  
        
    do chem_index= 1, nchem


        koc           = koc_all(chem_index) 
        hydro         = hydro_all(chem_index) 
        RFLAT         = RFLAT_all(chem_index)
        photo         = photo_all(chem_index)
        aer_aq        = aer_aq_all(chem_index)
        anae_aq       = anae_aq_all(chem_index)
        temp_ref_aer  = temp_ref_aer_all(chem_index)
        temp_ref_anae = temp_ref_anae_all(chem_index)  
      
      !*******************************************

        
        
        call solute_holding_capacity(koc)    
        call omega_mass_xfer
        call hydrolysis(hydro) 
        
        call photolysis(RFLAT,photo,latitude)
        
        
        call metabolism(aer_aq, anae_aq, temp_ref_aer ,temp_ref_anae)     
        call burial_calc(koc)
        

        call volatilization(chem_index )
            
          !********************************************
          !process the individual degradation rates into overall parameters:
        call gamma_one
        call gamma_two
          !**************************************************************
        
        call initial_conditions(chem_index)
       
        call MainLoop
       
        
        select case  (simtypeflag)
        case (3)  
            waterbodytext = "Reservoir"
        case (2)
            waterbodytext = "Pond"
        case (1,4,5)
            waterbodytext =  "Custom"
        end select
        
        
        
if (is_minimum_batch_output) then
       open(unit=44,FILE= trim(Batch_outputfile),Access = 'append',Status='unknown')  
else
       
              
   select case (chem_index)
   case (1)
      open (UNIT=11, FILE= trim(outputfile_parent_analysis), STATUS ='unknown')
      open (UNIT=12,FILE= trim(outputfile_parent_daily),  STATUS='unknown')
      
      
       if (SimTypeFlag /=2 .and. is_hed_files_made  ) then  !No need for Calendex and DEEM for Pond
          open (UNIT=22,FILE=trim(outputfile_parent_deem), STATUS='unknown')
          open (UNIT=23,FILE=trim(outputfile_parent_calendex), STATUS='unknown')
       end if
           
       if (is_add_return_frequency) then  !make additional return files
           write(stringreturn, '(I3)')  int(additional_return_frequency)
            open (UNIT=33, FILE=outputfile_parent_esa, STATUS ='unknown')
       end if

   case (2)  
      open (UNIT = 11, FILE = trim(outputfile_deg1_analysis), STATUS = 'unknown')
      open (UNIT = 12, FILE=  trim(outputfile_deg1_daily), STATUS = 'unknown')
      
      if (SimTypeFlag /=2 .and. is_hed_files_made ) then  !No need for Calendex and DEEM for Pond
          open (UNIT= 22, FILE= trim(outputfile_deg1_deem),  STATUS = 'unknown')
          open (UNIT= 23, FILE= trim(outputfile_deg1_calendex), STATUS = 'unknown')
      end if
      
      if (is_add_return_frequency) then  !make additional return files
          
           write(stringreturn, '(I3)')  int(additional_return_frequency)
            open (UNIT=33, FILE=trim( outputfile_deg1_esa), STATUS ='unknown')
      end if

   case (3)
      open (UNIT = 11, FILE = trim(outputfile_deg2_analysis), STATUS = 'unknown')
      open (UNIT= 12,FILE = trim(outputfile_deg2_daily),  STATUS = 'unknown')
      
      if (SimTypeFlag /=2 .and. is_hed_files_made ) then  !No need for Calendex and DEEM for Pond
          open (UNIT= 22,FILE = trim(outputfile_deg2_deem),  STATUS = 'unknown')      
          open (UNIT= 23,FILE = trim(outputfile_deg2_calendex), STATUS = 'unknown')   
      end if
      
     if (is_add_return_frequency) then  !make additional return files
           write(stringreturn, '(I3)') int(additional_return_frequency)
            open (UNIT=33, FILE=trim(outputfile_deg2_esa),   STATUS ='unknown')
     end if  
      
  
   end select 
   
   
end if
   

        if (nchem > chem_index) then     
              call DegradateProduction(chem_index) 
        end if
        
        call output_processor(chem_index)

      !  call output_processor_PEAK(chem_index)
        
        call WriteInputs(aer_aq,temp_ref_aer,anae_aq ,temp_ref_anae,photo,rflat,hydro,koc, &
                          MWT_all(chem_index),VAPR_all(chem_index),sol_ALL(CHEM_INDEX))
        close (11) 
    !**********************************************************
  end do


end subroutine VVWM
end module VVWM_module