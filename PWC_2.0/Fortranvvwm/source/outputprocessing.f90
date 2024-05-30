module outputprocessing

    contains
    

    subroutine output_processor(chem_index)
    use utilities
    use variables
    use noninputvariables, ONLY: num_records,    &
                                 num_years,      &
                                 k_burial,       &
                                 k_aer_aq,       &
                                 k_flow,         &
                                 k_hydro,        &
                                 k_photo,        &
                                 k_volatile,     &
                                 k_anaer_aq,     &
                                 gamma_1,        &
                                 gamma_2,        &
                                 fw1,            &
                                 fw2,            &
                                 aq1_store,      &   !beginning day after app concentration in water column
                                 aq2_store,      &
                                 aqconc_avg1,    &   !average daily concentration (after app)
                                 aqconc_avg2,    &
                                 daily_depth,    &
                                 runoff_total ,  &
                                 erosion_total,  &
                                 spray_total ,   &
                                 startday1900, &
                                 Daily_Avg_Runoff, Daily_avg_flow_out
                                                                              
                                        
    implicit none
    integer, intent(in) :: chem_index
    
    
    !temporary parameters for esa, should make this more generalk in the future
    real:: return_frequency
    integer:: unit_number
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    real:: simulation_average
    
    real :: xxx  !local variable

 !   real(8),dimension(num_records)::c1
 !   real(8),dimension(num_records)::cavgw


    real,dimension(num_records):: c4        !all 4-day averaged values
    real,dimension(num_records):: c21
    real,dimension(num_records):: c60
    real,dimension(num_records):: c90
    real,dimension(num_records):: c365
    real,dimension(num_records):: benthic_c21
    

    real,dimension(num_years):: peak   
    real,dimension(num_years):: benthic_peak
    
    
    real,dimension(num_years):: c1_max  !peak year to year daily average
    
    real,dimension(num_years):: c4_max  !the peak 4-day average within the 365 days after application
    real,dimension(num_years):: c21_max
    real,dimension(num_years):: c60_max
    real, dimension(num_years)::c90_max
    real,dimension(num_years):: c365_max !the peak 365-day average within the 365 days after application
                                            !last year will be short depending on application date
                                        
    real,dimension(num_years):: benthic_c21_max                         
                                        
    integer :: i    
    integer ::date_time(8)
    real :: convert                    !conversion factor kg/m3 

    real :: Total_Mass
    integer :: YEAR,MONTH,DAY
    integer :: eliminate_year
    
    
    ! No longer using first app as the first day
    
    integer,dimension(num_years) ::  first_annual_dates !array of yearly first dates (absolute days).
                                    ! First date is the calendar day of start of simulation 
    first_annual_dates= 0
    
    
    
    
if (is_minimum_batch_output .eqv. .False.) then


    !***** EFED TIME SERIES ********************************************
    write(12,*) "col 1: Daily Depth (m)" 
    write(12,*) "col 2: Daily Average Aqueous Conc. in Water Columm, kg/m3 "
    write(12,*) "col 3: Daily Average Aqueous Conc. in Benthic Zone, kg/m3"
    write(12,*) "col 4: Daily Peak Aqueous Conc. in Water Column, kg/m3"
    
    write(12,*)  startday1900, "= Start Day (number of days since 1900)"
    do i =1, num_records
         write(12,'(G12.4E3, "," ,ES12.4E3, "," ,ES12.4E3, "," ,ES12.4E3)')  daily_depth(i), aqconc_avg1(i), aqconc_avg2(i), aq1_store(i) 
    end do
    
    !******HED TIME SERIES **************************************
    if (SimTypeFlag /=2  .and. is_hed_files_made) then  !No need for Calendex and DEEM for Pond
        !************************  DEEM Input File Header ********************************
        write(22,*) "'DEEM Input File" 
        write(22,'(" ''Performed on: ", i2,"/",i2,"/",i4,2x,"at " ,i2,":",i2) ') &
                       date_time(2),date_time(3),date_time(1),date_time(5),date_time(6)
        write(22,*) "'Scenario: ", trim(scenario_id)," " ,trim( waterbodytext)
        write(22,*) "'List of Daily Average Aqueous Concentration in Water Columm, mg/L "
        write(22,*) "'Start data:"
        
        !************************  CALENDEX & DEEM DATA Input ********************************  
        call get_date (startday1900, YEAR,MONTH,DAY)
        eliminate_year = year
        
        do i =1, num_records
             call get_date (startday1900+i-1, YEAR,MONTH,DAY)
            if (year > eliminate_year) then 
                write(22,'(ES15.4)')  aqconc_avg1(i)*1000.0   !DEEM FILE DATA
                write(23,'(I2.2,"/",I2.2,"/", I4, "," ,ES15.4E3)')  month,day, year, aqconc_avg1(i)*1000.0   !Calendex File Data   
            end if    
        end do
    end if
    !**************************************************************************
end if

    
    !Calculate chronic values *******************
    !The following returns the n-day running averages for each day in simulation
    
    
    call window_average(aqconc_avg1,4,num_records,c4)
    call window_average(aqconc_avg1,21,num_records,c21)
    call window_average(aqconc_avg1,60,num_records,c60)
    call window_average(aqconc_avg1,90,num_records,c90)  
    call window_average(aqconc_avg1,365,num_records,c365)
 
    Simulation_average = sum(aqconc_avg1)/num_records
    call window_average(aqconc_avg2,21,num_records,benthic_c21)
    
    call find_first_annual_dates (num_years, first_annual_dates )
    
    call pick_max(num_years,num_records, first_annual_dates,aqconc_avg1,c1_max) !NEW FIND DAILY AVERAGE CONCENTRATION RETURN
    
    
    
    
    call pick_max(num_years,num_records, first_annual_dates,c4,c4_max)
    call pick_max(num_years,num_records, first_annual_dates,c21,c21_max)
    call pick_max(num_years,num_records, first_annual_dates,c60,c60_max)
    call pick_max(num_years,num_records, first_annual_dates,c90,c90_max)

    call pick_max(num_years,num_records, first_annual_dates,benthic_c21,benthic_c21_max)
    
    !treat the 365 day average somewhat differently:
    !In this case, we simply are calculating the average for the 365 day forward from the
    !day of application
    do concurrent (i=1:num_years-1) 
        c365_max(i) = c365(first_annual_dates(i)+365)  
    end do
    c365_max(num_years) = c365(num_records)
    
    !*************************************************

    !****Calculate Acute values *******************
    call pick_max(num_years,num_records, first_annual_dates,aq1_store, peak)
    
    call pick_max(num_years,num_records, first_annual_dates,aq2_store, benthic_peak)
    
    !*****************************************

    convert = 1000000.
    peak               = peak*convert
    
    c1_max             = c1_max*convert
    c4_max             = c4_max*convert
    c21_max            = c21_max*convert
    c60_max            = c60_max*convert
    c90_max            = c90_max*convert
    c365_max           = c365_max*convert
    benthic_peak       = benthic_peak*convert
    benthic_c21_max    = benthic_c21_max*convert
    Simulation_average = Simulation_average*convert
    

       
if (is_minimum_batch_output) then  !append an output file for a reduced output batch run
        return_frequency = 10.0
       unit_number = 44
       call write_simple_batch_data(return_frequency, unit_number,num_years, peak,Simulation_average,c1_max,c4_max,c21_max,c60_max,c90_max,c365_max,benthic_peak, benthic_c21_max )
         
 else
   
    if (is_add_return_frequency) then 
       return_frequency = additional_return_frequency
       unit_number = 33
       call write_returnfrequency_data(return_frequency, unit_number,num_years, peak,Simulation_average,c1_max,c4_max,c21_max,c60_max,c90_max,c365_max,benthic_peak, benthic_c21_max )
    end if
    
    
    
    return_frequency = 10.0
    unit_number = 11
    

    
    call write_returnfrequency_data(return_frequency, unit_number,num_years, peak,Simulation_average,c1_max,c4_max,c21_max,c60_max,c90_max,c365_max,benthic_peak, benthic_c21_max )
    
    
    

    
   write (11,'(A)') '****************************************************************************************************'
   write (11,*) "Effective compartment halflives averaged over simulation duration:"
   write (11,*)
   xxx = sum(k_flow)
   if (xxx > 0.) then
        write (11,*) "washout halflife (days) =           ", 0.69314/(xxx/num_records)/86400
   else
        write (11,*) "zero washout                          0"
   end if
    
   xxx = sum(k_aer_aq)
   if (xxx > 0.) then
        write (11,*) "water col metab halflife (days) =   ", 0.69314/(xxx/num_records)/86400
   else
        write (11,*) "zero metabolic                        0"
   end if
    
    
    xxx = sum(k_hydro*fw1)  
    if (xxx > 0.) then
        write (11,*) "hydrolysis halflife (days)  =       ", 0.69314/(xxx/num_records)/86400
    else
        write (11,*) "zero hydrolysis                       0"
    end if
   
    xxx =  sum(k_photo*fw1)
    if (xxx > 0.) then
        write (11,*) "photolysis halflife (days)  =       ", 0.69314/(xxx/num_records)/86400
    else
        write (11,*) "zero photolysis                       0"
    end if    
    
    
    xxx =  sum(k_volatile*fw1)
    if (xxx > 0.) then
        write (11,*) "volatile halflife (days)  =         ", 0.69314/(xxx/num_records)/86400
    else
        write (11,*) "zero volatility                       0"
    end if      
    
    
    xxx =  sum(gamma_1)
    if (xxx > 0.) then
        write (11,*) "total water col halflife (days) =   ", 0.69314/(xxx/num_records)/86400
    else
        write (11,*) "zero degradation in water column      0"
    end if    
    
        write (11,*)
    
    
    xxx = sum(k_burial)
    if (xxx > 0.) then
        write (11,*) "burial halflife (days)  =           ", 0.69314/(xxx/num_records)/86400
    else
        write (11,*) "zero burial                           0"
    end if
    
    
    xxx = sum(k_anaer_aq)
    if (xxx > 0.) then
        write (11,*) "benthic metab halflife (days) =     ", 0.69314/(xxx/num_records)/86400
    else
        write (11,*) "zero benthic metab                    0"
    end if
    
       xxx = sum(k_hydro*fw2)
    if (xxx > 0.) then
        write (11,*) "benthic hydrolysis halflife (days) =", 0.69314/(xxx/num_records)/86400
    else
        write (11,*) "zero benthic hydrolysis               0"
    end if 
    
    
    xxx = sum(gamma_2)
    if (xxx > 0.) then
        write (11,*) "total benthic halflife (days) =     ", 0.69314/(xxx/num_records)/86400
    else
        write (11,*) "zero benthic total degradation        0"
    end if 
    write (11,*) "***********************************************************************"
    write (11,*)  "Fractional Contribution of Transport Processes to Waterbody & Total Mass (kg):"
    write (11,*)
    
   
    
    Total_Mass = runoff_total(chem_index) + erosion_total(chem_index) +  spray_total(chem_index)  
    
    If (Total_Mass <= 0.0) then
        write(11,'(A18,F10.4, G20.4 )') "Due to Runoff  = ", 0.0,0.0
        write(11,'(A18,F10.4, G20.4 )') "Due to Erosion = ", 0.0,0.0
        write(11,'(A18,F10.4, G20.4 )') "Due to Drift   = ", 0.0,0.0
    else
        write(11,'(A18,F10.4, G20.4E3 )') "Due to Runoff  = ", runoff_total(chem_index) /Total_Mass,runoff_total(chem_index)
        write(11,'(A18,F10.4, G20.4E3 )') "Due to Erosion = ", erosion_total(chem_index) /Total_Mass,erosion_total(chem_index)
        write(11,'(A18,F10.4, G20.4E3 )') "Due to Drift   = ", spray_total(chem_index)/Total_Mass,spray_total(chem_index)
    end if
    
    
    write (11,*) "***********************************************************************"
    write (11,*)  "Flow in/out Characteristics of Waterbody:"
    Write (11,*)  "Average Daily Runoff Into Waterbody (m3/s) = " , Daily_Avg_Runoff
    Write (11,*)  "Baseflow Into Waterbody (m3/s)             = " , baseflow
    Write (11,*)  "Average Daily Flow Out of Waterbody (m3/s) = " , Daily_avg_flow_out
    
    
    
end if 
    
    
    
    end subroutine output_processor


!***********************************************************************************************************    
    subroutine WriteInputs(aer_aq,temp_ref_aer,anae_aq ,temp_ref_anae,photo,rflat,hydro,koc,mwt,vapr,sol)
        use variables
        implicit none
        real, intent(in) :: aer_aq,temp_ref_aer,anae_aq ,temp_ref_anae,photo,rflat,hydro,koc,mwt,vapr,sol


!      open (UNIT = 11, FILE = (trim(outputfilename) // "Output.txt"), STATUS = 'old', POSITION = 'append', IOSTAT = ierror)
               
        write(11,*)  "***********************************************************************"
        write(11,*) "Inputs:"
        write(11,'(G12.4, A)') koc, " = partitioning coefficient"
        write(11,'(G12.4, A)') aer_aq, " = water column half Life" 
        write(11,'(G12.4, A)') temp_ref_aer, " = reference temp for water column degradation"
        write(11,'(G12.4, A)') anae_aq , " = benthic Half Life"
        write(11,'(G12.4, A)') temp_ref_anae, " = Reference temp for benthic degradation"    
        write(11,'(G12.4, A)') QT, " = Q ten value"
        write(11,'(G12.4, A)') photo, " = photolysis half life"
        write(11,'(G12.4, A)') rflat, " = reference latitude for photolysis study"
        write(11,'(G12.4, A)') hydro, " = hydrolysis half life"
        write(11,'(G12.4, A)') mwt, " = molecular wt"
        write(11,'(G12.4, A)') vapr, " = vapor pressure"
        write(11,'(G12.4, A)') sol, " = solubility"
        write(11,'(G12.4, A)') afield, " = field area"
        write(11,'(G12.4, A)') area, " = water body area"
        write(11,'(G12.4, A)') depth_0, " = initial depth"
        write(11,'(G12.4, A)') depth_max, " = maximum depth"
        write(11,'(I4,8X, A)') SimTypeFlag, " 1=vvwm, 2=usepa pond, 3 = usepa reservoir, 4 = const vol no flow, 5 = const vol w/flow " 
        write(11,*)            burialflag, " T = burial, else no burial"
        write(11,'(G12.4, A)') D_over_dx, " = mass transfer coefficient "
        write(11,'(L,1X, A)') is_calc_prben, "PRBEN is calculated"
        write(11,'(G12.4, A)') benthic_depth, " = benthic compartment depth"
        write(11,'(G12.4, A)') porosity, " = benthic porosity"
        write(11,'(G12.4, A)') bulk_density, " =  benthic bulk density"
        write(11,'(G12.4, A)') FROC2, " = OC frcation in benthic sediment"
        write(11,'(G12.4, A)') DOC2, " = DOC in benthic compartment"
        write(11,'(G12.4, A)') BNMAS, " = benthic biomass"
        write(11,'(G12.4, A)') DFAC, " = DFAC"
        write(11,'(G12.4, A)') SUSED, " = SS"
        write(11,'(G12.4, A)') CHL, " = chlorophyll"
        write(11,'(G12.4, A)') FROC1, " = OC frcation in water column SS"
        write(11,'(G12.4, A)') DOC1, " = DOC in water column"
        write(11,'(G12.4, A)') PLMAS, " = biomass in water column"
        
        
        write(11,*) "FRACTION AREA CROPPED = ", cropped_fraction
    end subroutine WriteInputs
 !***********************************************************************************************************    


!***************************************************************
subroutine pick_max (num_years,num_records,bounds,c, output)
!    !this subroutine choses the maximum values of subsets of the vector c
!    !the subsets are defined by the vector "bounds"
!    !maximum values of "c" are chosen from within the c indices defined by "bounds"
!    !output is delivered in the vector "output"
    implicit none
    integer, intent(in) :: num_records
    integer, intent(in) :: num_years
    integer, intent(in) :: bounds(num_years)
    real, intent(in), dimension(num_records) :: c
    real, intent(out),dimension(num_years) :: output

    integer :: i

    !forall (i = 1: num_years-1) output(i) = maxval( c(bounds(i):bounds(i+1)-1) ) changed 2/5/2020
    
    
    
    do concurrent(i = 1: num_years-1)
        output(i) = maxval( c(bounds(i):bounds(i+1)-1) )
    end do
    
    output(num_years)= maxval( c(bounds(num_years):num_records) )

    
    
end subroutine pick_max

!***************************************************************
subroutine Return_Frequency_Value(returnfrequency, c_in, n, c_out, lowYearFlag)
    !CALCULATES THE Concentration at the given yearly return frequency
    implicit none
    
    real,intent(in) :: returnfrequency             !Example 1 in 10 years would be 10.0
    integer,intent(in) :: n                        !number of items in list
    real, intent(in), dimension(n):: c_in          !list of items
    
    real,intent(out):: c_out                       !output of 90th centile of peaks
    real:: f,DEC      
    integer:: m    
    real,dimension(n):: c_sorted
    logical, intent(out) :: LowYearFlag  !if n is less than 10, returns max value and LowYearFlag =1
    LowYearFlag = .false.

    call hpsort(n,c_sorted, c_in)  !returns a sorted array
    

    f = (1.0 -1.0/returnfrequency )*(n+1)
    m=int(f)
    DEC = f-m      
    
   if (n < returnfrequency)then
      c_out = c_sorted(n)
      LowYearFlag = .true.
   else 
    c_out = c_sorted(m)+DEC*(c_sorted(m+1)-c_sorted(m))
   end if

end subroutine Return_Frequency_Value
!***************************************************************



!****************************************************************
subroutine hpsort(n,ra,b)
!  from numerical recipes  (should be upgraded to new f90 routine)
    implicit none
    integer,intent(in):: n
    real,intent(out),dimension(n)::ra !ordered output array
    real,intent(in),dimension(n):: b  !original unordered input array

    integer i,ir,j,l
    real rra
    
    ra=b    ! this added to conserve original order

    if (n.lt.2) return

    l=n/2+1
    ir=n
10    continue
    if(l.gt.1)then 
    l=l-1
    rra=ra(l)
    else 
    rra=ra(ir)    
    ra(ir)=ra(1)
    ir=ir-1
    if(ir.eq.1)then 
    ra(1)=rra 
    return
    endif
    endif
    i=l 
    j=l+l
20    if(j.le.ir)then 
        if(j.lt.ir)then
            if(ra(j).lt.ra(j+1))j=j+1 
        endif
        if(rra.lt.ra(j))then 
            ra(i)=ra(j)
            i=j
            j=j+j
        else 
            j=ir+1
        endif
        goto 20
        endif
    ra(i)=rra
    goto 10
end subroutine hpsort
!***********************************************************




!*******************************************************************************
subroutine find_first_annual_dates(num_years, first_annual_dates )
   use noninputvariables, only: firstyear, startday1900,firstday, firstmon
   use utilities
   implicit none
   
   integer,intent(in) :: num_years
   integer,intent(out),dimension(num_years) :: first_annual_dates
   integer i
 
   do concurrent (i = 1:num_years)
      first_annual_dates(i) =  jd(firstyear+(i-1), firstmon,firstday )
   end do
   

   
   
   
   first_annual_dates = first_annual_dates - startday1900+1

   
end subroutine find_first_annual_dates
!********************************************************************************





!***********************************************************

!subroutine find_first_app_dates(first_app_date)
!   use noninputvariables, only: firstyear, num_years, first_app_date, startday1900, first_app_day , first_app_mon
!   use utilities
!   implicit none
!   integer i
!   
!
!   integer, intent(out), dimension(num_years) :: first_app_date
!
!   
!   do concurrent (i = 1:num_years)
!      first_app_date(i) =  jd(firstyear+1900+(i-1), first_app_mon,first_app_day )
!   end do
!   first_app_date = first_app_date-startday1900+1
! 
!   write(*,*) first_app_day , first_app_mon
!   pause
!   
!end subroutine find_first_app_dates




 subroutine write_returnfrequency_data(return_frequency, unit_number,num_years, peak,Simulation_average,c1_max,c4_max,c21_max,c60_max,c90_max,c365_max,benthic_peak, benthic_c21_max   )
   use variables, Only : version_number
   use noninputvariables, ONLY:  Sediment_conversion_factor,fw2 
   implicit none
  
     real, intent(in)                         :: return_frequency
     integer, intent(in)                       :: unit_number
     integer, intent(in)                       :: num_years
     real   , intent(in), dimension(num_years) :: peak,c1_max,c4_max,c21_max,c60_max,c90_max,c365_max,benthic_peak, benthic_c21_max
     real, intent(in)                          :: Simulation_average
     
     
     real :: peak_out,c1_out, c4_out,c21_out,c60_out,c90_out,c365_out,benthic_peak_out,benthic_c21_out
     logical :: lowyearflag
     integer :: date_time(8)
     integer :: i
     character(len=10) :: frequencystring 
     
     
 
  
    !**find values corresponding to  percentiles
    call Return_Frequency_Value(return_frequency, peak,           num_years, peak_out,         lowyearflag)
    
    call Return_Frequency_Value(return_frequency, c1_max,         num_years, c1_out,           lowyearflag)
    
    call Return_Frequency_Value(return_frequency, c4_max,         num_years, c4_out,           lowyearflag)
    call Return_Frequency_Value(return_frequency, c21_max,        num_years, c21_out,          lowyearflag)
    call Return_Frequency_Value(return_frequency, c60_max,        num_years, c60_out,          lowyearflag)
    call Return_Frequency_Value(return_frequency, c90_max,        num_years, c90_out,          lowyearflag)
    call Return_Frequency_Value(return_frequency, c365_max,       num_years, c365_out,         lowyearflag)
    call Return_Frequency_Value(return_frequency, benthic_peak,   num_years, benthic_peak_out, lowyearflag)
    call Return_Frequency_Value(return_frequency, benthic_c21_max,num_years, benthic_c21_out,  lowyearflag)
    

    call date_and_time(VALUES = date_time)
    
    Write(unit_number,*) "Variable Volume Water Model, Version ", version_number
  
    write(unit_number,*) 
    write(unit_number,*)  '*******************************************'
    write(unit_number,'("Performed on: ", i2,"/",i2,"/",i4,2x,"at " ,i2,":",i2) ') &
                   date_time(2),date_time(3),date_time(1),date_time(5),date_time(6)
  
    
    
  write(frequencystring, '(F4.1)')  return_frequency  !internal write in order to right justify string
  

  
  if (LowYearFlag)then
      Write(unit_number,  '(''* Insufficient years to calculate 1-in-'', A5, ''. Only maximums are reported here.'') '   ) adjustl(frequencystring)
  else 
       write(unit_number,*)
  end if

 
  
  write(unit_number, '(''Initial 1-in-'', A5 ,'' = '', G15.4E3,'' ppb'')'  )    adjustl(frequencystring), peak_out
  write(unit_number, '(''Chronic 1-in-'', A5 ,'' = '', G15.4E3,'' ppb'')'  )    adjustl(frequencystring),c365_out
  write(unit_number, '(''Simulation Avg     = '', G15.4E3,'' ppb'')'  )         simulation_average
  write(unit_number, '(''4-d avg 1-in-'', A5 ,'' = '', G15.4E3,'' ppb'')'  )  adjustl(frequencystring),c4_out
  write(unit_number, '(''21-d avg 1-in-'', A5 ,''= '', G15.4E3,'' ppb'')'  )  adjustl(frequencystring), c21_out
  write(unit_number, '(''60-d avg 1-in-'', A5 ,''= '', G15.4E3,'' ppb'')'  )  adjustl(frequencystring), c60_out
  write(unit_number, '(''90-d avg 1-in-'', A5 ,''= '', G15.4E3,'' ppb'')'  )  adjustl(frequencystring),c90_out
  
  write(unit_number, '(''1-d avg 1-in-'', A5 ,'' = '', G15.4E3,'' ppb'')'  )  adjustl(frequencystring),c1_out
    
  write(unit_number, '(''Benthic Pore Water 1-d  avg 1-in-'', A5 ,''= '', G15.4E3,'' ppb'')'  ) adjustl(frequencystring),  benthic_peak_out
  write(unit_number, '(''Benthic Pore Water 21-d avg 1-in-'', A5 ,''= '', G15.4E3,'' ppb'')'  ) adjustl(frequencystring),  benthic_c21_out
    
  write(unit_number, '(''Benthic Conversion Factor             = '', G15.4E3,'' -Pore water (ug/L) to (total mass, ug)/(dry sed mass,kg)'')')&
        Sediment_conversion_factor*1000.
  
  write(unit_number, '(''Benthic Mass Fraction in Pore Water   = '', G15.4E3)'  )  fw2
  write (unit_number,*)
  write (unit_number,'(A)') 'YEAR    1-day     4-day      21-day     60-day     90-day   Yearly Avg Benthic Pk  Benthic 21-day'
  do I=1, num_years  
   write(unit_number,'(I3,1x,8ES11.2E3)') i,c1_max(i),c4_max(i),c21_max(i),c60_max(i),c90_max(i),c365_max(i),benthic_peak(i),benthic_c21_max(i)
  end do
  
 
  
     
     
     
     
 end subroutine write_returnfrequency_data
 




subroutine write_simple_batch_data(return_frequency, unit_number,num_years, peak,Simulation_average,c1_max,c4_max,c21_max,c60_max,c90_max,c365_max,benthic_peak, benthic_c21_max   )
   use variables, Only : version_number, run_id
   use noninputvariables, ONLY:  Sediment_conversion_factor 
   implicit none
  
     real, intent(in)                         :: return_frequency
     integer, intent(in)                       :: unit_number
     integer, intent(in)                       :: num_years
     real   , intent(in), dimension(num_years) :: peak,c1_max,c4_max,c21_max,c60_max,c90_max,c365_max,benthic_peak, benthic_c21_max
     real, intent(in)                          :: Simulation_average
     logical :: lowyearflag
     
     real :: peak_out,c1_out, c4_out,c21_out,c60_out,c90_out,c365_out,benthic_peak_out,benthic_c21_out

 

     
     
 
  
    !**find values corresponding to  percentiles
    call Return_Frequency_Value(return_frequency, peak,           num_years, peak_out,         lowyearflag)
    
    call Return_Frequency_Value(return_frequency, c1_max,         num_years, c1_out,           lowyearflag)
    
    !call Return_Frequency_Value(return_frequency, c4_max,         num_years, c4_out,           lowyearflag)
    !call Return_Frequency_Value(return_frequency, c21_max,        num_years, c21_out,          lowyearflag)
    !call Return_Frequency_Value(return_frequency, c60_max,        num_years, c60_out,          lowyearflag)
    !call Return_Frequency_Value(return_frequency, c90_max,        num_years, c90_out,          lowyearflag)
    call Return_Frequency_Value(return_frequency, c365_max,       num_years, c365_out,         lowyearflag)
    !call Return_Frequency_Value(return_frequency, benthic_peak,   num_years, benthic_peak_out, lowyearflag)
    !call Return_Frequency_Value(return_frequency, benthic_c21_max,num_years, benthic_c21_out,  lowyearflag)
    


        write(unit_number,'(A50,1x,3ES11.2E3)') adjustl(run_id), c1_out, c365_out , simulation_average

  
 
  
     
     
     
     
 end subroutine write_simple_batch_data










end module outputprocessing