module MassInputs

contains

    subroutine readzts
    use utilities
    use nonInputVariables, Only: num_records,   &
                                 mass,          &   !OUTPUT array to hold runoff & erosion & spraydrift events
                                 q,             &   !OUTPUT (m3/sec) array of daily flow
                                 burial,        &   !OUTPUT burial rate each day kg/s 
                                 runoff_total,  &
                                 erosion_total,  &
                                 eroded_solids_mass, &
                                 Daily_Avg_Runoff
                                 
                                       
    use variables, ONLY:  afield,   &  !field area (m2)
                         outputfilename,  & !path and family name of przm output
                         nchem, baseflow
                         
    !****************************************************************
    !This subroutine reads the PRZM output file with ZTS subscript
    !It then reads in the file and puts pesticide spray drift and runoff mass into first
    !column of the array mass.
    !It returns all masses for the simulation in the array mass
    !It puts erosion pesticide mass into second column of 'mass'
    !It outputs daily runoff in m3/s in "q"
    !****************************************************************
    implicit none

!    real :: applied
!    real(8) :: percent

!    real(8) :: spraymass                !individual spraydrift mass (kg)
!    real(8) :: runoff_cm
!    real(8):: erosion_tonnes        !
!    real(8):: absolute_day            !total number of days from start of simulation

    integer:: ierror    !file read errors

!    integer:: day_number            !number of the day (e.g., jan 1 = 1, feb 1 = 32)
!    integer:: app_day                !day of application
!    integer:: app_mon                !month of application
!    integer:: day                !month and day of erosion/ runoff event

    integer :: i,j

    character (len=5) :: dummy
    
    q=0.                    !zero runoff
    mass=0.                    !zero mass array 
    burial = 0.
    runoff_total = 0.0
    erosion_total =0.0
    
     open (UNIT=13,FILE=(trim(outputfilename) // ".zts"),STATUS='old',ACTION='read',IOSTAT=ierror)
        
     read (13,*)
     read (13,*)
     read (13,*)
    

    do i=1, num_records 
         read (13,*) dummy,dummy,dummy,q(i),eroded_solids_mass(i),(mass(i,1,j),mass(i,2,j), j=1,nchem)
         !q is in  cm here.
    end do                 
    mass= mass* afield*10.  !converts to kg
    
    eroded_solids_mass= eroded_solids_mass*1000. !convert from tonnes to kg
  
    
    runoff_total(1:nchem) = sum(mass(:,1,1:nchem),1)
    erosion_total(1:nchem)= sum(mass(:,2,1:nchem),1)
  

        q=q*afield/8640000.    !m3/s :(cm/day) *(m2)* (m/100cm)* (day/86400s)  
        
 
        Daily_Avg_Runoff = sum(q)/num_records
        
        q = q + baseflow  !add in baseflow
        
        
        !corrected Feb 1 2015  zts file is tonnes absolute, not per ha
        !Burial = Burial*afield/864000.  !tonnes/day/ha  (ha/10000 m2)*(1000 kg/tonne)*(day/86400 sec)
        
        Burial = eroded_solids_mass/86400.  ! kg/day*(day/86400 sec)    = kg/sec
        
        
        
        
        
        close (UNIT=13)
    end subroutine readzts

    
    
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    subroutine spraydrift
       use variables, only: napp, spray,appdate_sim_ref
       use nonInputVariables, only: mass, num_records,spray_total
                                                                              
       implicit none
       integer ::  i!,j, total_apps,status, counter,test
       
       !these hold the entire simulation of pesticide applications
       !they are indexed by the vector date_app
       !integer, allocatable, dimension(:) :: date_app
       !integer, allocatable, dimension(:) :: actual_date_app
        
       !real(8), allocatable, dimension(:) :: spray_app
       !real(8), allocatable, dimension(:) :: rate_app
       !real(8), allocatable, dimension(:) :: spray_vector
      

      ! total_apps = napp*num_years  
 
       !allocate (date_app(total_apps), STAT=status)  
       !allocate (spray_app(total_apps), STAT=status)
       !allocate (rate_app(total_apps), STAT=status)
   !    allocate (spray_vector(total_apps), STAT=status)
       
       spray_total= 0.0
       
       !years =  (/(i, i=(firstyear+1900),(firstyear+1900 +num_years-1))/)      
       !counter = 0
       !do i=1,num_years 
       !   do j=1,napp
       !       
       !       !test for applications that occur with weather files that do not start Jan 1.
       !       !prevents negative app dates
       !       if (counter == 0) then
       !           test = jd (years(i),appmon(j),appday(j))- startday1900+1
       !           if(test <=0) cycle
       !       end if
       !       
       !     counter = counter+1
       !     date_app(counter) = jd (years(i),appmon(j),appday(j))
       !     
       !     spray_app(counter) = spray(j)
       !     rate_app(counter) = apprate(j)
       !   end do      
       !end do
       
       
       
       do i=1, napp
    
           if (appdate_sim_ref(i) >0 .and. appdate_sim_ref(i) <=num_records) then
               mass(appdate_sim_ref(i), 1,1) =  mass(appdate_sim_ref(i), 1,1) + spray(i)         
               spray_total(1) =  spray_total(1) + spray(i)
           end if
       end do

       
       !allocate (actual_date_app(counter), STAT=status)  
       !allocate (spray_vector(counter), STAT=status)
       !
       !actual_date_app = date_app(1:counter) - startday1900+1 
       !spray_vector = rate_app(1:counter) *(area/10000.)*spray_app(1:counter)
       !
       !mass(actual_date_app, 1,1) =  mass(actual_date_app, 1,1) + spray_vector
       
     !  spray_total(1) = sum(spray_vector)

       !mass(date_app, 1,1) =  mass(date_app, 1,1) + rate_app *(area/10000.)*spray_app
       
    end subroutine spraydrift
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    

!**********************************************************************************************
subroutine DegradateProduction(chem_index)
!when this routine is run, it calculates the production of degradate from parent
!Thus, the "index" refers to the parent chemical
      use nonInputVariables, ONLY: num_records,degradateProduced1,degradateProduced2,  &
                                    volume1, v2    ,k_photo, k_hydro, k_aer_aq,capacity_1,  &
                                    aqconc_avg1,aqconc_avg2,k_anaer_aq,capacity_2 
      
      use variables, ONLY: xPhoto, xHydro, xAerobic,xBenthic,DELT, MWT_all
      implicit none               
      integer,intent(in) :: chem_index

      real :: MWTRatio
      
      MWTRatio = MWT_all(chem_index+1)/MWT_all(chem_index)

      degradateProduced1 = MWTRatio*(xPhoto(chem_index)*k_photo*volume1 + xHydro(chem_index)*k_hydro*volume1 + &
                            xAerobic(chem_index)*k_aer_aq*capacity_1)*aqconc_avg1*DELT
      
      
      degradateProduced2 = MWTRatio*(xHydro(chem_index)*k_hydro*v2 + xBenthic(chem_index)*k_anaer_aq*capacity_2)*aqconc_avg2*DELT 
             
      !Degradate production is delayed one time step to approximate the process and to maintain analytical solution for time step  
      degradateProduced1(2:num_records)= degradateProduced1(1:num_records-1)
      degradateProduced2(2:num_records)= degradateProduced2(1:num_records-1)
      degradateProduced1(1)= 0.
      degradateProduced2(1)= 0.

      
end subroutine DegradateProduction    
    
!######################################################################################
subroutine initial_conditions(chem_index)
       !THIS SUBROUTINE RETURNS VALUES FOR input masses into each compartment 
        use variables, ONLY: PRBEN, is_calc_prben
        
        use nonInputVariables, Only:  mass, & 
                                     m1_input,           & !OUTPUT mass added to littoral region (kg) 
                                     m2_input,           &       !OUTPUT mass added to bethic region (kg)
                                     degradateProduced1, &
                                     degradateProduced2, &
                                     capacity_1,         &
                                     kd_sed_1,           &
                                     eroded_solids_mass, &
                                     fraction_to_benthic
        
                                     !INPUT !mass(:,2)mass coming in by runoff and spraydrift (kg) 
                                     !INPUT mass(:,2) mass coming in on sediment (kg)
        !Local Variables
       
        
        implicit none      
        integer,intent(in) :: chem_index

        !********************************************************************
        !Enter the mass distribution alternative here
        
        ! fraction = ( mass capacity in water and base SS) /(total capacity in water column including on eroded solids)) 

        
        !if (is_calc_prben) then
        !  !   fraction_to_benthic = 1.0-( kd_sed_1* m_sed_1 + v1)/ (capacity_1 + kd_sed_1*eroded_solids_mass)
        !     !changed feb 6 2017
        !     
        !else
        !     
        !end if
        
        
        !do i=1, num_records
        !     write (86,*) fraction_to_benthic(i)
        !end do
        ! 
        !********************************************************************
        
        !CALCULATE INPUT MASSES   
        if (is_calc_prben) then
            fraction_to_benthic = kd_sed_1*eroded_solids_mass/ (capacity_1 + kd_sed_1*eroded_solids_mass)    !used later
            m1_input = mass(:,1,chem_index) +  mass(:,2,chem_index)    !all mass goes to water column initially
            m2_input = 0  

        else 
            fraction_to_benthic = prben
            m1_input = mass(:,1,chem_index) +(1.-fraction_to_benthic)*mass(:,2,chem_index)    
            m2_input = fraction_to_benthic*mass(:,2,chem_index)
                      
        end if

        !******* Add in any degradate mass produced by parent from subsequent parent run******
        if (chem_index>1) then                 !j=1 is the parent.  The following call is for the manual pesticide applications.
          m1_input = m1_input + degradateProduced1   
          m2_input = m2_input + degradateProduced2
        end if
        
end subroutine initial_conditions


end module MassInputs


