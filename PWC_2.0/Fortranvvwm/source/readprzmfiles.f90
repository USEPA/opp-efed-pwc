module readprzmfiles

contains

    subroutine read_przm_files
    use utilities
    use nonInputVariables, Only: num_records,     &
                                       mass,      &   !OUTPUT array to hold runoff & erosion & spraydrift events
                                       q,         &   !OUTPUT (m3/sec) array of daily flow
                                       burial,    &   !OUTPUT burial rate each day kg/s 
                                       num_years, &   !INPUT
                                       first_app_date !INPUT
                                       
    use variables, ONLY: area,    & !area of water body
                         afield  !field area (m2)

                         
                         
    !****************************************************************
    !This subroutine reads the PRZM output files.
    !It first searches for the beginining file--the one with the lowest year
    !It then reads in the file and puts pesticide spray drift and runoff mass into first
    !column of the array mass.
    !It returns all masses for the simulation in the array mass
    !It puts erosion pesticide mass into second column of 'mass'
    !It outputs daily runoff in m3/s in "q"
    !only reads to year 99 maximum
    !****************************************************************
    implicit none
	
    integer :: first_year			!return the first year of the simulation

    integer :: count
    integer :: year					!year of current simulation (2 digits)

    integer :: flag_read			!signifies that a file has been read
    integer :: prior_days			!total number of days prior to begining of current year in simulation
    real(8):: applied
    real(8):: percent, dummy
    real(8):: efx					!erosion flux read from PRZM output (g/cm2)/day)
    real(8):: rfx					!runoff flux read from PRZM output (g/cm2)/day)
    real(8):: er_mass				!erosion mass (kg)
    real(8):: rf_mass				!runoff mass (kg)
    real(8):: spraymass				!individual spraydrift mass (kg)
    real(8):: runoff_cm
    real(8):: erosion_tonnes        !
    real(8):: absolute_day			!total number of days from start of simulation
    integer:: j 
    integer:: ierror,ieof			!file read errors
    integer:: napp					!number of applications
    integer:: day_number			!number of the day (e.g., jan 1 = 1, feb 1 = 32)
    integer:: app_day				!day of application
    integer:: app_mon				!month of application
    integer:: mon, day				!month and day of erosion/ runoff event
    integer:: leap					!leap year flag
    integer:: first_year_flag
    character (len=12) :: date
    character (len=15) :: filename

    first_year_flag=0
    prior_days=0
    year=0
    q=0.					!zero runoff
    mass=0.					!zero mass array 
    flag_read=0
    filename = 'p2e-c1.dxx'
    count = 0
    Burial = 0.


    do
	    write (date,*) year				!turn 'year' to charater format
	    date= trim(adjustl(date))
	    filename(9:10)=date				!creates filename string (e.g., 'p2e-c1.d48' )
	    open (UNIT=12,FILE=filename,STATUS='old',ACTION='read',IOSTAT=ierror)


	    !********************************************************************************
	    !if no files have been previously read, advance the year and check for another file
	    !this section is only implemented on first start up

	    
	    if (ierror /=0 .AND. flag_read==0) then    
		    year=year+1
			    if (year > 99) then   !condition to stop program if no input files are present
				    write (11,*) 'error: no input file'
				    return
			    end if
			    cycle  !return to top of loop and search for another file name
	    endif

	    !***************************************
	    !Exit point of subroutine for successfull run
	    !***************************************
	    !if a file has been previously opened (flag_read=1) then ierror=0 signifies
	    ! that there are no more files to be read so exit subroutine with
	    if (ierror /=0 .AND. flag_read==1) then

		    q=q*afield/8640000.	!m3/s :(cm/day) *(m2)* (m/100cm)* (day/86400s)
		    
		    Burial = Burial*afield/864000.  !tonnes/day/ha  (ha/10000 m2)*(1000 kg/tonne)*(day/86400 sec)
		    
		    return				!prior_days at this point will be total days in simulation
	    end if
	    !**************************************

	    count = count+1
    	
	    ! if file exists then proceed 	
	    flag_read =1					!flag to signify that at least first year has been read

	    if (first_year_flag==0)then		!record the first year of the simulation
		    first_year_flag=1
		      
		    first_year =year  !First_year is not used, must be an artifact
	    end if

	    read (12,'(25/)')				!skip over text
	    read (12,*) dummy, dummy, NAPP

	    !%%%%%%%%%%%%%%%% Process  spray drift info  %%%%%%%%%%%%%%%%%%%
	    do j=1, NAPP
		    read(12,*)  app_mon,app_day, applied, dummy, percent
		    call calc_daynumber(app_mon, app_day, year, day_number)
		    absolute_day = prior_days+day_number
			    if (j==1) then
				    first_app_date(count) = absolute_day
			    end if
		    spraymass= applied*percent/100.	*(area/10000.)	!kg	
		    mass (absolute_day,1) = spraymass
	    end do
    	 	 
	    !%%%%%%%%%%%%%%%% Process list of runoff events %%%%%%%%%%%%%%%%
	    do
		    read (12,*, IOSTAT=IEOF) mon,day,runoff_cm,rfx, erosion_tonnes,efx
		    if(ieof /= 0) exit 	
		    call calc_daynumber(mon, day, year, day_number)	
		    absolute_day = prior_days + day_number
		    rf_mass = rfx*AFIELD*10.  !converts to kg  !because of spray drift cannot do whole array here
		    er_mass = efx*AFIELD*10.  !could do whole array here, but for consistency it is left as is
		    mass(absolute_day,1) = mass(absolute_day,1) + rf_mass
		    mass(absolute_day,2) = er_mass
		    q(absolute_day)=runoff_cm    !q is still in cm until the final return (see above)
		    Burial(absolute_day) = erosion_tonnes

		    
	    end do

	    call leapyear(year,leap)
	    prior_days=prior_days+365+leap
	    year=year+1
	    close (UNIT=12)
    end do

    end subroutine read_przm_files



end module readprzmfiles