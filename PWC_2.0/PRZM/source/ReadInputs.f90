 module readinputs
  implicit none
    contains

    
     logical function new_weatherfile()
           use utilities
           use constants_and_variables, ONLY:  weatherfilename
           integer :: start
           character(len=3) :: extension

           
           
           new_weatherfile = .FALSE.
           start = scan(weatherfilename,'.', BACK = .TRUE.)          
           extension = weatherfilename(start+1:start+3)
           call To_uppercase(extension )          
           if (extension == "WEA") new_weatherfile = .TRUE.
           
          return 
    end function new_weatherfile
    
    
    
    
    
    !***********************************************************************************************
      subroutine Read_Weatherfile
      use utilities
      use constants_and_Variables, ONLY:precip, pan_evap,air_temperature, wind_speed, solar_radiation, num_records, &
                                        EchoFileUnit,metfileunit,weatherfilename, startday, first_year
      
      integer :: dummy, status, first_month, first_day,i
      
      

      
      !open weather file, count file, allocate the weather variables, read in dat and place in vectors.
      

      OPEN(Unit = metfileunit,FILE=trim(adjustl(weatherfilename)),STATUS='OLD', IOSTAT=status)
      IF (status .NE. 0) THEN
         write(EchoFileUnit,*)'Problem with met file.'
         stop
      ENDIF   
      
      num_records = 0
      
      !Read first Date of Simulation
      

      read (metfileunit,*, IOSTAT=status) first_month, first_day, first_year

      
      if (status /= 0) then
          write(EchoFileUnit,*) status,  "No data or other problem in Weather File"
          stop
      end if
      
       
      startday = jd(first_year, first_month, first_day)

      !Count the records in the weather file
      num_records=1
      do        
          read (metfileunit,*, IOSTAT=status) dummy
          if (status /= 0)exit
          num_records=num_records+1
      end do
      write (EchoFileUnit,*) "Total days = ",num_records
      

      !Allocate the weather parameters
      
      allocate (precip(num_records))
      allocate (pan_evap(num_records))
      allocate (air_temperature(num_records))
      allocate (wind_speed(num_records))
      allocate (solar_radiation(num_records))
      
      !rewind and read in weather data
      rewind(metfileunit)
      
      do i = 1, num_records
          READ(MetFileUnit,*) dummy,dummy,dummy,precip(i),pan_evap(i),air_temperature(i),wind_speed(i),solar_radiation(i)
 
      end do
  
      close(metfileunit)
      
      end subroutine Read_Weatherfile
    
    !***************************************************************************************************
     subroutine Read_Old_Weatherfile
      use utilities
      use constants_and_Variables, ONLY:precip, pan_evap,air_temperature, wind_speed, solar_radiation, num_records, &
                                        EchoFileUnit,metfileunit,weatherfilename, startday, first_year
      
      integer :: dummy, status, first_month, first_day,i
      
      !open weather file, count file, allocate the weather variables, read in dat and place in vectors.
      



      
      
      OPEN(Unit = metfileunit,FILE=trim(adjustl(weatherfilename)),STATUS='OLD', IOSTAT=status)
      IF (status .NE. 0) THEN
         write(EchoFileUnit,*)'Problem with met file.'
         stop
      ENDIF   
      
      num_records = 0
      
      !Read first Date of Simulation
      read (metfileunit,'(1X,3I2)', IOSTAT=status) first_month, first_day, first_year

      first_year = first_year + 1900
      if (status /= 0) then
          write(EchoFileUnit,*) "No data or other problem in Weather File"
          stop
      end if
      
       
      startday = jd(first_year, first_month, first_day)

      !Count the records in the weather file
      num_records=1
      do        
          read (metfileunit,*, IOSTAT=status) dummy
          if (status /= 0)exit
          num_records=num_records+1
      end do
      write (EchoFileUnit,*) "Total days = ",num_records    

      !Allocate the weather parameters
      
      allocate (precip(num_records))
      allocate (pan_evap(num_records))
      allocate (air_temperature(num_records))
      allocate (wind_speed(num_records))
      allocate (solar_radiation(num_records))
      
      !rewind and read in weather data
      rewind(metfileunit)
      
      do i = 1, num_records
          READ(MetFileUnit,'(1X,3I2,5F10.0)', IOSTAT =status) dummy,dummy,dummy,precip(i),pan_evap(i),air_temperature(i),wind_speed(i),solar_radiation(i)
          if (status /= 0) then
              write (EchoFileUnit,*) "Weather file problem on line ", i
              exit
          end if
          
      end do
     write (EchoFileUnit,*) "finished reading old weather file"
      close(metfileunit)
      
     end subroutine Read_Old_Weatherfile 
     
      
      
!***********************************************************************************************************
   !subroutine GETMET(status)
   !   !Read Met File and gets the Ordinal day (number of day of the year from Jan 1)          
   !   use constants_and_Variables, ONLY: MetFileUnit,EchoFileUnit,air_TEMP,PEVP, precipitation,SOLRAD,WIND,   &
   !                                       current_day,current_month,current_year,julday1900
   !   use utilities
   !   implicit none
   !   integer,intent(out) :: status  !status = -1 is end of file
   !
   !
   !   READ(MetFileUnit,'(1X,3I2,5F10.0)',IOSTAT=status) current_month,current_day,current_year, precipitation, PEVP,air_TEMP,WIND,SOLRAD
   !     !THese are loaded into module:  current_month,current_day,current_year
   !          
   !
   !   julday1900 = jd(current_year,current_month,current_day)  !Set the julian day into module
   !   
   !   if (status >=1) write( EchoFileUnit,*)"Metfile read failure"
   !   
   !end subroutine   GETMET
   ! 
!******************************************************************************************************************************
 
         
   
  
SUBROUTINE PRZMRD_PRZM5
  !Reads and checks input data. 
      
      use  constants_and_Variables, ONLY:PRZMinputUnit, EchoFileUnit,pfac, sfac, &
      soil_temp_input,is_temperature_simulated,  &
      albedo,emmiss,bbt,IRTYPE,PCDEPL,max_irrig,FLEACH, min_evap_depth ,                   &
      AFIELD,IREG,USLEK,USLELS,USLEP,HL,SLP, USLEC,MNGN,theta_zero_input,sand_input,clay_input,fc_input,wp_input,CN_2,     &
      NUSLEC,GDUSLEC,GMUSLEC,GYUSLEC,erflag, IRFLAG, Num_delx,                          &
      DEPI, appeff,Tband_top,plant_pesticide_degrade_rate,         &
      foliar_formation_ratio_12,PTRN13,foliar_formation_ratio_23, &
      MolarConvert_aq12_input, MolarConvert_aq13_input, MolarConvert_aq23_input,  &
      MolarConvert_s12_input,  MolarConvert_s13_input, MolarConvert_s23_input,   &    
      ENPY,num_applications,DAIR,HENRYK,plant_volatilization_rate,oc_input,cam,     &
      Q_10,TBASE,NCHEM,NHORIZ,SOL,thickness,CONST,NPLOTS,chem_id  ,           &
      ARG,IARG2,PLNAME,mode,  &
      emergence_date, maturity_date, harvest_date, application_date,dispersion_input,  &
      runoff_effic,runoff_decline,runoff_extr_depth,erosion_decline,erosion_depth,erosion_effic ,Num_hydro_factors,use_usleyears,  &
      metfileunit,maxFileLength,TimeSeriesUnit,RunFilePath,  &
      FLAG1,nonequilibrium_flag,calibrationflag,CalibrationDataUnit, CalibrationFilename, CalibrationFilenameOutput, &
      UserSpecifiesDepth, user_irrig_depth, k_f_input, N_f_input,lowest_conc, uWind_Reference_Height,Height_stagnant_air_layer_cm, &
      application_rate,number_subdelt,     N_f_2_input,k_f_2_input,K2, bd_input, OC_input,  &
      aq_rate_input, sorb_rate_input, gas_rate_input,ADJUST_CN,&
      some_applications_were_foliar, plant_washoff_coeff,plant_volatilization_rate,weatherfilename, & 
      max_root_depth,max_canopy_cover,max_canopy_holdup,max_canopy_height,  foliar_disposition,num_crop_periods,TAPP ,is_true_rain_distribution, is_TC_lag_method,uptkf, &
      is_adjust_for_rain ,rain_limit, intolerable_rain_window,  optimum_application_window, min_days_between_apps
                                            
      use utilities
      use Output_File
      
     ! Use m_Wind
      implicit none

      INTEGER       dummy

      INTEGER      I,K
      
      integer  :: apy, apm, apd                                  !local application date
      INTEGER      emd, emm, emy, mad, mam, may,had, ham, hay    !local crop dates

      
      CHARACTER(len = 150):: note  !used for sending error messages

      LOGICAL      FATAL
 

      Character(Len = maxFileLength)    :: Filename
      
      ! if CAM == 1,2,3 then force soil incorporation depth == 4 cm
      Real, Parameter :: cam123_soil_depth = 4.0 ! cm
      integer :: status
      
      I   = 80
   !   cam = 0  Not yet allocated delete this line 3/29/17
      sol = 0.0
     MolarConvert_aq12_input = 0.0
     MolarConvert_aq23_input = 0.0
     MolarConvert_aq13_input = 0.0
     MolarConvert_s12_input  = 0.0
     MolarConvert_s23_input  = 0.0
     MolarConvert_s13_input  = 0.0
     
      soil_temp_input = 0.0
      sand_input      = 0.0
      clay_input      = 0.0

      
      OPEN(Unit = PRZMinputUnit,FILE=(trim(adjustl(RunFilePath)) //'PRZM5.inp'),STATUS='OLD', IOSTAT=status)
      IF (status .NE. 0) THEN
          WRITE(EchoFileUnit,*)'Problem with PRZM INP file.'
      ENDIF 
      
      
      write(EchoFileUnit,*) 'Record A1' 
      CALL check_for_comment(PRZMinputUnit)
      READ(PRZMinputUnit,'(A)',IOSTAT = status) weatherfilename !Weather File
      if (status/=0) stop
      
      write(EchoFileUnit,*) "weather file: ", trim(weatherfilename)
      !OPEN(Unit = metfileunit,FILE=trim(adjustl(filename)),STATUS='OLD', IOSTAT=status)
      !IF (status .NE. 0) THEN
      !   note = 'Problem with met file.'
      !   CALL ERRCHK(note,FATAL)
      !ENDIF    
      


       write(EchoFileUnit,*) 'Record A2:  PRZM Output Time Series File'
       CALL check_for_comment(PRZMinputUnit)
       READ(PRZMinputUnit,'(A)',IOSTAT = status) filename 
       
       if (status/=0) stop
       OPEN(Unit=TimeSeriesUnit,FILE=trim(adjustl(filename)), STATUS='UNKNOWN', IOSTAT=status)
       note= 'Time Series:' //  trim(Filename)
       WRITE(EchoFileUnit,*) trim(note) 

 
        write(EchoFileUnit,*) 'Record A3:  PRZM Options'
       CALL check_for_comment(PRZMinputUnit)
       READ(PRZMinputUnit,*,IOSTAT = status) FLAG1, ADJUST_CN, is_TC_lag_method, nonequilibrium_flag, calibrationFlag, number_subdelt, is_true_rain_distribution

       if (calibrationflag) then
         CALL check_for_comment(PRZMinputUnit)
         READ(PRZMinputUnit,'(A)',IOSTAT = status)  CalibrationFilename
         OPEN(Unit = CalibrationDataUnit,FILE=trim(adjustl( CalibrationFilename)),STATUS='OLD', IOSTAT=status)
         IF (status .NE. 0) THEN
             note = 'Problem with calibration data file.'
             CALL ERRCHK(note,FATAL)
         ENDIF  
         note = 'Calibration file:'  // trim(adjustl(filename))
         WRITE(EchoFileUnit,*)  trim(note)
         
         READ(PRZMinputUnit,'(A)',IOSTAT = status)  CalibrationFilenameOutput
         
       end if
       
      write(EchoFileUnit,*) 'Record 1'
      CALL check_for_comment(PRZMinputUnit)
      READ(PRZMinputUnit,*,IOSTAT = status) PFAC,SFAC, min_evap_depth
      
      if (status/=0) stop
                
      write(EchoFileUnit,*) 'Record 2'
       CALL check_for_comment(PRZMinputUnit)
      READ(PRZMinputUnit,*,IOSTAT = status)  ERFLAG
      if (status/=0) stop 

!    ERFLAG = 0 indicates that erosion will not be calculated
      if (ERFLAG.eq.1 .or. ERFLAG.gt.4 .or. ERFLAG.lt.0) then
         note = 'Erosion flag (ERFLAG) out of range'
         FATAL = .True.
         Call ERRCHK (note, FATAL)
      ENDIF 
      
      write(EchoFileUnit,*) 'Record 3'
      CALL check_for_comment(PRZMinputUnit)
      READ(PRZMinputUnit,*,IOSTAT = status) USLEK,USLELS,USLEP,AFIELD,IREG,SLP,HL
      if (status/=0) stop            
   

      !--------- Erosion/Curve number Inputs ---------------------
          
      write(EchoFileUnit,*) 'Record 4'
      CALL check_for_comment(PRZMinputUnit)
      READ(PRZMinputUnit,*) NUSLEC, use_usleyears
      if (status/=0) stop 
          
      write(EchoFileUnit,*) 'Record 5'
      CALL check_for_comment(PRZMinputUnit)
      
      do i= 1, NUSLEC
         READ(PRZMinputUnit,*)  GDUSLEC(i), GMUSLEC(i), GYUSLEC(i),USLEC(i),MNGN(i),CN_2(i)           
      end do
     
      !-------------End Erosion Inputs ------------------------
      write(EchoFileUnit,*) 'Record 6'
      CALL check_for_comment(PRZMinputUnit)
      READ(PRZMinputUnit,*,IOSTAT = status)  num_crop_periods
      if (status/=0) stop
      write(EchoFileUnit,*) 'number of crop cycles = ', num_crop_periods 
        
      allocate (emergence_date       (num_crop_periods))
      allocate (maturity_date        (num_crop_periods))
      allocate (harvest_date         (num_crop_periods))   
      allocate (max_root_depth       (num_crop_periods))
      allocate (max_canopy_cover     (num_crop_periods))
      allocate (max_canopy_holdup    (num_crop_periods))
      allocate (max_canopy_height    (num_crop_periods))
      allocate (foliar_disposition   (num_crop_periods))
      
      write(EchoFileUnit,*) 'Record 7'
      DO  I=1,num_crop_periods
        CALL check_for_comment(PRZMinputUnit)
        READ(PRZMinputUnit,*,IOSTAT = status) emd,emm,emy,mad,mam,may,had,ham,hay,max_root_depth(i),max_canopy_cover(i),  &
                                              max_canopy_height(i), max_canopy_holdup(i),foliar_disposition(i) 
        if (status/=0) stop                                
        emergence_date(i) = jd(emy,emm,emd)
        maturity_date(i)  = jd(may,mam,mad)
        harvest_date(i)   = jd(hay,ham,had) 
      end do
  
      write(EchoFileUnit,*) 'Record 8 Flags'
      call check_for_comment(PRZMinputUnit)
      READ(PRZMinputUnit,*,IOSTAT = status)  IRFLAG, is_temperature_simulated 
      if (status/=0) stop
          
!     reads in irrigation parameters if IRFLAG > 0
      IF(IRFLAG .NE. 0)THEN
        write(EchoFileUnit,*) 'Record 9 Irrigation'
        call check_for_comment(PRZMinputUnit)
        !New Variables: user_irrig_depth, UserSpecifiesDepth
        READ(PRZMinputUnit,*,IOSTAT = status) IRTYPE,FLEACH,PCDEPL,max_irrig, UserSpecifiesDepth, user_irrig_depth
        if (status/=0) stop
                
        IF (IRTYPE ==2) then
            write (echofileunit,*) "Irrigation Type 2 is not supported in PRZM 5"
            stop
        end if    
        
        !removed 2/10/2020 check 
         !value_in_range = (0.0 <= PCDEPL) .And. (PCDEPL <= 1.0)
         !If (.Not. value_in_range) Then
         !   note = "PCDEPL was not in the range 0 to 1.0; PCDEPL was reset to 0.5"
         !   FATAL = .False.
         !   CALL ERRCHK(note,FATAL)
         !   PCDEPL = 0.5
         !End If
      END IF

      IF (is_temperature_simulated) THEN  !soil temperature simulation, if ITFLAG = 1
        write(EchoFileUnit,*) 'Record 10 '
        call check_for_comment(PRZMinputUnit)
        READ(PRZMinputUnit,*,IOSTAT = status)(ALBEDO(I),I=1,12),EMMISS
        
        write(EchoFileUnit,*) "status of record 15 (0 is good)= ", status
        if (status/=0) stop

        write(EchoFileUnit,*) 'Record 11 '
        call check_for_comment(PRZMinputUnit)
        READ(PRZMinputUnit,*) uWind_Reference_Height,   Height_stagnant_air_layer_cm
             
!       reads in Lower boundary temperatures if is_temperature_simulated
        write(EchoFileUnit,*) 'Record 12'
        call check_for_comment(PRZMinputUnit)
        READ(PRZMinputUnit,*,IOSTAT = status)(BBT(I),I=1,12)
                
         write(EchoFileUnit,*) "status of record 16 = ", status
        if (status/=0) stop
        
!       reads in Q10FAC and TBASE
        write(EchoFileUnit,*) 'Record 13'
        call check_for_comment(PRZMinputUnit)
        READ(PRZMinputUnit,*,IOSTAT = status) Q_10,TBASE
        write(EchoFileUnit,*) "status of record 16 = ", status
        
        if (status/=0) stop
      ENDIF
        
      write(EchoFileUnit,*) 'Record 14'
      call check_for_comment(PRZMinputUnit)
      READ(PRZMinputUnit,*,IOSTAT = status) NHORIZ
      if (status/=0) stop

      do I=1,NHORIZ
        write(EchoFileUnit,*) 'Record 15  Soil Parameters with temperature'
        call check_for_comment(PRZMinputUnit)
        READ(PRZMinputUnit,*,IOSTAT=status) dummy,thickness(I),num_delx(I),dispersion_input(i),bd_input(I), &
                                            theta_zero_input(i),fc_input(i),wp_input(i),oc_input(i),  &
                                            sand_input(i),clay_input(i),soil_temp_input(I)
        if (status/=0) stop
      end do
   
      write(EchoFileUnit,*) 'Record 16  PRZM5 runoff extraction parameters'
      call check_for_comment(PRZMinputUnit)
      READ(PRZMinputUnit,*,IOSTAT = status) runoff_extr_depth,runoff_decline,runoff_effic     
      if (status/=0) stop 
      
      write(EchoFileUnit,*) 'Record 17  PRZM5 erosion extraction parameters'
      call check_for_comment(PRZMinputUnit)
      READ(PRZMinputUnit,*,IOSTAT = status)  erosion_depth, erosion_decline, erosion_effic
      if (status/=0) stop     
      
      
      !*********CHEMICAL INPUTS*************************************
      write(EchoFileUnit,*) 'Record C1'
      call check_for_comment(PRZMinputUnit)
      READ(PRZMinputUnit,*,IOSTAT = status) num_applications, NCHEM, is_adjust_for_rain ,rain_limit, intolerable_rain_window,  optimum_application_window, min_days_between_apps
      if (status/=0) stop
      
      IF (NCHEM .EQ. 0) NCHEM = 1
      
      !*** allocate pesticide application parameters ****
      allocate(application_date(num_applications))
      allocate(application_rate(num_applications))
      
      allocate(DEPI            (num_applications))     
      allocate(TAPP            (num_applications))  !used first in initialization but put here for consistency         
      allocate(APPEFF          (num_applications))
      allocate(Tband_top       (num_applications))
      allocate(CAM             (num_applications))      
      !****************************************

      write(EchoFileUnit,*) 'Record C2'
      do i=1, num_applications
        call check_for_comment(PRZMinputUnit)
        READ(PRZMinputUnit,*,IOSTAT = status) APD,APM,APY, CAM(i),DEPI(i), application_rate(i), APPEFF(i), Tband_top(i)
 
        if (status/=0) stop  
        application_date(i) = jd(APY, apm,apd)  !determine julian application date
      end do
      
      some_applications_were_foliar = .FALSE.
      if(any(cam==2).or.any(cam==3).or.any(cam==9).or.any(cam==10))then
          some_applications_were_foliar = .TRUE.
      end if
      
      write(EchoFileUnit,*) 'Some applications were foliar: ', some_applications_were_foliar
      
      write(EchoFileUnit,*) 'Reading Uptake Factor'
      
      
      call check_for_comment(PRZMinputUnit)
      READ(PRZMinputUnit,*)   (UPTKF(K),K=1,NCHEM)

      IF(some_applications_were_foliar)THEN  !foliar applications
        write(EchoFileUnit,*) 'Record C4'
        DO  K=1,NCHEM
         call check_for_comment(PRZMinputUnit)
         READ(PRZMinputUnit,*,IOSTAT = status) plant_volatilization_rate(K),plant_pesticide_degrade_rate(K),plant_washoff_coeff(K)
          write(EchoFileUnit,*) plant_volatilization_rate(K),plant_pesticide_degrade_rate(K),plant_washoff_coeff(K)
         
         if (status/=0) stop         
        end do
      ENDIF

      IF((some_applications_were_foliar).AND.(NCHEM.GT.1))THEN
        write(EchoFileUnit,*) 'Record C5'
        call check_for_comment(PRZMinputUnit)
        READ(PRZMinputUnit,*,IOSTAT = status) foliar_formation_ratio_12, PTRN13, foliar_formation_ratio_23
        if (status/=0) stop        
      ENDIF
      
      write(EchoFileUnit,*) 'Record C6'
      do i=1, nchem     
          call check_for_comment(PRZMinputUnit)
          READ(PRZMinputUnit,*,IOSTAT = status) DAIR(I),HENRYK(I),ENPY(I)
          if (status/=0) stop  
      end do            

      
      
      do I=1,NHORIZ
           write(EchoFileUnit,*) 'Record C7  Kf'
           call check_for_comment(PRZMinputUnit)
           READ(PRZMinputUnit,*,IOSTAT = status)(k_f_input(K,I),K=1,NCHEM)
           if (status/=0) stop
      end do
      
      do I=1,NHORIZ
           write(EchoFileUnit,*) 'Record C7A  Freundlich N'
           call check_for_comment(PRZMinputUnit)
           READ(PRZMinputUnit,*,IOSTAT = status)(N_f_input(K,I),K=1,NCHEM)
           if (status/=0) stop
      end do
      
      do I=1,NHORIZ
           write(EchoFileUnit,*) 'Record C7B  Region 2 Freundlich Coefficients'
           call check_for_comment(PRZMinputUnit)
           READ(PRZMinputUnit,*,IOSTAT = status)(k_f_2_input(K,I),K=1,NCHEM)
           if (status/=0) stop
      end do
      
      do I=1,NHORIZ
           write(EchoFileUnit,*) 'Record C7C  Region 2 Freundlich Exponents'
           call check_for_comment(PRZMinputUnit)
           READ(PRZMinputUnit,*,IOSTAT = status)(N_f_2_input(K,I),K=1,NCHEM)
           if (status/=0) stop
           write(EchoFileUnit,*) (N_f_2_input(K,I),K=1,NCHEM)
      end do    
 
      write(EchoFileUnit,*) 'Record C7D  Concentration below which isotherm is linear(mg/L)'
      call check_for_comment(PRZMinputUnit)
      READ(PRZMinputUnit,*,IOSTAT = status) lowest_conc 
   
      write(EchoFileUnit,*) 'Record C7E Mass Transfer Coefficient, K2'
      call check_for_comment(PRZMinputUnit)
      READ(PRZMinputUnit,*,IOSTAT = status) (K2(K),K=1,NCHEM)
      write(EchoFileUnit,*)  "Mass Transfer Coefficient = ", ( K2(K),K=1,NCHEM)
       
  
       
     do I=1,NHORIZ
          write(EchoFileUnit,*) 'Record C8 Degradation Rates'
          call check_for_comment(PRZMinputUnit)
  
          !changed order on 9/9/14
          READ(PRZMinputUnit,*,IOSTAT = status)(Aq_rate_input(K,I),Sorb_rate_input(K,I), Gas_rate_input(K,I), K=1, NCHEM)
          if (status/=0) stop
     end do

     do i=1,NHORIZ
          !read the molar prouction ratio of daughter to parent
             write(EchoFileUnit,*) 'Record C9'
             call check_for_comment(PRZMinputUnit)
             READ(PRZMinputUnit,*,IOSTAT = status) MolarConvert_aq12_input(i),MolarConvert_aq13_input(i),MolarConvert_aq23_input(i), &
                                                   MolarConvert_s12_input(i), MolarConvert_s13_input(i) ,MolarConvert_s23_input(i)
             
             !,DKRS12(I),DKRS13(I),DKRS23(I)  not used
            
             if (status/=0) stop
     end do

 
   
!****************TIME SERIES OUTPUT****************

      write(EchoFileUnit,*) 'Record U1'
      call check_for_comment(PRZMinputUnit)
      READ(PRZMinputUnit,*,IOSTAT = status) NPLOTS


      IF (NPLOTS .GT. 0) THEN
   
        DO I=1,NPLOTS
          write(EchoFileUnit,*) 'Record U2' 
          call check_for_comment(PRZMinputUnit)
          READ(PRZMinputUnit,*) PLNAME(I),chem_id(I),MODE(I),ARG(I),IARG2(I),CONST(I) 
        end do
        
        
        
        !write the output file header
        call write_outputfile_header
      ENDIF
      
 
      write(EchoFileUnit,*) 'End of record reading'

  END subroutine PRZMRD_PRZM5
  

end module readinputs