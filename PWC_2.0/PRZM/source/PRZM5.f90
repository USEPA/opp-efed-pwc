PROGRAM   PRZM5
    
  ! Start file of PRZM5
    
   use constants_and_Variables, ONLY: EchoFileUnit,maxFileLength,calibrationFlag,RunFilePath, julday1900 ,&
                                  precip, pan_evap,air_temperature ,wind_speed, solar_radiation, &
                                  precipitation,PEVP,air_TEMP,WIND, SOLRAD ,harvest_day, &
                                  startday, num_records,is_harvest_day,canopy_holdup, &
                                  canopy_height, canopy_cover  , cover, height, harvest_placement, &
                                  potential_canopy_holdup,evapo_root_node_daily, &
                                  evapo_root_node,root_node ,root_node_daily,atharvest_pest_app,day_number,is_adjust_for_rain 

   use PRZM_VERSION
   use initialization
   use utilities
   use readinputs
   use PRZM_core
   use plantgrowth
   use Pesticide_Applications
   use calibration
   implicit none
   
   integer :: i , IERROR
   !integer YEAR,MONTH,DAY
   Character(Len=maxFileLength) :: filename

   !Command line may contain a path to the runfile, else path is the working default path
   Call GetArgs (RunFilePath)


   !KECHO.prn records the operating status of PRZM during runtime
   filename =  trim(RunFilePath) //  'KECHO.PRN'    
  
   
   OPEN(Unit=EchoFileUnit,FILE=Filename,STATUS='UNKNOWN',IOSTAT=IERROR)  

   call przm_id(EchoFileUnit)
  
  ! CALL read_run_file(RunFilePath)
    
  
   CALL PRZMRD_PRZM5   !read PRZM INPUT FILE

   
   if (new_weatherfile()) then  
      write(EchoFileUnit,*) "wea weather file selected"
       call Read_Weatherfile  !this reads the new format weather file
   else
       write(EchoFileUnit,*) "old dvf weather file selected"
       call Read_Old_Weatherfile  !this reads the new format weather file
   end if
   
    if  ( is_adjust_for_rain ) then   !no need to adjust for rain if continous
       call adjust_application_dates_for_weather
   end  if
   
   
   
   
   CALL INITL    !initialize variables   


   Call Crop_Growth
   
    
   if (calibrationFlag) then
       call read_calibration_data
   end if

   julday1900 = startday
   do i=1, num_records   !day loop driven by metfile only   
     
       day_number = i
       !Vectors determined outside of loop transfered as scalers to loop
       precipitation           = precip(i) 
       PEVP                    = pan_evap(i)
       air_TEMP                = air_temperature(i)
       WIND                    = wind_speed(i)
       SOLRAD                  = solar_radiation(i)
       cover                   = canopy_cover(i)   
       height                  = canopy_height(i) 
       harvest_day             = is_harvest_day(i)
       potential_canopy_holdup = canopy_holdup(i)
       evapo_root_node_daily   = evapo_root_node(i)
       root_node_daily         = root_node(i)       !only needed for irrigation
       harvest_placement       = atharvest_pest_app(i)
       

      CALL PRZM 
      julday1900 = julday1900  +1


   end do
   
  
   write(EchoFileUnit,*) 'PRZM5 program normal completion.'
  
   if (calibrationFlag) then
       call write_calibration_data
   end if
   
END PROGRAM PRZM5