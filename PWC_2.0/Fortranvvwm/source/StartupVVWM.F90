Program StartupVVWM
!Purpose of the startup is to populate the Variable module.
!Command line to execute has one argument and is as follows:
! c:\> [name of this executable] ("path and file name of input file")
    use variables
    use VVWM_module
    implicit none
    
    integer :: ierror,length,i,status
    character (len=256) command
    logical :: is_Koc

    aer_aq_all= 0.0   !if blank is read in for half life, a zero will be left and represents stability
    anae_aq_all= 0.0   !if blank is read in for half life, a zero will be left and represents stability
    hydro_all = 0.0
    photo_all=0.0
   

   call get_command_argument(1,command,length)
    
    open (UNIT = 15, FILE = command, STATUS = 'old')
    
    write(*,*) "Start reading VVWM inputs"

    read(15,'(A256)')  outputfilename          !Line 1  output file name
    read(15,*)                                 !Line 2  chem name, not used 
    read(15,*) nchem                           !Line 3
    read(15,*) is_Koc                          !Line 4 logical info used in GUI for Koc/Kd status,  True means Koc
    read(15,*) (koc_all(i),  i=1,nchem)        !Line 5 Koc (ml/g)    
    read(15,*) (aer_aq_all(i),i=1,nchem)       !Line 6 aerobic aquatic halflife(days)
    read(15,*) (temp_ref_aer_all(i),i=1,nchem) !Line 7 ref temperature of aerobic(C)
    read(15,*) (anae_aq_all(i), i=1,nchem)     !Line 8 anaerobic aquatic halflife(days)
    
    write(*,*) "Done reading first 8 lines"
!Line 9    
    read(15,*) (temp_ref_anae_all(i), i=1,nchem)    !ref temperature of anaerobic(C)
!Line 10    
    read(15,*) (photo_all(i),  i=1,nchem)           !photolysis halflife(days)
!Line 11    
    read(15,*) (RFLAT_all(i),  i=1,nchem)           !latitude of photo study  
!Line 12    
    read(15,*) (hydro_all(i),  i=1,nchem)           !hydrolysis halflife (days)
    write(*,*) "Done reading first 12 lines"
    
    
!Line 13
    read(15,*) 
!Line 14    
    read(15,*) 
!Line 15   
    read(15,*)
!Line 16
    read(15,*) (MWT_all(i),  i=1,nchem)           !molecular wt  
!Line 17    
     write(*,*) "Done reading first 17 lines"
    
    read(15,*) (VAPR_all(i),  i=1,nchem)          !vapor pressure (torr)

    read(15,*) (SOL_all(i),  i=1,nchem)           !Line 18 solubilty (mg/L) 
    read(15,*) (xAerobic(i),i=1,nchem-1)          !Line 19  molar conversion factor aerobic 
    read(15,*) (xBenthic(i),i=1,nchem-1)          !Line 20  molar conversion factor benthic  
    read(15,*) (xPhoto(i),  i=1,nchem-1)          !Line 21  molar conversion factor photolysis
    read(15,*) (xHydro(i),  i=1,nchem-1)          !Line 22  molar conversion factor hydrolysis 
    read(15,*)                                    !Line 23   
    read(15,*)                                    !Line 24
    read(15,*)                                    !Line 25
    read(15,*) (Henry_unitless(i),  i=1,nchem)    !Line 26   
    read(15,*) (Heat_of_Henry(i),   i=1,nchem)    !Line 27
    read(15,*) QT                                 !Line 28    

    read(15,'(A50)')   scenario_id           !Line 29!  identifier for scenario, used for output file naming 
    read(15,'(A256)')  metfilename           !Line 30   !met file name
    write(*,*) "Done reading first 30 lines"
    read(15,*) Text_latitude                 !  Line 31, read in as text so it is compatible with previous metfile
    
    write(*,*) "Done reading first 31 lines"
    
    
    read(15,*) ! Unused      Line 32  
    read(15,*) ! Unused      Line 33  
    read(15,*) burialFlag   !Line 34  TRUE = burial
    
    write(*,*) "Done reading first 34 lines"


    read(15,*) !Line 35 Unused in VVWM, but stores custom afield in GUI   !area of adjacent runoff-producing field    
    read(15,*) !Line 36 Unused in VVWM, but stores custom area in GUI
    read(15,*) !Line 37 Unused in VVWM, but stores custom depth_0 in GUI   initial water body depth
    read(15,*) !Line 38 Unused in VVWM, but stores custom depth_max in GUI !maximum water body depth

    read(15,*) D_over_dx !Line 39
    
    write(*,*) "Done reading first 39 lines"
    
    read(15,*) is_calc_prben, PRBEN     !Line 40

    write(*,*)prben
    
    
    read(15,*) benthic_depth !Line 41
    read(15,*) porosity      !Line 42  volume water/total volume
    read(15,*) bulk_density  !Line 43  dry mass/total volume  g/ml
    read(15,*) FROC2         !Line 44  fraction oc in benthic sed
    read(15,*) DOC2          !Line 45
    read(15,*) BNMAS         !Line 46  
    read(15,*) DFAC          !Line 47
    read(15,*) SUSED         !Line 48  suspended soilids in water column
    read(15,*) CHL           !Line 49
    read(15,*) FROC1         !Line 50
    read(15,*) DOC1          !Line 51
    read(15,*) PLMAS         !Line 52
    

    read(15,*)               !Line 53 unused in VVWM, in GUI stores logical for using EPA default parameters
    read(15,*)               !Line 54 PCA Reservoir and Custom water body used here only to report in output output
    read(15,*)               !Line 55 unused
    read(15,*) napp          !Line 56 number of apps
    
    allocate (appdate_sim_ref(napp), STAT=status)
    read(15,*)(appdate_sim_ref(i),  i=1, napp) !Line 57 series app days

!********************************************************************************
    read(15,*) simtypeflag  !SimTypeFlag Overide  !Line 58
       ! 1= vary volume w/ flowthrough, 2=const volume, no flowthrough, 3=const vol, flowthrough, 
       ! 4 = const vol no flow, 5 = const vol, flow
       ! 2 & 3 For use with the USEPA pond and reservoir ( and other situations)
       
     If (.not. is_Koc)  then
           koc_all = koc_all/froc2   !This turns the Kd value into a Koc, which the model uses
     end if 
   
    read(15,*) afield         !Line 59 area of adjacent runoff-producing field    
    read(15,*) area           !Line 60
    read(15,*) depth_0        !Line 61 initial water body depth
    read(15,*) depth_max      !Line 62 maximum water body depth
    
    
    allocate (spray(napp), STAT=status)
    read(15,*) (spray(i), i=1, napp)   !Line 63
    read(15,*) flow_averaging          !Line 64 days used to avg flow for special case reservoir, 0 indicates complete sim avg
    read(15,*) baseflow                !Line 65
    read(15,*) cropped_fraction        !Line 66
    
    read(15,*) is_hed_files_made
    
    read(15,*, IOSTAT= ierror)  is_add_return_frequency, additional_return_frequency
       if (ierror /= 0) then 
           is_add_return_frequency = .false.
       end if
    
       
       
    read(15,*) outputfile_parent_daily
    read(15,*) outputfile_deg1_daily
    read(15,*) outputfile_deg2_daily
     
    read(15,*) outputfile_parent_analysis
    read(15,*) outputfile_deg1_analysis
    read(15,*) outputfile_deg2_analysis
    
    read(15,*) outputfile_parent_deem
    read(15,*) outputfile_deg1_deem
    read(15,*) outputfile_deg2_deem
    
    read(15,*) outputfile_parent_calendex
    read(15,*) outputfile_deg1_calendex
    read(15,*) outputfile_deg2_calendex
    
    read(15,*) outputfile_parent_esa
    read(15,*) outputfile_deg1_esa
    read(15,*) outputfile_deg2_esa
    
    read(15,*)   Batch_outputFile
    read(15,*)   is_minimum_batch_output
    read(15,*)   run_id
    
close(15)
    

    call VVWM          

    
    
    
    
end program startupVVWM



