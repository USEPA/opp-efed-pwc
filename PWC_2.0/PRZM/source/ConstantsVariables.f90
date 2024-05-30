module constants_and_variables
    
    
      implicit none
      save
      real,parameter :: Version_Number = 5.08
    
     !*** New Routine Application Optimizing for Rainfall ********* 
     logical :: is_adjust_for_rain       !True means to adjust the applications dates
     real    :: rain_limit               !cm
     integer :: optimum_application_window 
     integer :: intolerable_rain_window
     integer :: min_days_between_apps
      
      
     !*********** Parameters used for calibration Routines *********
      integer :: data_number = 0
      integer :: data_date(100) = 0
      real    :: data_erosion(100) = 0.0
      real    :: data_runoff(100) = 0.0
      real    :: data_mgsed(100) = 0.0
      real    :: data_mgh2o(100) = 0.0
      
      real    :: model_erosion(100) = 0.0
      real    :: model_runoff(100) = -0.0
      real    :: model_mg_ro(100) = 0.0
      real    :: model_mg_er(100) = 0.0
      
      real    :: cn_moisture(100) = 0.0   
      real    :: Calibrated_curve_Number(100) = 0.0
      !************************************************************
      
      logical :: is_true_rain_distribution
      logical :: is_TC_lag_method
      
      integer :: day_number  !only used for diagnostics
   
      
      integer, parameter :: maxFileLength = 300  ! Maximum path and File name length  probably should limit to 256 due to windows limit
      integer, parameter :: max_horizons = 10    ! maximum number of soil horizons  
      integer, parameter :: max_number_plots = 100

      integer, parameter ::  PRZMinputUnit  = 51
      integer, parameter ::  MetFileUnit    = 52
      integer, parameter ::  TimeSeriesUnit = 54
      integer, parameter ::  EchoFileUnit   = 56
      integer, parameter ::  CalibrationDataUnit = 57
      
      Character(Len = maxFileLength) :: CalibrationFilename
      Character(Len = maxFileLength) :: CalibrationFilenameOutput
      
      Character(Len = maxFileLength) :: RunFilePath = ''
      
      integer, parameter :: Num_hydro_factors = 100
      
      real,parameter    :: DELT = 1.0  !1 day time step, this value is not adjustable unless considerations are made for runoff which is daily
      
      integer     :: number_subdelt  !number of sub time steps in tridigonal calculation (used to investigate effect on nonlinear isotherms)
      real        :: subdelt         !delt/number_subdelt
      
      integer :: NCOM2              ! actual number of total compartments   
      
      integer :: user_node =1      ! a user specified node that is used for specific output of flux at a depth (node), not hooked up yet
         

      Character(Len = maxFileLength)    :: weatherfilename
      
      
      !*****Soil Profile Dependent Input Parameters *****************
      integer :: NHORIZ                           !input value
      integer :: Num_delx(max_horizons)           !Input Number of discretizations in horizon
      real    :: bd_input(max_horizons)           !bulk density of each horizon input
      real    :: clay_input(max_horizons)         !clay of each horizon input
      real    :: sand_input(max_horizons)         !clay of each horizon input
      real    :: oc_input(max_horizons)           !organic carbon fraction input
      real    :: theta_zero_input(max_horizons)   !organic carbon fraction input
      real    :: fc_input(max_horizons)           !field capacity input
      real    :: wp_input(max_horizons)           !wilting point input
      real    :: soil_temp_input(max_horizons)    !input of initial soil temperatures
      real    :: thickness(max_horizons)          !thickness of each horizon
      real    :: Aq_rate_input(3, max_horizons)   !input aqueous degradation rate parent, daughter, grand daughter
      real    :: Sorb_rate_input(3,max_horizons)  !input sorbed degradation rate parent, daughter, grand daughter
      real    :: Gas_rate_input(3,max_horizons)   !input gas degradation rate parent, daughter, grand daughter
      real    :: dispersion_input(max_horizons)   !replacement for disp(3,ncmpts)
      real    :: k_f_input(3,max_horizons)        !input Freundlich Kf for each horizon
      real    :: N_f_input(3,max_horizons)        !input Freundlich N for each horizon
       
      !nonequilibrium parameters
      real :: k_f_2_input(3,max_horizons)  !input Freundlich Kf for each horizon Region 2
      real :: N_f_2_input(3,max_horizons)  !input Freundlich N for each horizon Region 2
          
      real :: MolarConvert_aq12_input(max_horizons),MolarConvert_aq13_input(max_horizons),MolarConvert_aq23_input(max_horizons)
      real :: MolarConvert_s12_input(max_horizons), MolarConvert_s13_input(max_horizons), MolarConvert_s23_input(max_horizons)
        
      real :: lowest_conc             !Below this concentration (mg/L), Freundlich isotherm is approximated to be linear to prevent numerical issues
           
      !Compartment-Specific Parameters
      real,allocatable,dimension(:,:) :: k_freundlich   !Freundich Coefficient in equilibrium Region  
      real,allocatable,dimension(:,:) :: N_freundlich   !Freundich Exponent in equilibrium Region
      real,allocatable,dimension(:,:) :: k_freundlich_2 !Freundich Coefficient in Nonequilibrium Region 2
      real,allocatable,dimension(:,:) :: N_freundlich_2 !Freundich Exponent in Nonequilibrium Region 2 
      
      real,allocatable,dimension(:) :: soil_depth       !vector of soil depths
      real,allocatable,dimension(:) :: wiltpoint_water  !wilting point content weighted by size of compartment (absolute water content)
      real,allocatable,dimension(:) :: fieldcap_water   !field capacity content weighted by size of compartment (absolute water content)
      real,allocatable,dimension(:) :: delx
      real,allocatable,dimension(:) :: bulkdensity
      
      real,allocatable,dimension(:) :: clay
      real,allocatable,dimension(:) :: sand
      real,allocatable,dimension(:) :: orgcarb
      real,allocatable,dimension(:) :: theta_zero  !beginning day water content
      real,allocatable,dimension(:) :: theta_end   !end day water content
      real,allocatable,dimension(:) :: theta_sat   !stauration water content (fractional)
      real,allocatable,dimension(:) :: theta_fc    !field capacity water content (fractional)
      real,allocatable,dimension(:) :: theta_wp    !wilting point water content (fractional)
        
      real,allocatable,dimension(:) :: soil_temp   !soil temperature
      real,allocatable,dimension(:) :: soilwater
      
      real,allocatable,dimension(:,:) :: dwrate, dsrate, dgrate
      real,allocatable,dimension(:,:) :: dwrate_atRefTemp, dsrate_atRefTemp, dgrate_atRefTemp
      
      real,allocatable,dimension(:) :: MolarConvert_aq12,MolarConvert_aq13,MolarConvert_aq23
      real,allocatable,dimension(:) :: MolarConvert_s12, MolarConvert_s13, MolarConvert_s23
      
      real,allocatable,dimension(:) :: dispersion  !formerly disp
      real,allocatable,dimension(:) :: thair_old
      real,allocatable,dimension(:) :: thair_new
      
      real,allocatable,dimension(:,:) :: Kd_new
      real,allocatable,dimension(:,:) :: Kd_old   !current time step Kd (applies to nnlinear isothrems)
      real,allocatable,dimension(:,:) :: Kd_2
      
      real,allocatable,dimension(:,:) :: new_henry  !henry constant for current time step (end of current step)
      real,allocatable,dimension(:,:) :: old_Henry  !Henry constant previous time step (start of current step)
      
      real,allocatable,dimension(:,:) :: Sorbed2  !nonequilib phase conc (internal units of g/g to correspond with mg/L aqueous)
    
      real,allocatable,dimension(:,:) :: conc_porewater
      real,allocatable,dimension(:,:) :: conc_total_per_water     !pest conc in Total Mass in all phases divided by water content)
      real,allocatable,dimension(:,:) :: mass_in_compartment      !New experimetal variable to replace "conc_total_per_water"
      real,allocatable,dimension(:,:) :: mass_in_compartment2     !mass in nonequilibrium compartment
      real,allocatable,dimension(:,:) :: SOILAP  !g/cm2  Mass of applied pesticide in soil
      
      real,allocatable,dimension(:) :: DGAIR !effective air diffusion coefficient through profile for chemical in the loop  
         
      !****Pesticide Flux**************
      real,allocatable,dimension(:,:) :: DKFLUX
      real,allocatable,dimension(:,:) :: SRCFLX
      real,allocatable,dimension(:,:) :: PVFLUX
      real,allocatable,dimension(:,:) :: UPFLUX
      real,allocatable,dimension(:,:) :: DFFLUX
      real,allocatable,dimension(:,:) :: ADFLUX
            
      real,allocatable,dimension(:,:) :: soil_applied_washoff
   
      real :: ERFLUX(3), WOFLUX(3), ROFLUX(3), RZFLUX(3)
 
      real, parameter :: washoff_incorp_depth  = 2.0 !depth at which foliar washoff pesticide is incorporated into soil
      !previously this value was fixed to the runoff extraction depth
      integer :: number_washoff_nodes  !number of nodes coreresponding to washoff_incorp_depth

     ! integer :: NCOM1  !holds the lowest relavant evaopration node for each day
     ! integer :: NCOM0  !holds the node of corresponding to anetd
 
       integer :: min_evap_node  !holds the node of corresponding to anetd (min_evap_depth)
 
 
      Real, Parameter :: cam123_soil_depth = 4.0 ! cm  Default pesticide incorporation depth
      real, parameter :: CN_moisture_depth = 10.0  !depth of soil moisture for CN calculations
      
      REAL :: r1
      REAL :: R0MIN  = Tiny(r1)
                   
      logical :: really_not_thrufl  !flag to prevent washoff of undercanopy irrigation in PRZM3 (not used in PRZM5)
      real    :: canopy_flow        !flow through the canopy
      real    :: effective_rain     !total water, without consideration for canopy holdup 
      real    :: THRUFL             !total water minus canopy holdup 
      
      !***Simulation Timne **************************************************
      integer  :: startday      !julian day start date referenced to 1900 (note that we use two digit date, makes no difference)
      integer  :: julday1900    !the current day referenced to Jan 1, 1900
      integer  :: first_year
         
      
      !***PRZM5 Advanced Analysis OPtipon
      logical :: FLAG1      !True if Freundlich


   
      logical :: nonequilibrium_flag
      logical :: calibrationFlag =.FALSE.
      
      logical :: ADJUST_CN
      
      !*****Pesticide Applications ******************
      integer :: num_applications
      integer,allocatable,dimension(:) :: application_date
      integer,allocatable,dimension(:) :: CAM    
      
      real,allocatable,dimension(:) :: DEPI
      real,allocatable,dimension(:) :: application_rate  !New For PRZM5, app rate that is read in and remains unchanged     
      real,allocatable,dimension(:) :: TAPP              !read in as KG/HA but converted to G/CM**2 in initialization
      real,allocatable,dimension(:) :: APPEFF
      real,allocatable,dimension(:) :: Tband_top
      
      real    :: plant_app     !store the plant applied pesticide
      logical :: some_applications_were_foliar
      
            
     !*****Pesticide Properties ******************  
      real    :: plant_pesticide_degrade_rate(3)    !plant decay rate
      real    :: plant_washoff_coeff(3)             !plant washoff coefficient (per cm rainfall)  
      real    :: ENPY(3)                           
      real    :: SOL(3)                            
      integer :: PCMC                               !signal to tell if its Koc to use
      integer :: KDFLAG                             !Select Kd calculation
      integer :: THFLAG                             !computw Wp and FC based on texture  
      real    :: k2(3)                              !noneq mass transfer coeff
      

      
      
      real :: foliar_formation_ratio_12, PTRN13, foliar_formation_ratio_23 !foliar transformation rates 
      real :: plant_volatilization_rate(3)   !plant pesticide volatilization rate (does not produce degradate)
      

      REAL :: UPTKF(3)         !PLANT UPTAKE FACTOR, CONVERTED TO GAMMA1 IN PLANTGROW

      !********Volatilization
      real           :: Height_stagnant_air_layer_cm = 0.5  !(can be changed by input file)
      real           :: uWind_Reference_Height = 10.0       !(can be changed by input file)
      real,parameter :: vonKarman = 0.4
      
      real :: CONDUC(3)        !Some canopy volatilization parameter
      REAL :: DAIR(3)
      real :: HENRYK(3)

      real,parameter :: henry_ref_temp  = 25.0
      
      REAL    :: Q_10   !temperature basis for formerly QFAC
      REAL    :: TBASE  !temperature reference for Q_10 degradation
      
     integer :: NCHEM

   
      !*******Heat transfer variables*********************
      real    :: emmiss   
      real    :: UBT                            !used in volatilization and heat routines

      real    :: Albedo(13), bbt(13)
   !   integer :: ITFLAG
      
      logical :: is_temperature_simulated

      
                
      
      real :: enriched_eroded_solids  !mass of eroded solids bumped up be an enrichment factor,  g/cm2, (analogous to runoff)
   
      real :: TCNC(3)     !used for output only
      
      


      !******Weather Related Variables******************       
      real :: precip_rain     !rain-only component, minus snow
      real :: PFAC,SFAC
      real :: SNOWFL

      integer :: num_records  !number od records in weather file
      
      
      real, allocatable, dimension(:) :: precip, pan_evap,air_temperature, wind_speed, solar_radiation  !fromn weather file
      
      !Weather Scalers
      
      real :: wind
      real :: precipitation,air_TEMP,PEVP,SOLRAD !old ones
      
      !*****Irrigation Parameters
      integer :: IRFLAG
      integer :: IRTYPE
      
      real    :: PCDEPL,FLEACH      !inputs
      real    :: max_irrig  !maximum  irrigation water over 24 hour period
      real    :: under_canopy_irrig, over_canopy_irrig,flood_irrig  
      
      real    :: IRRR                      !daily irrigation water, used for time series reporting only
      logical :: UserSpecifiesDepth        !True if custom depth, False if auto with root depth
      real    :: user_irrig_depth          !user-specified depth of irrigation, input value
      integer :: user_irrig_depth_node     !node for user-specified depth of irrigation, calc in INITL subroutine
      
      !*****Crop Parameters ********* 

      real    :: min_evap_depth  ! the minimum depth that is used for PET satisfaction                     
      !VECTORS ODF SIZE NUM_RECORDS
      real,   allocatable, dimension(:) :: crop_fraction_of_max  !vector of daily fration of crop growth
      real,   allocatable, dimension(:) :: canopy_cover          !Fractional Coverage (but in PWC interface it is %)
      real,   allocatable, dimension(:) :: canopy_height
      real,   allocatable, dimension(:) :: canopy_holdup
      real,   allocatable, dimension(:) :: root_depth
      integer,allocatable, dimension(:) :: evapo_root_node  !Node that represents the root depth or evap zone (whichever greatest) Ncom1
      integer,allocatable, dimension(:) :: root_node        !Node for the root (formerly nrzomp)
      integer,allocatable, dimension(:) :: atharvest_pest_app
      
      logical,allocatable, dimension(:) :: is_harvest_day !True if the current day is a harvest day
    
      !VECTORS OF SIZE OF INPUT READ
      integer,allocatable,dimension(:) :: emergence_date
      integer,allocatable,dimension(:) :: maturity_date 
      integer,allocatable,dimension(:) :: harvest_date  
      integer,allocatable,dimension(:) :: foliar_disposition       !1 = pesticide is surface applied at harvest, 2 = complete removal, 3 = left alone, 4 = bypass, 0 = ????
      real   ,allocatable,dimension(:) :: max_root_depth
      real   ,allocatable,dimension(:) :: max_canopy_cover
      real   ,allocatable,dimension(:) :: max_canopy_holdup
      real   ,allocatable,dimension(:) :: max_canopy_height
     
      
      !Scalers for core przm run
      integer :: root_node_daily   
      integer :: evapo_root_node_daily       !the nodes to the bottom of evap/root zone
      real    :: cover                       !Fractional Coverage (but in PWC interface it is %)
      real    :: height     
      integer :: harvest_placement
      logical :: harvest_day


      integer  :: Num_Days_into_crop                    !number of days into crop growth cycle
      
      integer :: num_crop_periods                 !number of cropping periods
      
      integer :: current_grow_days        !number of days between emergence and harvest for current period

      

      !*****Erosion Parameters *************************
      real :: AFIELD           !INPUT, field area input, hA
      real :: USLEK            !INPUT,
      real :: USLEP            !INPUT, 
      real :: USLELS           !INPUT,
      real :: HL               !INPUT,
      real :: SLP              !slope. no erosion if slope is zero
      
      integer :: erflag         !erosion flag
      
      integer :: CN_index                       !Current index for ereosion and runoff parameters, current one being used
      real    :: N1                             !The current mannings n value
      real    :: CFAC                           !The current c factor
      real    :: USLEC(Num_hydro_factors)    !inpuit values for c facator
      real    :: MNGN(Num_hydro_factors)      
      
      integer :: use_usleyears= 0               !flag to use year specific or repeating erosion/runoff factors, 1 is year specific
      integer :: NUSLEC                                    !number of c factors
      integer :: GDUSLEC(Num_hydro_factors)  !day, 
      integer :: GMUSLEC(Num_hydro_factors)  !month
      integer :: GYUSLEC(Num_hydro_factors)  !year when this option is used
      
      
      
      integer :: JUSLEC(Num_hydro_factors)   !julian day for each of the erosion days converted from gduslec and gmuslec
      

      real :: CN_2(Num_hydro_factors) !Curve Number 2 read from inputs, Now a Real number ( was an integer because its used as index in lookup table )    
      
      
      
      integer :: IREG        !erosion/rain intensity map region
      
      
      !******Hydrology*********
      real :: STTDET              !evaporation amount in the top 5 cm, used for temperature calculations 
      REAL :: CEVAP               !HOLDS ACTUAL PET
      real :: SMELT               !snow melt     
      real :: CN_moisture_ref     !water content halfway between wp and fc averaged over cn depth
      real :: INABS
      real :: Infiltration_Out   !Infiltration out of last compartment, this info was previously not captured by PRZM
      integer :: cn_moist_node   !number of compartments that make up runff moisture depth
      
      
      !real :: THETH    dfy renamed CN_moisture_ref  8/24/17          !water content halfway between wp and fc averaged over cn depth (not sure of purpose)
      
      !***Accumulators***************

      real, allocatable,dimension(:)   :: ainf        !infiltration INTO compartment(needs to be dimensioned as ncom2+1 to capture outflow)
      real, allocatable,dimension(:)   :: vel         !velocity out of compartment = ainf(i+1)/theta
      real, allocatable,dimension(:)   :: EvapoTran
      
      real, allocatable,dimension(:,:) :: GAMMA1   

      real :: runof                           = 0.0   
      real :: snow                            = 0.0   !accumulated snow for each day
      real :: foliar_degrade_loss(3)          = 0.0   !foliar loss by degradation
      real :: FOLPST(3)                       = 0.0   !foliar storage
      real :: DCOFLX(3)                       = 0.0   !Pesticide outflow below soil core
      real :: SUPFLX(3)                       = 0.0 
      real :: SDKFLX(3)                       = 0.0 
      real :: Foliar_volatile_loss(3)         = 0.0    !volatilization

      real :: potential_canopy_holdup  = 0.0    !daily maximum water that the canopy can hold
      real :: CINT             = 0.0    !the actual water held on the canopy during the current day
      real :: TDET             = 0.0    !evapotranspiration in the topp ncom1 compartments
      
      real :: SEDL
   



!      integer :: MTR1  !some kind of comaprtment count = ncom2
      
      
      !****Miscelaneous******

      
      !CHARACTER(len=78) ::  TITLE     !INPUTused in EXAMS output files and time series
      CHARACTER(len=20) ::  PSTNAM(3) !INPUT used in EXAMS

      !integer :: current_day        !day of month
      !integer :: current_month
      !integer :: current_year       !taken from metfile
              
      integer  :: CFLAG                 !INPUT initial pesticide concentration conversion flag
      
      character(len=4)  :: MODE(max_number_plots)      !INPUT time series plot modes
      character(len=4)  :: PLNAME(max_number_plots)    !INPUT Plot IDs for time series
      integer           :: ARG(max_number_plots)      !Formerly IARG, INPUT arguments for time series plots
      integer           :: IARG2(max_number_plots)      !INPUT arguments for time series plots
      
      integer           :: NPLOTS                       !INPUT number of time series plots
      real              :: CONST(max_number_plots)             !INPUT, the user defined adjustment to the time series output
      real              :: OUTPUJ(max_number_plots)
      
      
      real              :: OUTPJJ(max_number_plots)  !output accumulator
      integer           :: chem_id(max_number_plots)    !plot characterizer chem 1 2 3 
      
      REAL   ::  curve_number_daily   !holds curve number for plotting
             
      !***** PRZM5 runoff and erosion extraction variables ****************
      real :: runoff_effic              !amount of runoff bypassing surface soil
      real :: runoff_decline            !exponential factor for interaction decline with depth
      real :: runoff_extr_depth         !depth of runoff interaction
      real, allocatable,dimension(:) :: runoff_intensity  !PRZM5 uses this instead of DRI, fraction of runoff per cm depth

      real :: erosion_effic             !propotional reduction in erosion intensity
      real :: erosion_decline           !exponential factor for interaction decline with depth
      real :: erosion_depth             !erosion depth
      real, allocatable,dimension(:) :: erosion_intensity
      
      integer :: RNCMPT             !the number of compartments corresponding to the runoff extraction depth
      integer :: erosion_compt      !number of compartments for rerosion extraction
      

end module constants_and_variables