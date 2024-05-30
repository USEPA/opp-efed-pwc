module variables
!Only variables that are imported in from the User Interface are kept in this module.  This rule is 
!intended to facilititate changing outthe interface, as only and all the parameters in this module need to be populated.
!Common variables that are calculated and used internally between routines are transported as arguments.

    implicit none
    save
    real, parameter :: Version_number = 1.032
    
    real, parameter :: DELT = 86400.  !seconds, simulation TIME INTERVAL IS ONE DAY
    real, parameter :: wind_height= 6.     !height at which wind measurements are reported (m)
    
    logical :: is_minimum_batch_output
    
    !**********************************************************
    character(len=256) :: metfilename    !met file name
    character(len=256) :: outputfilename !output file name
    character(len=50)  ::  scenario_id
    character(len=10)  :: waterbodytext  !text for water body type (not an input)
    !***********chemical inputs*****************************
    integer :: nchem          ! nchem = 1 parent only, =2 parent and 1 degradate, =3 parent and 2 degradates
    
    real :: aer_aq_all(3)         !input aqueous-phase aerobic halflife (days) delivered from window interface
    real :: temp_ref_aer_all(3)   !temp at which aerobic study conducted  
    real :: anae_aq_all(3)        !anaerobic aquatic
    real :: temp_ref_anae_all(3)  !temp at which anaerobic study conducted    
    real :: QT                    !EXAMS Q10 values eq. 2-133
    
    real ::   latitude !of scenario, input for new files, internal database for old files
    character(len=6) :: Text_latitude
    
    real :: photo_all(3)          !near-surface photolysis half life (days
    real :: RFLAT_all(3)          !input latitude for photolysis study   
    real :: hydro_all(3)          !input hydrolysis half life (days)
    real :: koc_all(3)            !Koc value (ml/g)

    real :: MWT_all(3)            !molecular wt (g/mol)
    real :: VAPR_all(3)           !vapor pressure (torr)
    real :: SOL_all(3)            !solubility (mg/L)   
    real :: Henry_unitless(3)     !Henry's Constant ConcAir/ConcWater volumetric   
    real :: Heat_of_Henry(3)      !Enthalpy of air water exchange (J/mol)

    integer :: napp               !number of applications per year
    integer :: appday(50) 
    integer :: appmon(50) 
    real :: apprate(50)    !kg/ha
   ! real :: spray(50)      !Fraction of application to be applied to waterbody area

    integer,allocatable,dimension(:) :: appdate_sim_ref  !simulation referenced app dates (first sim day = 1)
    real,allocatable,dimension(:) :: spray  !total spray mass into water column dates with appdate_sim_ref(first sim day = 1)
  
    
    
    real :: afield         !area of adjacent runoff-producing field(m2)    
    real :: area           !water body area (m2)     
    real :: depth_0        !initial water body depth (m)
    real :: depth_max      !maximum water body depth (m)
    
    integer :: SimTypeFlag    !1=vvwm,2 = USepa pond, 3 = usepa reservoir, 4=constant vol w/o flow, 5 = const vol w/flow
    logical :: burialflag    !true = burial, else no burial

    real:: D_over_dx 

    real:: benthic_depth
    real:: porosity     !volume water/total volume
    real:: bulk_density  !dry mass/total volume  g/ml
    real:: FROC2
    real:: DOC2
    real:: BNMAS
    real:: DFAC
    real:: SUSED   !mg/L
    real:: CHL
    real:: FROC1
    real:: DOC1
    real:: PLMAS
    
    
    real:: PRBEN
    logical :: is_calc_prben
    
    
    real,parameter :: CLOUD = 0.
    real, parameter :: minimum_depth = 0.00001     !minimum water body depth

    
    real :: xAerobic(2)
    real :: xBenthic(2)
    real :: xPhoto(2)
    real :: xHydro(2)
 
    integer :: flow_averaging                
    real :: baseflow !m3/sec
    real :: cropped_fraction !fractional area used for crop: only used here to display in output
    
    logical :: is_hed_files_made
    logical :: is_add_return_frequency
    real    :: additional_return_frequency
    

   character(len= 256) :: outputfile_parent_daily
   character(len= 256) :: outputfile_deg1_daily
   character(len= 256) :: outputfile_deg2_daily
            
   character(len= 256) :: outputfile_parent_analysis
   character(len= 256) :: outputfile_deg1_analysis
   character(len= 256) :: outputfile_deg2_analysis
               
   character(len= 256) :: outputfile_parent_deem
   character(len= 256) :: outputfile_deg1_deem
   character(len= 256) :: outputfile_deg2_deem
         
   character(len= 256) :: outputfile_parent_calendex
   character(len= 256) :: outputfile_deg1_calendex
   character(len= 256) :: outputfile_deg2_calendex
               
   character(len= 256) :: outputfile_parent_esa
   character(len= 256) :: outputfile_deg1_esa
   character(len= 256) :: outputfile_deg2_esa
       
   character(len= 256) :: Batch_outputfile
    
   character(len= 256) :: run_id
    
    
    
    
    
    
    
    
    
    
end module variables