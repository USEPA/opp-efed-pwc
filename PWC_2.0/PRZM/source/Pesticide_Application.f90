module Pesticide_Applications
implicit none
    contains

     
Subroutine adjust_application_dates_for_weather
!Start with given application date. Check to see if a rain event of size "rain_limit" occurs within
! the "intolerable_rain_window", check alternately forward and backward one day at a time to find a date that
! has only less than the limit during that intolerable window. 
! If no date is found, then the original date is retained.

  use  constants_and_Variables, ONLY: application_date, precip, num_applications, startday , &
  rain_limit, optimum_application_window,intolerable_rain_window,min_days_between_apps, EchoFileUnit,num_records
  
  use utilities
  implicit none
  

  
  
  integer :: i, j, YEAR,MONTH,DAY
  integer :: day_initial
  integer :: m, n , k
  integer :: previous_application_date 
  character(len = 20) :: overide
  
  previous_application_date = -1000 !arbitrary low number
  do i = 1 , num_applications  
      day_initial = application_date(i)-startday+1       
      do j = 0, optimum_application_window 
          k = (-1)**(j+1) *( j/2+ (1-(-1)**j)/2) !Alternating sign and increasing magnitude index 0, 1, -1, 2,-2, 3,-3...
          
          m = day_initial + k
          n = day_initial+intolerable_rain_window +k 
          m =max(1,m)
          n= min(n,num_records)
          
          call get_date (m +startday-1, YEAR,MONTH,DAY) 
          
         
          if (any(precip(m:n) > rain_limit) .eqv. .FALSE. )  then    
              application_date(i) = application_date(i) +k         
              if (application_date(i) - previous_application_date < min_days_between_apps) cycle
              exit
          end if      
          
          
          
      end do  
   
     
     overide = ""
     if (application_date(i) - previous_application_date < min_days_between_apps) then
         application_date(i) = previous_application_date + min_days_between_apps
         overide = "minimum overide"       
     end if
     
     
     call get_date(application_date(i) , YEAR,MONTH,DAY)  
     
     
      write(EchoFileUnit,'("actual application day (YMD) = ", I4,1x, I2,1x ,I2, ", added days =", I3,  ", " , A20)') YEAR,MONTH,DAY, k, overide
      
      previous_application_date =  application_date(i)  !reset app date to be used for check with minimum interval
  end do



end subroutine adjust_application_dates_for_weather
    
    
    
       
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
SUBROUTINE PESTAP(application_index)
      !Loads conc_porewater and conc_total_per_water into parameter module after pesticide application

      use  constants_and_Variables, ONLY: COVER,CAM,  &
                                    delx,theta_zero,DEPI,TAPP,appeff, &
                                    conc_total_per_water,mass_in_compartment, &
                                    SOILAP,FOLPST,plant_app,Tband_top, ncom2
      use utilities
      implicit none
      
      
    !K is the chenmical ID Number
      ! Computes amount and location of pesticide application (foliage, soil surface, or soil layer)

      !NOTE: IFSCND is set to 0 after each foliar application, but left untouched after ground application, Why?
      
      
      REAL               ::  DMAX,SLOPE,BASE
      real               ::  appamt          !g/cm2 actual amount pesticide applied to soil (adjusted for efficiency and canopy)
      integer            ::  CAM_local 
      integer,intent(in) ::  application_index
      real               :: distribution_of_applied(ncom2)
      

      
      plant_app = 0.0
      SOILAP = 0.0
      CAM_local = CAM(application_index)
      distribution_of_applied = 0.0
      
 
      
      
      select case (CAM_local)
      case (1)  !Soil surface pesticide application
        plant_app= 0.
        APPAMT = TAPP(application_index)*APPEFF(application_index)    !g/cm2
        !Solve for base of isosceles triangle given: length of 1 leg and area = 1.0
        DMAX = 4.0
        BASE=2./DMAX
        SLOPE=(-BASE/DMAX) 
        call pesticide_decreasing_distribution(APPAMT,DMAX,BASE,SLOPE, distribution_of_applied) 
  
      case (2) ! Linear foliar pesticide application
        !IF (DELX(1) .GE. 2.0) THEN !if the del x is greater than 2 cm, just distribute it in the top comnpartment
        !  plant_app    = COVER * TAPP(application_index) * APPEFF(application_index)
        !  SOILAP(1,1)  = (1.0- COVER) * TAPP(application_index)*APPEFF(application_index)
        !
        !ELSE
          plant_app    = COVER * TAPP(application_index) * APPEFF(application_index)
          APPAMT       = (1.0- COVER) * TAPP(application_index)*APPEFF(application_index)
          !Solve for base of isosceles triangle given: length of 1 leg and area = 1.0   
          DMAX = 4.0
          BASE=2./DMAX
          SLOPE=(-BASE/DMAX)
          call pesticide_decreasing_distribution(APPAMT,DMAX,BASE,SLOPE,distribution_of_applied)
        !ENDIF
        
        FOLPST(1)    = FOLPST(1)+ plant_app


      case (4) !Uniform  Incorporated pesticide application method 1
        plant_app= 0.
        APPAMT = TAPP(application_index)*APPEFF(application_index)
        DMAX = DEPI(application_index)
        IF(DEPI(application_index).EQ.0.0)THEN
          SOILAP(1,1)  = TAPP(application_index)*APPEFF(application_index)
        ELSE
          call pesticide_uniform_distribution(APPAMT,DMAX,distribution_of_applied) 
        ENDIF
        
      case (5) ! Incorporated increasing with depth pesticide application 
        plant_app= 0.
        APPAMT = TAPP(application_index)*APPEFF(application_index)
!       Solve for base of isosceles triangle given:length of 1 leg and area = 1.0
        DMAX = DEPI(application_index)
        BASE=2./DMAX
        SLOPE=(-BASE/DMAX)
        IF(DEPI(application_index) == 0.0)THEN
          distribution_of_applied(1)  = TAPP(application_index)*APPEFF(application_index)
        ELSE
          CALL pesticide_increasing_distribution(APPAMT,DMAX,BASE,SLOPE,distribution_of_applied)
        ENDIF

      case (6) !**** linearly decreasing with depth *****
        plant_app= 0.
        APPAMT = TAPP(application_index)*APPEFF(application_index)
        !  Solve for base of isosceles triangle given: length of 1 leg and area = 1.0
        DMAX = DEPI(application_index)
        BASE=2./DMAX
        SLOPE=(-BASE/DMAX)
        IF(DEPI(application_index).EQ.0.0)THEN
          distribution_of_applied(1)  = TAPP(application_index)*APPEFF(application_index)
        ELSE
          call pesticide_decreasing_distribution(APPAMT,DMAX,BASE,SLOPE, distribution_of_applied) 
        ENDIF
        
      case (7)  !**** T-Band *************************
        plant_app= 0.
        APPAMT = TAPP(application_index)*APPEFF(application_index)
        DMAX = DEPI(application_index)
        IF(DEPI(application_index).EQ.0.0)THEN
          distribution_of_applied(1)  = TAPP(application_index)*APPEFF(application_index)
        ELSE
          CALL pesticide_Tband_distribution(APPAMT,DMAX, Tband_top(application_index),distribution_of_applied)
        ENDIF
        
      case(8)
        plant_app= 0.
        APPAMT = TAPP(application_index)*APPEFF(application_index)
        !Solve for base of isosceles triangle given:length of 1 leg and area = 1.0
        DMAX = DEPI(application_index)
        BASE=0.
        SLOPE=0.
        IF(DEPI(application_index).EQ.0.0)THEN
          distribution_of_applied(1)  = TAPP(application_index)*APPEFF(application_index)
        ELSE
          CALL pesticide_atdepth_distribution(APPAMT,DMAX,distribution_of_applied)
        ENDIF
        
      case (9)
        !IF (DELX(1) .GE. 2.0) THEN
        !  plant_app    = COVER * TAPP(application_index) * APPEFF(application_index)
        !  distribution_of_applied(1)  = (1.0- COVER) * TAPP(application_index)*APPEFF(application_index)
        !ELSE
          plant_app    = COVER * TAPP(application_index) * APPEFF(application_index)
          APPAMT       = (1.0- COVER) * TAPP(application_index)*APPEFF(application_index)
          !Solve for base of isosceles triangle given:length of 1 leg and area = 1.0     
          IF(DEPI(application_index).GT.0.0)THEN
             DMAX = DEPI(application_index)
          ELSE
             DMAX = 4.0
          ENDIF
          BASE=2./DMAX
          SLOPE=(-BASE/DMAX)
          IF(DEPI(application_index).EQ.0.0)THEN
            distribution_of_applied(1)  = TAPP(application_index)*APPEFF(application_index)
          ELSE
            call pesticide_decreasing_distribution(APPAMT,DMAX,BASE,SLOPE,distribution_of_applied)
          ENDIF
        !ENDIF
        FOLPST(1)    = FOLPST(1)+ plant_app



      end select
    
     !Calculate Soil Pesticide Mass as a Total Concentrations (Total Mass/Water-- kind of odd to use per water):

     conc_total_per_water(1,:) = conc_total_per_water(1,:)   +   distribution_of_applied/(delx*theta_zero)
     mass_in_compartment(1,:)  = mass_in_compartment(1,:)    +   distribution_of_applied 

END SUBROUTINE PESTAP



!*********************************************************************************************************
SUBROUTINE plant_pesticide_harvest_application

      use  constants_and_Variables, ONLY: FOLPST, theta_zero,delx, conc_total_per_water, &
                                          mass_in_compartment, harvest_placement,ncom2, nchem
      use utilities
      implicit none

      !Determines amount of pesticide which disappears from plant surface by first order decay.  The variable PLDKRT
      !is a pseudo first order decay rate which may include processes of volatilization, oxidation, photolysis, etc.
      !also determines pesticide washed off during rainfall events.

      INTEGER:: K

      REAL         DMAX,SLOPE,BASE
      real   ::    harvested_to_soil(3, ncom2)
      real   ::    unit_distribution (ncom2)

      harvested_to_soil=0.0
       
      select case (harvest_placement)
         case (1) !pesticide is applied to the gound in a linearly decreasing manner to 4 cm
          
              DMAX = 4.0
              BASE=2./DMAX
              SLOPE=(-BASE/DMAX)
              call pesticide_decreasing_distribution(1.0,DMAX,BASE,SLOPE,  unit_distribution) 
              
              do k=1, nchem
                  
                    harvested_to_soil(k,:) =   unit_distribution* FOLPST(k)
                    conc_total_per_water(k,:) = conc_total_per_water(k,:)   +    harvested_to_soil(k,:)/(delx*theta_zero)
                    mass_in_compartment(k,:)  = mass_in_compartment(k,:)    +    harvested_to_soil(k,:)
              end do
              
              FOLPST = 0.0 
              
          case (2)  !pesticide is removed
              FOLPST = 0.0   
          case (3)  !pesticide remains on the plant (ghost plant)
          case default
       
      end select
      
end subroutine plant_pesticide_harvest_application



!==========================================================================================================================================
    !SUBROUTINE pesticide_decreasing_distribution(APPAMT,DMAX,BASE,SLOPE,applied_to_soil )
    !  use  constants_and_Variables, ONLY: delx, ncom2
    !  implicit none
    !  ! This routine distributes a chemical application down to an input depth decreasing proportionally with depth.
    !  ! Used for CAM 1 and 6 and Harvest
    !  
    !  ! APPAMT - total amount of chemical to distribute in the soil
    !  ! CHEM   - chemical id number (1-3)
    !  ! DMAX   - depth to which chemical should be applied;
    !  ! BASE   - initial starting amount of application
    !  ! SLOPE  - slope of linearly decreasing application
    !
    !  !OUTPUT IS SOILAP the distribution of applied pesticide in the profile
    !  
    !
    !  REAL,intent(in)  :: APPAMT,DMAX,BASE,SLOPE
    !  real,intent(out) :: applied_to_soil(ncom2)
    !   
    !  INTEGER  CMPT
    !  REAL     DEP,APPTOT,APPREM,FRACT,SLP2,FRCTOT    ! 
    !  
    !  FRCTOT=0.0
    !  SLP2= 0.
    !  CMPT= 0
    !  DEP = 0.0
    !  APPTOT=0.0
    !  APPREM=APPAMT
    !  applied_to_soil = 0.0
    !  
    !  
    !
    !  do 
    !    CMPT= CMPT + 1
    !    FRACT=(SLOPE*(DEP+DELX(CMPT)/2.)+BASE)*DELX(CMPT)
    !    FRCTOT=FRCTOT+FRACT
    !    IF(FRCTOT.GT.1.00)FRACT=FRACT-(1.0-FRCTOT)
    !    applied_to_soil(CMPT)=AMIN1(APPREM,APPAMT*FRACT)
    !    APPTOT=APPTOT+applied_to_soil(CMPT)
    !    APPREM=APPAMT-APPTOT
    !    DEP=DEP+delx(CMPT)
    !    if (DEP >= (DMAX-1.E-4)) exit
    !  end do
    !   
    !END SUBROUTINE pesticide_decreasing_distribution
!==============================================================================================================================
!##############################################################################################################################
SUBROUTINE pesticide_decreasing_distribution(APPAMT,DMAX,BASE,SLOPE,applied_to_soil )
    
!Updated by Houbou with original changes propoposed by Ian Kennedy
    use  constants_and_Variables, ONLY: delx, ncom2
    implicit none
      ! This routine distributes a chemical application down to an input depth decreasing proportionally with depth.
      ! Used for CAM 1 and 6 and Harvest
      
      ! APPAMT - total amount of chemical to distribute in the soil
      ! CHEM   - chemical id number (1-3)
      ! DMAX   - depth to which chemical should be applied;
      ! BASE   - initial starting amount of application
      ! SLOPE  - slope of linearly decreasing application
      !OUTPUT IS SOILAP the distribution of applied pesticide in the profile
      

    REAL,intent(in)  :: APPAMT,DMAX,BASE,SLOPE
    real,intent(out) :: applied_to_soil(ncom2)

    INTEGER  CMPT, ncmpwithchem, i
    REAL     DEP,APPTOT,APPREM,FRACT,SLP2,FRCTOT, prev, curr, bottom

    DEP = 0.0
    applied_to_soil = 0.0
    prev = 0.0
    bottom = 0.0

    do ncmpwithchem = 1, ncom2 ! Count the number of compartments that will have chemical added
        DEP = DEP + delx(ncmpwithchem)
        if (DEP >= (DMAX-1E-7)) exit
    end do

    do i=1, ncmpwithchem
        bottom = min(bottom + delx(i), DMAX)
        curr = bottom*bottom*SLOPE/2 + BASE*bottom
        applied_to_soil(i) = (curr - prev) * APPAMT
        prev = curr
    end do
END SUBROUTINE pesticide_decreasing_distribution
!#################################################################################################################################


!=================================================================================================================================
    !
    !SUBROUTINE pesticide_uniform_distribution(APPAMT,DMAX,applied_to_soil)
    !   use  constants_and_Variables, ONLY: DELX,ncom2
    !   implicit none
    !  ! This routine distributes a chemical application uniformly to depth dmax.
    !  ! Used for CAM 1 ans 6 and Harvest
    !  
    !  ! APPAMT - total amount of chemical to distribute in the soil
    !  ! CHEM   - chemical id number (1-3)
    !  ! DMAX   - depth to which chemical should be applied;
    !
    !  real,intent(out) :: applied_to_soil(ncom2)
    !  REAL     APPAMT,DMAX
    !  INTEGER  CMPT
    !  REAL     DEP,APPTOT,APPREM,FRACT,SLP2,FRCTOT
    !
    !  FRCTOT=0.0
    !  SLP2= 0.
    !  CMPT= 0
    !  DEP = 0.0
    !  APPTOT=0.0
    !  APPREM=APPAMT
    !  applied_to_soil = 0.0
    !
    !  do       
    !    CMPT= CMPT + 1
    !    FRACT=DELX(CMPT)/DMAX
    !    FRCTOT=FRCTOT+FRACT
    !    IF(FRCTOT.GT.1.00)FRACT =FRACT-(1.0-FRCTOT)
    !    applied_to_soil(CMPT) = AMIN1(APPREM,APPAMT*FRACT)
    !    APPTOT= APPTOT+applied_to_soil(CMPT)
    !    APPREM=APPAMT-APPTOT
    !    DEP=DEP+DELX(CMPT)
    !    if (DEP >= DMAX) exit
    !  end do  
    !   
    !END SUBROUTINE pesticide_uniform_distribution
!======================================================================================================================================
!######################################################################################################################################
SUBROUTINE pesticide_uniform_distribution(APPAMT,DMAX,applied_to_soil)
     use  constants_and_Variables, ONLY: DELX,ncom2
     implicit none
      ! This routine distributes a chemical application uniformly to depth dmax.
      ! Used for CAM 1 ans 6 and Harvest
      
      ! APPAMT - total amount of chemical to distribute in the soil
      ! CHEM   - chemical id number (1-3)
      ! DMAX   - depth to which chemical should be applied;
      
    REAL,intent(in)  :: APPAMT,DMAX
    real,intent(out) :: applied_to_soil(ncom2)
       
    INTEGER  CMPT, ncmpwithchem, i
    REAL     DEP,APPTOT,APPREM,FRACT,SLP2,FRCTOT, prev, curr, bottom
      
    DEP = 0.0
    applied_to_soil = 0.0
    prev = 0.0
    bottom = 0.0

    do ncmpwithchem=1, ncom2 ! Count the number of compartments that will have chemical added
        DEP = DEP + delx(ncmpwithchem)
        if (DEP >= (DMAX-1E-7)) exit
    end do

    do i=1, ncmpwithchem
        bottom = min(bottom + delx(i), DMAX)
        curr = bottom/DMAX
        applied_to_soil(i) = (curr - prev) * APPAMT
        prev = curr
        
    end do  
  
END SUBROUTINE pesticide_uniform_distribution
!########################################################################################################################################


!========================================================================================================================================
    !
    !SUBROUTINE pesticide_increasing_distribution(APPAMT,DMAX,BASE,SLOPE,applied_to_soil)
    !  use  constants_and_Variables, ONLY: delx,ncom2
    !  implicit none
    !  !This routine distributes a chemical application down to an input depth increasing proportionally with depth.
    !  
    !  !APPAMT - total amount of chemical to distribute in the soil
    !  !CHEM   - chemical id number (1-3)
    !  !DMAX   - depth to which chemical should be applied;
    !  !BASE   - initial starting amount of application
    !  !SLOPE  - slope of linearly decreasing application
    !
    !  real,intent(out) :: applied_to_soil(ncom2)
    !  REAL     APPAMT,DMAX,BASE,SLOPE
    !  INTEGER  CMPT
    !  REAL     DEP,APPTOT,APPREM,FRACT,SLP2,FRCTOT
    !
    !  FRCTOT=0.0
    !  SLP2= 0.
    !  CMPT= 0
    !  DEP = 0.0
    !  APPTOT=0.0
    !  APPREM=APPAMT
    !  applied_to_soil = 0.0
    !  
    !  do     
    !    CMPT= CMPT + 1
    !    DEP=DEP+DELX(CMPT)        
    !    FRACT=(SLOPE*((DMAX-DEP)+DELX(CMPT)/2.)+BASE)*DELX(CMPT)
    !    FRCTOT=FRCTOT+FRACT
    !    IF(FRCTOT.GT.1.00)FRACT=FRACT-(1.0-FRCTOT)
    !    applied_to_soil(CMPT)=AMIN1(APPREM,APPAMT*FRACT)
    !    APPTOT=APPTOT+applied_to_soil(CMPT)
    !    APPREM=APPAMT-APPTOT
    !    IF(DEP>=(DMAX-1.E-4)) exit
    !  end do
    !
    !END SUBROUTINE pesticide_increasing_distribution
    !
!======================================================================================================================================       
!#######################################################################################################################################     
SUBROUTINE pesticide_increasing_distribution(APPAMT,DMAX,BASE,SLOPE,applied_to_soil)
      use  constants_and_Variables, ONLY: delx,ncom2
      implicit none

      REAL,intent(in)  :: APPAMT,DMAX,BASE,SLOPE
      real,intent(out) :: applied_to_soil(ncom2)
       
      INTEGER  CMPT, ncmpwithchem, i
      REAL     DEP,APPTOT,APPREM,FRACT,SLP2,FRCTOT, prev, curr, bottom
      
      DEP = 0.0      
      applied_to_soil = 0.0
      prev = 0.0
      bottom = 0.0

      do ncmpwithchem=1, ncom2 ! Count the number of compartments that will have chemical added
          DEP = DEP + delx(ncmpwithchem)
          if (DEP >= (DMAX-1E-7)) exit
      end do

      do i=1, ncmpwithchem
          bottom = min(bottom + delx(i), DMAX)
          curr = 1 - ((DMAX-bottom)*(DMAX-bottom)*SLOPE/2 + BASE*(DMAX-bottom))
          applied_to_soil(i) = (curr - prev) * APPAMT
          prev = curr
      end do      

END SUBROUTINE pesticide_increasing_distribution 
!#####################################################################################################################################


!===================================================================================================================================== 
    !SUBROUTINE pesticide_Tband_distribution(APPAMT,DMAX,top2cm,applied_to_soil)
    !  use  constants_and_Variables, ONLY: DELX,ncom2
    !  implicit none    
    !
    !  real,intent(out) :: applied_to_soil(ncom2)
    !  REAL     APPAMT,DMAX
    !  INTEGER  CMPT
    !  REAL     depth,APPTOT,APPREM,SLP2,FRCTOT
    !  real,intent(in) :: top2cm
    !  
    !  
    !  FRCTOT=0.0
    !  SLP2= 0.
    !  CMPT= 0
    !  depth = 0.0
    !  APPTOT=0.0
    !  APPREM = APPAMT
    !  applied_to_soil = 0.0
    !  
    !   do
    !     CMPT= CMPT + 1
    !     depth = depth + DELX(CMPT)
    !     SLP2=DELX(CMPT)/2.
    !     IF(SLP2 > 1.)  SLP2=1.0
    !     applied_to_soil(CMPT) = AMIN1(APPREM,(SLP2*APPAMT*top2cm))
    !     APPTOT=APPTOT+applied_to_soil(CMPT)
    !     APPREM=(APPAMT*top2cm)-APPTOT
    !     write(46,*) CMPT, applied_to_soil(CMPT)
    !     if (depth >= 1.95) exit
    !     
    !  
    !   end do 
    !  
    !   do  
    !      IF((depth.GT.1.95).AND.(depth.LT.2.05))APPREM=APPAMT-APPTOT
    !      CMPT= CMPT + 1
    !      depth = depth+DELX(CMPT)
    !      SLP2=DELX(CMPT)/(DMAX-2.)
    !      IF(SLP2.GT.1.)SLP2=1.0
    !    !  SOILAP(CHEM,CMPT)=AMIN1(APPREM,(SLP2*APPAMT*(1.0-DRFT(CHEM,NAPPC))))
    !      applied_to_soil(CMPT)=AMIN1(APPREM,(SLP2*APPAMT*(1.0-top2cm)))
    !      APPTOT=APPTOT + applied_to_soil(CMPT)
    !      APPREM=APPAMT-APPTOT
    !      write(46,*) CMPT, applied_to_soil(CMPT)
    !      if (depth >= (DMAX-1.E-4)) exit
    !      
    !  
    !  
    !   end do          
    !
    !END SUBROUTINE pesticide_Tband_distribution   
!===========================================================================================================================================
!###########################################################################################################################################
SUBROUTINE pesticide_Tband_distribution(APPAMT,DMAX,top2cm,applied_to_soil) 
    use  constants_and_Variables, ONLY: DELX,ncom2
    implicit none
      !This routine distributes in a T Band: Part in the top 2 cm and the remainder uniformly distributed, 
      
      !APPAMT - total amount of chemical to distribute in the soil
      !CHEM   - chemical id number (1-3)
      !DMAX   - depth to which chemical should be applied;
      
    real,intent(out) :: applied_to_soil(ncom2)
    REAL     APPAMT,DMAX
    INTEGER  CMPT,ncmpwithchem,i
    REAL     depth,APPTOT,APPREM,SLP2,FRCTOT,DEP,prev,curr,bottom
    real,intent(in) :: top2cm        
      
    DEP = 0.0
    APPTOT=0.0
    APPREM = APPAMT
    applied_to_soil = 0.0
    prev=0.0
    bottom=0.0
    curr=0.0
      
    do ncmpwithchem=1, ncom2 ! Count the number of compartments that will have chemical added
        DEP = DEP + delx(ncmpwithchem)
        if (DEP >= (DMAX-1E-7)) exit
    end do

    do i=1, ncmpwithchem
        bottom = min(bottom + delx(i), DMAX)
        if (bottom <= 2.0) then
            curr = (bottom/2.0)*APPAMT * top2cm
            applied_to_soil(i) = (curr - prev)
        else
            curr = APPAMT * top2cm +((bottom-2.0)/(DMAX-2.0)) * APPAMT *(1-top2cm)
            applied_to_soil(i) = (curr - prev)
        end if
        prev = curr 
    end do
      
END SUBROUTINE pesticide_Tband_distribution
!#######################################################################################################################################



!======================================================================================================================================= 
!SUBROUTINE pesticide_atdepth_distribution(APPAMT,DMAX,applied_to_soil)
!    use constants_and_variables, ONLY: DELX,ncom2
!    implicit none
!          !Places all pesticide at a specfic depth
!          !APPAMT - total amount of chemical to distribute in the soil
!          !CHEM   - chemical id number (1-3)          
!          
!    real,intent(out)   :: applied_to_soil(ncom2)
!    REAL,intent(in)    ::  APPAMT,DMAX
!    INTEGER  CMPT
!    REAL     DEP
!          
!    CMPT= 0
!    DEP = 0.0
!    applied_to_soil = 0.0
!          
!    do
!        CMPT= CMPT + 1
!        DEP=DEP+DELX(CMPT)
!        IF(DEP >= (DMAX-1.E-4)) exit
!    end do     
!          
!    applied_to_soil(CMPT)=APPAMT
!          
!END SUBROUTINE pesticide_atdepth_distribution  
!=====================================================================================================================================
!######################################################################################################################################
SUBROUTINE pesticide_atdepth_distribution(APPAMT,DMAX,applied_to_soil)
    use constants_and_variables, ONLY: DELX,ncom2
    implicit none
    real,intent(out) :: applied_to_soil(ncom2)
    REAL     APPAMT,DMAX
    INTEGER  CMPT,ncmpwithchem,i
    REAL     DEP,bottom     
      
    DEP = 0.0
    applied_to_soil = 0.0
    bottom=0.0
    CMPT = 1
    
    
    do ncmpwithchem=1, ncom2 ! Count the number of compartments that will have chemical added
        DEP = DEP + delx(ncmpwithchem)
        if (DEP >= (DMAX-1E-7)) exit
    end do
    
    do i=1, ncmpwithchem
        bottom = min(bottom + delx(i), DMAX)
        if (bottom >= DMAX) exit
        CMPT = i
    end do 
    applied_to_soil(CMPT) = APPAMT

END SUBROUTINE pesticide_atdepth_distribution
!##########################################################################################################################################    

  
end module Pesticide_Applications 
    
    
    
    
!module Pesticide_Applications
!implicit none
!contains
!    
!SUBROUTINE PESTAP(application_index)
!      !Loads conc_porewater and conc_total_per_water into parameter module after pesticide application
!
!      use  constants_and_Variables, ONLY: COVER,CAM,  &
!                                    delx,theta_zero,DEPI,TAPP,appeff, &
!                                    conc_total_per_water,mass_in_compartment, &
!                                    SOILAP,FOLPST,plant_app,Tband_top, ncom2
!      use utilities
!      implicit none
!      
!      
!    !K is the chenmical ID Number
!      ! Computes amount and location of pesticide application (foliage, soil surface, or soil layer)
!
!      !NOTE: IFSCND is set to 0 after each foliar application, but left untouched after ground application, Why?
!      
!      
!      REAL               ::  DMAX,SLOPE,BASE
!      real               ::  appamt          !g/cm2 actual amount pesticide applied to soil (adjusted for efficiency and canopy)
!      integer            ::  CAM_local 
!      integer,intent(in) ::  application_index
!      real               :: distribution_of_applied(ncom2)
!      
!
!      
!      plant_app = 0.0
!      SOILAP = 0.0
!      CAM_local = CAM(application_index)
!      distribution_of_applied = 0.0
!      
! 
!      
!      
!      select case (CAM_local)
!      case (1)  !Soil surface pesticide application
!        plant_app= 0.
!        APPAMT = TAPP(application_index)*APPEFF(application_index)    !g/cm2
!        !Solve for base of isosceles triangle given: length of 1 leg and area = 1.0
!        DMAX = 4.0
!        BASE=2./DMAX
!        SLOPE=(-BASE/DMAX) 
!        call pesticide_decreasing_distribution(APPAMT,DMAX,BASE,SLOPE, distribution_of_applied) 
!  
!      case (2) ! Linear foliar pesticide application
!        !IF (DELX(1) .GE. 2.0) THEN !if the del x is greater than 2 cm, just distribute it in the top comnpartment
!        !  plant_app    = COVER * TAPP(application_index) * APPEFF(application_index)
!        !  SOILAP(1,1)  = (1.0- COVER) * TAPP(application_index)*APPEFF(application_index)
!        !
!        !ELSE
!          plant_app    = COVER * TAPP(application_index) * APPEFF(application_index)
!          APPAMT       = (1.0- COVER) * TAPP(application_index)*APPEFF(application_index)
!          !Solve for base of isosceles triangle given: length of 1 leg and area = 1.0   
!          DMAX = 4.0
!          BASE=2./DMAX
!          SLOPE=(-BASE/DMAX)
!          call pesticide_decreasing_distribution(APPAMT,DMAX,BASE,SLOPE,distribution_of_applied)
!        !ENDIF
!        
!        FOLPST(1)    = FOLPST(1)+ plant_app
!
!
!      case (4) !Uniform  Incorporated pesticide application method 1
!        plant_app= 0.
!        APPAMT = TAPP(application_index)*APPEFF(application_index)
!        DMAX = DEPI(application_index)
!        IF(DEPI(application_index).EQ.0.0)THEN
!          SOILAP(1,1)  = TAPP(application_index)*APPEFF(application_index)
!        ELSE
!          call pesticide_uniform_distribution(APPAMT,DMAX,distribution_of_applied) 
!        ENDIF
!        
!      case (5) ! Incorporated increasing with depth pesticide application 
!        plant_app= 0.
!        APPAMT = TAPP(application_index)*APPEFF(application_index)
!!       Solve for base of isosceles triangle given:length of 1 leg and area = 1.0
!        DMAX = DEPI(application_index)
!        BASE=2./DMAX
!        SLOPE=(-BASE/DMAX)
!        IF(DEPI(application_index) == 0.0)THEN
!          distribution_of_applied(1)  = TAPP(application_index)*APPEFF(application_index)
!        ELSE
!          CALL pesticide_increasing_distribution(APPAMT,DMAX,BASE,SLOPE,distribution_of_applied)
!        ENDIF
!
!      case (6) !**** linearly decreasing with depth *****
!        plant_app= 0.
!        APPAMT = TAPP(application_index)*APPEFF(application_index)
!        !  Solve for base of isosceles triangle given: length of 1 leg and area = 1.0
!        DMAX = DEPI(application_index)
!        BASE=2./DMAX
!        SLOPE=(-BASE/DMAX)
!        IF(DEPI(application_index).EQ.0.0)THEN
!          distribution_of_applied(1)  = TAPP(application_index)*APPEFF(application_index)
!        ELSE
!          call pesticide_decreasing_distribution(APPAMT,DMAX,BASE,SLOPE, distribution_of_applied) 
!        ENDIF
!        
!      case (7)  !**** T-Band *************************
!        plant_app= 0.
!        APPAMT = TAPP(application_index)*APPEFF(application_index)
!        DMAX = DEPI(application_index)
!        IF(DEPI(application_index).EQ.0.0)THEN
!          distribution_of_applied(1)  = TAPP(application_index)*APPEFF(application_index)
!        ELSE
!          CALL pesticide_Tband_distribution(APPAMT,DMAX, Tband_top(application_index),distribution_of_applied)
!        ENDIF
!        
!      case(8)
!        plant_app= 0.
!        APPAMT = TAPP(application_index)*APPEFF(application_index)
!        !Solve for base of isosceles triangle given:length of 1 leg and area = 1.0
!        DMAX = DEPI(application_index)
!        BASE=0.
!        SLOPE=0.
!        IF(DEPI(application_index).EQ.0.0)THEN
!          distribution_of_applied(1)  = TAPP(application_index)*APPEFF(application_index)
!        ELSE
!          CALL pesticide_atdepth_distribution(APPAMT,DMAX,distribution_of_applied)
!        ENDIF
!        
!      case (9)
!        !IF (DELX(1) .GE. 2.0) THEN
!        !  plant_app    = COVER * TAPP(application_index) * APPEFF(application_index)
!        !  distribution_of_applied(1)  = (1.0- COVER) * TAPP(application_index)*APPEFF(application_index)
!        !ELSE
!          plant_app    = COVER * TAPP(application_index) * APPEFF(application_index)
!          APPAMT       = (1.0- COVER) * TAPP(application_index)*APPEFF(application_index)
!          !Solve for base of isosceles triangle given:length of 1 leg and area = 1.0     
!          IF(DEPI(application_index).GT.0.0)THEN
!             DMAX = DEPI(application_index)
!          ELSE
!             DMAX = 4.0
!          ENDIF
!          BASE=2./DMAX
!          SLOPE=(-BASE/DMAX)
!          IF(DEPI(application_index).EQ.0.0)THEN
!            distribution_of_applied(1)  = TAPP(application_index)*APPEFF(application_index)
!          ELSE
!            call pesticide_decreasing_distribution(APPAMT,DMAX,BASE,SLOPE,distribution_of_applied)
!          ENDIF
!        !ENDIF
!        FOLPST(1)    = FOLPST(1)+ plant_app
!
!
!
!      end select
!    
!     !Calculate Soil Pesticide Mass as a Total Concentrations (Total Mass/Water-- kind of odd to use per water):
!
!     conc_total_per_water(1,:) = conc_total_per_water(1,:)   +   distribution_of_applied/(delx*theta_zero)
!     mass_in_compartment(1,:)  = mass_in_compartment(1,:)    +   distribution_of_applied 
!
!END SUBROUTINE PESTAP
!
!
!
!!*********************************************************************************************************
!SUBROUTINE plant_pesticide_harvest_application
!
!      use  constants_and_Variables, ONLY: FOLPST, theta_zero,delx, conc_total_per_water, &
!                                          mass_in_compartment, harvest_placement,ncom2, nchem
!      use utilities
!      implicit none
!
!      !Determines amount of pesticide which disappears from plant surface by first order decay.  The variable PLDKRT
!      !is a pseudo first order decay rate which may include processes of volatilization, oxidation, photolysis, etc.
!      !also determines pesticide washed off during rainfall events.
!
!      INTEGER:: K
!
!      REAL         DMAX,SLOPE,BASE
!      real   ::    harvested_to_soil(3, ncom2)
!      real   ::    unit_distribution (ncom2)
!
!      harvested_to_soil=0.0
!       
!      select case (harvest_placement)
!         case (1) !pesticide is applied to the gound in a linearly decreasing manner to 4 cm
!          
!              DMAX = 4.0
!              BASE=2./DMAX
!              SLOPE=(-BASE/DMAX)
!              call pesticide_decreasing_distribution(1.0,DMAX,BASE,SLOPE,  unit_distribution) 
!              
!              do k=1, nchem
!                  
!                    harvested_to_soil(k,:) =   unit_distribution* FOLPST(k)
!                    conc_total_per_water(k,:) = conc_total_per_water(k,:)   +    harvested_to_soil(k,:)/(delx*theta_zero)
!                    mass_in_compartment(k,:)  = mass_in_compartment(k,:)    +    harvested_to_soil(k,:)
!              end do
!              
!              FOLPST = 0.0 
!              
!          case (2)  !pesticide is removed
!              FOLPST = 0.0   
!          case (3)  !pesticide remains on the plant (ghost plant)
!          case default
!       
!      end select
!      
!end subroutine plant_pesticide_harvest_application
!
!
!
!!******************************************************************************************************************************
!
!SUBROUTINE pesticide_decreasing_distribution(APPAMT,DMAX,BASE,SLOPE,applied_to_soil )
!!Updated by Houbou with original changes propoposed by Ian Kennedy
!      use  constants_and_Variables, ONLY: delx, ncom2
!      implicit none
!      ! This routine distributes a chemical application down to an input depth decreasing proportionally with depth.
!      ! Used for CAM 1 and 6 and Harvest
!      
!      ! APPAMT - total amount of chemical to distribute in the soil
!      ! CHEM   - chemical id number (1-3)
!      ! DMAX   - depth to which chemical should be applied;
!      ! BASE   - initial starting amount of application
!      ! SLOPE  - slope of linearly decreasing application
!      !OUTPUT IS SOILAP the distribution of applied pesticide in the profile
!      
!
!      REAL,intent(in)  :: APPAMT,DMAX,BASE,SLOPE
!      real,intent(out) :: applied_to_soil(ncom2)
!
!      INTEGER  CMPT, ncmpwithchem, i
!      REAL     DEP,APPTOT,APPREM,FRACT,SLP2,FRCTOT, prev, curr, bottom
!
!      !FRCTOT=0.0
!      !SLP2= 0.
!      !CMPT= 0
!      DEP = 0.0
!      !APPTOT=0.0
!      !APPREM=APPAMT
!      applied_to_soil = 0.0
!      prev = 0.0
!      bottom = 0.0
!
!      do ncmpwithchem=1, ncom2 ! Count the number of compartments that will have chemical added
!        DEP = DEP + delx(ncmpwithchem)
!        if (DEP >= DMAX) exit
!      end do
!
!      do i=1, ncmpwithchem
!        bottom = min(bottom + delx(i), DMAX)
!        curr = bottom*bottom*SLOPE/2 + BASE*bottom
!        applied_to_soil(i) = (curr - prev) * APPAMT
!        prev = curr
!      end do
!      
!      !do 
!      !  CMPT= CMPT + 1
!      !  FRACT=(SLOPE*(DEP+DELX(CMPT)/2.)+BASE)*DELX(CMPT)
!      !  FRCTOT=FRCTOT+FRACT
!      !  IF(FRCTOT.GT.1.00)FRACT=FRACT-(1.0-FRCTOT)
!      !  applied_to_soil(CMPT)=AMIN1(APPREM,APPAMT*FRACT)
!      !  APPTOT=APPTOT+applied_to_soil(CMPT)
!      !  APPREM=APPAMT-APPTOT
!      !  DEP=DEP+delx(CMPT)
!      !  if (DEP >= (DMAX-1.E-4)) exit
!      !end do       
!END SUBROUTINE pesticide_decreasing_distribution
!
!
!
!
!
!    !SUBROUTINE pesticide_decreasing_distribution(APPAMT,DMAX,BASE,SLOPE,applied_to_soil )
!    !  use  constants_and_Variables, ONLY: delx, ncom2
!    !  implicit none
!    !  ! This routine distributes a chemical application down to an input depth decreasing proportionally with depth.
!    !  ! Used for CAM 1 and 6 and Harvest
!    !  
!    !  ! APPAMT - total amount of chemical to distribute in the soil
!    !  ! CHEM   - chemical id number (1-3)
!    !  ! DMAX   - depth to which chemical should be applied;
!    !  ! BASE   - initial starting amount of application
!    !  ! SLOPE  - slope of linearly decreasing application
!    !
!    !  !OUTPUT IS SOILAP the distribution of applied pesticide in the profile
!    !  
!    !
!    !  REAL,intent(in)  :: APPAMT,DMAX,BASE,SLOPE
!    !  real,intent(out) :: applied_to_soil(ncom2)
!    !   
!    !  INTEGER  CMPT
!    !  REAL     DEP,APPTOT,APPREM,FRACT,SLP2,FRCTOT
!    ! 
!    !  
!    !  FRCTOT=0.0
!    !  SLP2= 0.
!    !  CMPT= 0
!    !  DEP = 0.0
!    !  APPTOT=0.0
!    !  APPREM=APPAMT
!    !  applied_to_soil = 0.0
!    !  
!    !  
!    !
!    !  do 
!    !    CMPT= CMPT + 1
!    !    FRACT=(SLOPE*(DEP+DELX(CMPT)/2.)+BASE)*DELX(CMPT)
!    !    FRCTOT=FRCTOT+FRACT
!    !    IF(FRCTOT.GT.1.00)FRACT=FRACT-(1.0-FRCTOT)
!    !    applied_to_soil(CMPT)=AMIN1(APPREM,APPAMT*FRACT)
!    !    APPTOT=APPTOT+applied_to_soil(CMPT)
!    !    APPREM=APPAMT-APPTOT
!    !    DEP=DEP+delx(CMPT)
!    !    if (DEP >= (DMAX-1.E-4)) exit
!    !  end do
!    !   
!    !END SUBROUTINE pesticide_decreasing_distribution
!
!!******************************************************************************************************************************
! SUBROUTINE pesticide_uniform_distribution(APPAMT,DMAX,applied_to_soil)
!       use  constants_and_Variables, ONLY: DELX,ncom2
!       implicit none
!      ! This routine distributes a chemical application uniformly to depth dmax.
!      ! Used for CAM 1 ans 6 and Harvest
!      
!      ! APPAMT - total amount of chemical to distribute in the soil
!      ! CHEM   - chemical id number (1-3)
!      ! DMAX   - depth to which chemical should be applied;
!
!      ! FRCTOT=0.0
!      ! SLP2= 0.
!      ! CMPT= 0
!      ! DEP = 0.0
!      ! APPTOT=0.0
!      ! APPREM=APPAMT
!      ! applied_to_soil = 0.0
!
!      ! do       
!        ! CMPT= CMPT + 1
!        ! FRACT=DELX(CMPT)/DMAX
!        ! FRCTOT=FRCTOT+FRACT
!        ! IF(FRCTOT.GT.1.00)FRACT =FRACT-(1.0-FRCTOT)
!        ! applied_to_soil(CMPT) = AMIN1(APPREM,APPAMT*FRACT)
!        ! APPTOT= APPTOT+applied_to_soil(CMPT)
!        ! APPREM=APPAMT-APPTOT
!        ! DEP=DEP+DELX(CMPT)
!        ! if (DEP >= DMAX) exit
!      ! end do  
!      
!      REAL,intent(in)  :: APPAMT,DMAX
!      real,intent(out) :: applied_to_soil(ncom2)
!       
!      INTEGER  CMPT, ncmpwithchem, i
!      REAL     DEP,APPTOT,APPREM,FRACT,SLP2,FRCTOT, prev, curr, bottom
!      
!      DEP = 0.0
!      applied_to_soil = 0.0
!      prev = 0.0
!      bottom = 0.0
!
!      do ncmpwithchem=1, ncom2 ! Count the number of compartments that will have chemical added
!        DEP = DEP + delx(ncmpwithchem)
!        if (DEP >= DMAX) exit
!      end do
!
!      do i=1, ncmpwithchem
!        bottom = min(bottom + delx(i), DMAX)
!	 curr = bottom/DMAX
!        applied_to_soil(i) = (curr - prev) * APPAMT
!        prev = curr
!      end do  
!
!
!       
! END SUBROUTINE pesticide_uniform_distribution
!
!
!
!
!    !
!    !SUBROUTINE pesticide_uniform_distribution(APPAMT,DMAX,applied_to_soil)
!    !   use  constants_and_Variables, ONLY: DELX,ncom2
!    !   implicit none
!    !  ! This routine distributes a chemical application uniformly to depth dmax.
!    !  ! Used for CAM 1 ans 6 and Harvest
!    !  
!    !  ! APPAMT - total amount of chemical to distribute in the soil
!    !  ! CHEM   - chemical id number (1-3)
!    !  ! DMAX   - depth to which chemical should be applied;
!    !
!    !  real,intent(out) :: applied_to_soil(ncom2)
!    !  REAL     APPAMT,DMAX
!    !  INTEGER  CMPT
!    !  REAL     DEP,APPTOT,APPREM,FRACT,SLP2,FRCTOT
!    !
!    !  FRCTOT=0.0
!    !  SLP2= 0.
!    !  CMPT= 0
!    !  DEP = 0.0
!    !  APPTOT=0.0
!    !  APPREM=APPAMT
!    !  applied_to_soil = 0.0
!    !
!    !  do       
!    !    CMPT= CMPT + 1
!    !    FRACT=DELX(CMPT)/DMAX
!    !    FRCTOT=FRCTOT+FRACT
!    !    IF(FRCTOT.GT.1.00)FRACT =FRACT-(1.0-FRCTOT)
!    !    applied_to_soil(CMPT) = AMIN1(APPREM,APPAMT*FRACT)
!    !    APPTOT= APPTOT+applied_to_soil(CMPT)
!    !    APPREM=APPAMT-APPTOT
!    !    DEP=DEP+DELX(CMPT)
!    !    if (DEP >= DMAX) exit
!    !  end do  
!    !   
!    !END SUBROUTINE pesticide_uniform_distribution
!    
! 
! 
! 
! 
! SUBROUTINE pesticide_increasing_distribution(APPAMT,DMAX,BASE,SLOPE,applied_to_soil)
!       use  constants_and_Variables, ONLY: delx,ncom2
!      implicit none
!      ! This routine distributes a chemical application down to an input depth increasing proportionally with depth.
!      
!      ! APPAMT - total amount of chemical to distribute in the soil
!      ! CHEM   - chemical id number (1-3)
!      ! DMAX   - depth to which chemical should be applied;
!      ! BASE   - initial starting amount of application
!      ! SLOPE  - slope of linearly decreasing application
!
!      ! FRCTOT=0.0
!      ! SLP2= 0.
!      ! CMPT= 0
!      ! DEP = 0.0
!      ! APPTOT=0.0
!      ! APPREM=APPAMT
!      ! applied_to_soil = 0.0
!      
!      ! do     
!        ! CMPT= CMPT + 1
!        ! DEP=DEP+DELX(CMPT)        
!        ! FRACT=(SLOPE*((DMAX-DEP)+DELX(CMPT)/2.)+BASE)*DELX(CMPT)
!        ! FRCTOT=FRCTOT+FRACT
!        ! IF(FRCTOT.GT.1.00)FRACT=FRACT-(1.0-FRCTOT)
!        ! applied_to_soil(CMPT)=AMIN1(APPREM,APPAMT*FRACT)
!        ! APPTOT=APPTOT+applied_to_soil(CMPT)
!        ! APPREM=APPAMT-APPTOT
!        ! IF(DEP>=(DMAX-1.E-4)) exit
!      ! end do
!
!      REAL,intent(in)  :: APPAMT,DMAX,BASE,SLOPE
!      real,intent(out) :: applied_to_soil(ncom2)
!       
!      INTEGER  CMPT, ncmpwithchem, i
!      REAL     DEP,APPTOT,APPREM,FRACT,SLP2,FRCTOT, prev, curr, bottom
!      
!      DEP = 0.0      
!      applied_to_soil = 0.0
!      prev = 0.0
!      bottom = 0.0
!
!      do ncmpwithchem=1, ncom2 ! Count the number of compartments that will have chemical added
!        DEP = DEP + delx(ncmpwithchem)
!        if (DEP >= DMAX) exit
!      end do
!
!      do i=1, ncmpwithchem
!        bottom = min(bottom + delx(i), DMAX)
!        curr = 1 - ((DMAX-bottom)*(DMAX-bottom)*SLOPE/2 + BASE*(DMAX-bottom))
!        applied_to_soil(i) = (curr - prev) * APPAMT
!        prev = curr
!      end do      
!
!    END SUBROUTINE pesticide_increasing_distribution
! 
! 
! 
! 
! 
! 
! 
! 
! 
! 
! 
! 
! 
! 
! 
! 
!    !
!    !
!    !
!    !
!    !SUBROUTINE pesticide_increasing_distribution(APPAMT,DMAX,BASE,SLOPE,applied_to_soil)
!    !  use  constants_and_Variables, ONLY: delx,ncom2
!    !  implicit none
!    !  !This routine distributes a chemical application down to an input depth increasing proportionally with depth.
!    !  
!    !  !APPAMT - total amount of chemical to distribute in the soil
!    !  !CHEM   - chemical id number (1-3)
!    !  !DMAX   - depth to which chemical should be applied;
!    !  !BASE   - initial starting amount of application
!    !  !SLOPE  - slope of linearly decreasing application
!    !
!    !  real,intent(out) :: applied_to_soil(ncom2)
!    !  REAL     APPAMT,DMAX,BASE,SLOPE
!    !  INTEGER  CMPT
!    !  REAL     DEP,APPTOT,APPREM,FRACT,SLP2,FRCTOT
!    !
!    !  FRCTOT=0.0
!    !  SLP2= 0.
!    !  CMPT= 0
!    !  DEP = 0.0
!    !  APPTOT=0.0
!    !  APPREM=APPAMT
!    !  applied_to_soil = 0.0
!    !  
!    !  do     
!    !    CMPT= CMPT + 1
!    !    DEP=DEP+DELX(CMPT)        
!    !    FRACT=(SLOPE*((DMAX-DEP)+DELX(CMPT)/2.)+BASE)*DELX(CMPT)
!    !    FRCTOT=FRCTOT+FRACT
!    !    IF(FRCTOT.GT.1.00)FRACT=FRACT-(1.0-FRCTOT)
!    !    applied_to_soil(CMPT)=AMIN1(APPREM,APPAMT*FRACT)
!    !    APPTOT=APPTOT+applied_to_soil(CMPT)
!    !    APPREM=APPAMT-APPTOT
!    !    IF(DEP>=(DMAX-1.E-4)) exit
!    !  end do
!    !
!    !END SUBROUTINE pesticide_increasing_distribution
!    !
!  
!    SUBROUTINE pesticide_Tband_distribution(APPAMT,DMAX,top2cm,applied_to_soil)
!      use  constants_and_Variables, ONLY: DELX,ncom2
!      implicit none
!      !This routine distributes in a T Band: Part in the top 2 cm and the remainder uniformly distributed, 
!      
!      !APPAMT - total amount of chemical to distribute in the soil
!      !CHEM   - chemical id number (1-3)
!      !DMAX   - depth to which chemical should be applied;
!
!      real,intent(out) :: applied_to_soil(ncom2)
!      REAL     APPAMT,DMAX
!      INTEGER  CMPT
!      REAL     depth,APPTOT,APPREM,SLP2,FRCTOT
!      real,intent(in) :: top2cm
!      
!      
!      FRCTOT=0.0
!      SLP2= 0.
!      CMPT= 0
!      depth = 0.0
!      APPTOT=0.0
!      APPREM = APPAMT
!      applied_to_soil = 0.0
!      
!       do
!         CMPT= CMPT + 1
!         depth = depth + DELX(CMPT)
!         SLP2=DELX(CMPT)/2.
!         IF(SLP2 > 1.)  SLP2=1.0
!         applied_to_soil(CMPT) = AMIN1(APPREM,(SLP2*APPAMT*top2cm))
!         APPTOT=APPTOT+applied_to_soil(CMPT)
!         APPREM=(APPAMT*top2cm)-APPTOT
!         if (depth >= 1.95) exit
!         
!
!       end do 
!
!       do  
!          IF((depth.GT.1.95).AND.(depth.LT.2.05))APPREM=APPAMT-APPTOT
!          CMPT= CMPT + 1
!          depth = depth+DELX(CMPT)
!          SLP2=DELX(CMPT)/(DMAX-2.)
!          IF(SLP2.GT.1.)SLP2=1.0
!        !  SOILAP(CHEM,CMPT)=AMIN1(APPREM,(SLP2*APPAMT*(1.0-DRFT(CHEM,NAPPC))))
!          applied_to_soil(CMPT)=AMIN1(APPREM,(SLP2*APPAMT*(1.0-top2cm)))
!          APPTOT=APPTOT + applied_to_soil(CMPT)
!          APPREM=APPAMT-APPTOT
!          if (depth >= (DMAX-1.E-4)) exit
!
!
!       end do
!             
!
!
!
!       
!       
!       
!    END SUBROUTINE pesticide_Tband_distribution
!    
!  
!    
!    
!    
!    
!    SUBROUTINE pesticide_atdepth_distribution(APPAMT,DMAX,applied_to_soil)
!          use constants_and_variables, ONLY: DELX,ncom2
!          implicit none
!          !Places all pesticide at a specfic depth
!          !APPAMT - total amount of chemical to distribute in the soil
!          !CHEM   - chemical id number (1-3)
!          
!          real,intent(out)   :: applied_to_soil(ncom2)
!          REAL,intent(in)    ::  APPAMT,DMAX
!          INTEGER  CMPT
!          REAL     DEP
!          
!          CMPT= 0
!          DEP = 0.0
!          applied_to_soil = 0.0
!          
!          do
!            CMPT= CMPT + 1
!            DEP=DEP+DELX(CMPT)
!            IF(DEP >= (DMAX-1.E-4)) exit
!          end do     
!          
!          applied_to_soil(CMPT)=APPAMT
!          
!    END SUBROUTINE pesticide_atdepth_distribution
!
!     
!  
!  
!    end module Pesticide_Applications
    
 
 
 
    
    
    