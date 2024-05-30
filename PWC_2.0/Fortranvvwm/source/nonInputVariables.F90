module nonInputVariables
implicit none

integer :: num_records  !number of met file records

real,allocatable,dimension(:) :: k_aer_aq     !aqueous-phase aerobic rate (per sec)
real,allocatable,dimension(:) :: k_anaer_aq   !aqueous-phase anaerobic rate (per sec)
real,allocatable,dimension(:) :: k_aer_s      !sorbed-phase aerobic rate (per sec)
real,allocatable,dimension(:) :: k_anaer_s    !sorbed-phase anaerobic rate (per sec)
real,allocatable,dimension(:) :: k_volatile   !first order volatilization rate (per sec)
real,allocatable,dimension(:) :: k_photo      !photolysis rate (1/sec)
real,allocatable,dimension(:) :: daily_depth  !daily water body depths
real,allocatable,dimension(:) :: k_hydro      !hydrolysis rate (per sec)
real,allocatable,dimension(:) :: k_flow       !array of daily wash out rates (per second)
real,allocatable,dimension(:) :: k_burial
real,allocatable,dimension(:) :: theta        !solute holding capacity ratio [--]
real,allocatable,dimension(:) :: capacity_1   !solute holding capacity of region 1 [m3]
real                          :: capacity_2   !solute holding capacity of region 2 [m3]
real,allocatable,dimension(:) :: fw1          !fraction of region 1 solute in aqueous phase
real                          :: fw2          !fraction of region 2 solute in aqueous phase
real                          :: kd_sed_1     !local Kd of ss (m3/kg)
real                          :: m_sed_1      !base amount of suspended solids mass (kg)

real                          :: omega                !mass transfer coefficient
real,allocatable,dimension(:) :: gamma_1              !effective littoral degradation
real,allocatable,dimension(:) :: gamma_2              !effective benthic degradation

real,allocatable,dimension(:):: wind                  !wind speed (m/s) at 10 cm
real,allocatable,dimension(:):: temp_avg              !average 30 day previous temperature
real,allocatable,dimension(:):: evap                  !evaporation (m)
real,allocatable,dimension(:):: precip                !precipitation (m)

real,allocatable,dimension(:,:,:):: mass              !daily mass loading from runoff (column 1) & erosion (column 2) (kg)

real,allocatable,dimension(:) :: q                    !(m3/sec) array of daily flow

!Redundancy with calvulation of v and v1:  Fix this eventually. Calculated in volume cals as well as solute holdig calcs.

real,allocatable,dimension(:) :: volume1              !array of daily water column volumes  [m3]
real,allocatable,dimension(:) :: burial               !(kg/sec) array of daily burial rate
real,allocatable,dimension(:) :: eroded_solids_mass   !read in from zts as tonnes/day but immediately converted to kg/day
real:: v2                                          !aqueous volume of pore water in sediment



real,allocatable,dimension(:) :: fraction_to_benthic

real, allocatable, dimension(:)    :: A,B,E,F          !final coefficients for the 2 simultaneous equations

real, allocatable, dimension(:)    :: m1_input, m2_input               !at start of time step: the mass input in litt and benthic
real, allocatable, dimension(:)    :: m1_store,m2_store,mavg1_store    !array of daily peak/avg. mass in littoral and benthic

real, allocatable, dimension(:)    :: aq1_store, aq2_store             !begining day concentrations, after mass inputs


integer :: num_years
integer :: firstyear
integer :: startday1900,firstday, firstmon


real,allocatable,dimension(:) :: aqconc_avg1 ,aqconc_avg2  ! daily average aqueous concentrations
real,allocatable,dimension(:)   :: degradateProduced1,degradateProduced2
!real, allocatable, dimension(:) :: v1              !volume of water column, also assumed to be the aqueous volume w/SS negligible


real:: spray_total(3)
real:: runoff_total(3)
real:: erosion_total(3)

real :: Sediment_conversion_factor !Converts pore water to concentration (total mass)/(dry solid mass)
real :: Daily_Avg_Runoff
real :: Daily_avg_flow_out

end module nonInputVariables