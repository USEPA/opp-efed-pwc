module Temperatue_Calcs
    implicit none
    contains
    

      
      SUBROUTINE  Henry_Temp_Correction(Soil_temp,HENRY,ENP, henry_ref_temp,NUMB, NEWK)
           implicit none
!          to correct Henry's constant using Clausius-Clapeyron equation
           
           INTEGER,intent(in) ::  NUMB
           REAL,intent(in)    ::  Soil_temp(NUMB)
           REAL,intent(in)    ::  HENRY,ENP
           real,intent(out)   ::  NEWK(NUMB)
           
           real,intent(in)    ::  henry_ref_temp  !degrees C, temp at which variable henry applies

                     
           IF (HENRY .GT. 0.0) THEN
              ! newk =10.0**(LOG10(HENRY)-1000.0*ENP*( (henry_ref_temp  - Soil_temp) / (298.16*   (273.16 + Soil_temp)    )/(2.302585*1.98718))    )
               newk =10.0**(LOG10(HENRY)-(0.73298947987721)*ENP*( (henry_ref_temp  - Soil_temp) / (273.16 + Soil_temp)    )    )  
           ELSE
               NEWK = 0.0    !   not a valid Henry's value, don't correct
           END IF
 
      END SUBROUTINE  Henry_Temp_Correction
      

  !*******************************************************************************************************  
  SUBROUTINE SLTEMP()
      use  constants_and_Variables, ONLY: ncom2, SOLRAD, air_TEMP, wind,sttdet,bbt,albedo,              &
       emmiss,soil_temp,ubt,COVER,HEIGHT,bulkdensity,theta_zero,DELX,sand,clay,theta_wp,theta_fc,theta_sat,ncom2,snow,    &
       orgcarb,vonKarman,uWind_Reference_Height,julday1900
     
      use utilities
      use volatilization
!     Calculates the soil temperature profile using
!     air temperatures, solar radiation, surface albedo, wind velocity,
!     evaporation, soil water content, and soil physical properties as
!     input data. This procedure is based on the methods of
!     Thebodeaux (1979);
!     Van Bavel and  Hillel (1975); de Vries (1963);
!     and Hanks et al. (1971).
 
      
!   ******   !all array sizes shoul dbe ncom2 not ncmpts** fix this if you get time
      
      
      implicit none

      INTEGER  I,J,K,L,N,NUMDYS
      REAL     AIRDEN,Z0,D,ZCH,HTC,QC1,QEVF,QLW1,QLW2,QSWR
      real     QGHF,TEMPK,STK,FX1,FX2,DELTA,EVAP,AAA,BBB
      real     THKLY1,ABSOIL
      REAL     XVOL(5,ncom2),VOLCOR,LBTEMP
      real     DIFFCO(ncom2),ALAMDA(0:5),GEE(5,3),AKAY(5)
      real     THZERO(2),GFLD,SIGMA0,SIGMA1,SIGMA2,VAPLMD,AIRLMD
      real     TA(ncom2),TB(ncom2),TC(ncom2),TF(ncom2)

      real ::  vhtcap(ncom2),THCOND(ncom2)
      

      
      
!     + + + DATA INITIALIZATIONS + + +

      DATA GEE(1,1),GEE(1,2),GEE(1,3)/2*0.125,0.750/
      DATA GEE(2,1),GEE(2,2),GEE(2,3)/2*0.125,0.750/
      DATA GEE(3,1),GEE(3,2),GEE(3,3)/2*0.5,0.0/
      DATA GEE(4,1),GEE(4,2),GEE(4,3)/3*0.333/
      DATA ALAMDA/122.7,1762.6,604.8,51.8,122.7,5.3/
      DATA VAPLMD/15.2/
      
      integer :: current_day,current_month, current_year
      
      
      
      call get_date (julday1900, current_year, current_month, current_day)
      

!     Interpolation of daily values from neighboring monthly values of
!     soil surface albedo and bottom boundary temperature

      BBT(13) = BBT(1)
      numdys = jd(current_year, current_month+1, current_day) - jd(current_year, current_month, current_day)

      LBTEMP = BBT(current_month) + (BBT(current_month+1)-BBT(current_month))*current_day/NUMDYS
      ALBEDO(13) = ALBEDO(1)
      ABSOIL = ALBEDO(current_month)+(ALBEDO(current_month+1)-ALBEDO(current_month))*current_day/NUMDYS

!     Estimation of surface albedo from canopy albedo and
!     soil surface albedo

      IF (SNOW .LT. 0.5)THEN
        ABSOIL = 0.23*COVER + ABSOIL*(1.-COVER)
      ELSE
        ABSOIL = 0.8
      END IF

!     Evaporation correction for crop canopy

      EVAP = STTDET*(1.-COVER)

      AIRLMD  = 5.3

!     If thermal conductivity and heat capacities are supplied,
!     bypass this sec.

    !  IF (IDFLAG .EQ. 1) then  !thermal conductivity and heat capacity flag. 1=yes, 0=no

   
          
!     This portion of the routine estimates the Thermal Diffusivity of
!     soil compartment as the soil water content changes with time and
!     depth, using the procedure of de Vries (1963).
 
         do L =1,NCOM2

         !The vol fractions of sand,clay,and OM are adjusted so that their
         !total value equals to (1-porosity)

          VOLCOR=(1.0-theta_sat(L))/((SAND(L)/2.65+CLAY(L)/2.65+orgcarb(L)*1.724/1.3)*bulkdensity(L))

!       Conversion of Wt percents of soil constituents to vol fractions

        XVOL(1,L) = SAND(L)*bulkdensity(L)/2.65*VOLCOR
        XVOL(3,L) = orgcarb(L)*1.724*bulkdensity(L)/1.30*VOLCOR
        XVOL(2,L) = 1.0-theta_sat(L)-XVOL(1,L)-XVOL(3,L)

!       Defining water content and air in the soil pores

        XVOL(4,L) = theta_zero(L)
        IF(theta_zero(L) .LT. theta_wp(L))  XVOL(4,L)=theta_wp(L)
        IF(theta_zero(L) .GT. theta_sat(L)) XVOL(4,L)=theta_sat(L)
        XVOL(5,L) = theta_sat(L) - XVOL(4,L)

!       Estimation of 'G' parameter when W.C is greater than F.C.

        IF (XVOL(4,L) .GT. theta_fc(L))THEN
          GEE(5,1) = 0.333 - XVOL(5,L)/theta_sat(L)*(0.333-0.035)
          ALAMDA(5) = AIRLMD + VAPLMD

!       Estimation of 'G' parameter when water content is less than F.C.

        ELSE
          GFLD = 0.333 - (theta_sat(L)-theta_fc(L))/theta_sat(L)*(0.333-0.035)
          GEE(5,1) = 0.013 + XVOL(4,L)/theta_fc(L)*(GFLD-0.013)
          ALAMDA(5) = AIRLMD + XVOL(4,L)/theta_fc(L)*VAPLMD
        END IF
        GEE(5,2)=GEE(5,1)
        GEE(5,3)=1.-2*GEE(5,1)
        ALAMDA(0) = ALAMDA(4)

!       Estimation of thermal conductivity

        K = 0
 10     K = K+1
        SIGMA1 = 0.0
        SIGMA2 = 0.0
         do I = 1,5
           SIGMA0 = 0.0

!          Estimation of 'K' parameter

           do J = 1, 3
             SIGMA0 = SIGMA0+1./(1.+(ALAMDA(I)/ALAMDA(0)-1.)*GEE(I,J))
           end do
           AKAY(I) = SIGMA0/3.
           SIGMA1 = SIGMA1 + AKAY(I)*XVOL(I,L)*ALAMDA(I)
           SIGMA2 = SIGMA2 + AKAY(I)*XVOL(I,L)
         end do

         !Thermal Conductivity in cal/cm-day-C
         THZERO(K) =SIGMA1/SIGMA2
         IF (theta_zero(L) .LT. theta_wp(L) .AND. K .LT. 2)THEN
          XVOL(4,L) = 0.0
          XVOL(5,L) = theta_sat(L)
          ALAMDA(5) = AIRLMD
          ALAMDA(0) = ALAMDA(5)
          GOTO 10
         END IF
 
         !Interpolation of thermal cond. when W.C. is less than critical point

         IF (theta_zero(L) .LT. theta_wp(L))THEN
          THZERO(2) = 1.25*THZERO(2)
          THCOND(L)=THZERO(2)+theta_zero(L)/theta_wp(L)*(THZERO(1)-THZERO(2))
         ELSE
          THCOND(L)=THZERO(1)
         END IF

         !Volumetric Heat Capacity of the soil layer, cal/cm3-C
         VHTCAP(L)=0.46*(XVOL(1,L)+XVOL(2,L)) + 0.6*XVOL(3,L) + theta_zero(L)
         end do
         

         
   !   end if  !end of calc for IDFLAG=1


      do L = 1, NCOM2
!       Diffusion coefficient, cm2/day
        DIFFCO(L) = THCOND(L)/VHTCAP(L)
      end do

      
         
      
     
      
      
      
      
      
      
!     This portion of the subroutine estimates the Upper
!     Boundary Temperature using Energy-Balance at the air/soil
!     interface. The fourth order equation in terms of soil surface
!     temperature is solved by Newton-Raphson method for upper
!     boundary temperature.

!     Air Density(from Thibodeaux), gm/cm3

      AIRDEN = (-0.0042*air_TEMP +1.292)*1.E-3

      ! Computes zero displacement height, D (meter)
      ! and the roughness length, Z0 (meter)
      ZCH = HEIGHT / 100.0    ! convert to meter    ****** !same calc used in temperature routine STREAMLINE if possible
      Call Get_Crop_Params (ZCH, Z0, D)

!     Heat Transfer coefficient at air-surface interface, cm/day
!     Wind speed in cm/sec, (Wind * 86400) in cm/day

      HTC = vonKarman**2 * WIND * 86400 /((Log((uWind_Reference_Height-D)/Z0))**2)

!     Sensible air Heat Flux term, cal/cm2-K-day

      QC1= AIRDEN*0.2402*HTC

!     Evaporation heat flux term, cal/cm2-day, EVAP in cm/day

      QEVF = 580.0 * EVAP * 1.0

!     Atmospheric Longwave radiation component term, cal/cm2-K-day

      TEMPK = air_TEMP + 273.18
      QLW1 = EMMISS*0.936E-5*(TEMPK**2)*11.7E-8
!
!     Longwave radiation component emitted by the soil
!     term, cal/cm2-K-day
!
      QLW2 = EMMISS*11.7E-8
!
!     Short Wave radiation term, cal/cm2-day
!
  QSWR = (1.- ABSOIL)*SOLRAD


      
!     Calculation of Soil Heat flux term, cal/cm-C-day
!     Estimation of average temp gradient in the top 5cm of soil

      N = 1
      THKLY1 = DELX(1)
      AAA = 1.0/DELX(1)
      BBB = (soil_temp(1) + 273.18)/DELX(1)
      
      
!35    IF(THKLY1 .LT. 5.0) THEN
!        N = N +1
!        THKLY1 = THKLY1 + DELX(N)
!        BBB = BBB + (SPT(N) - SPT(N-1))/DELX(N)
!      GOTO 35
!      ENDIF
      
      do
        IF(THKLY1 .GE. 5.0) exit
        N = N +1
        THKLY1 = THKLY1 + DELX(N)
        BBB = BBB + (soil_temp(N) - soil_temp(N-1))/DELX(N)
      end do
       
      AAA = AAA/N
      BBB = BBB/N
      QGHF= THCOND(1)
!
!     Initializing the soil surface temperature
!
      STK = TEMPK
      DELTA = 0.0
!
!     Newton-Raphson method to solve the 4th order equation for UBT

 !40   STK = STK - DELTA
 !     FX1 = STK**4 + (QC1 + QGHF*AAA)/QLW2*STK - (QLW1*TEMPK**4.- QEVF+ QC1*TEMPK + QSWR + QGHF*BBB)/QLW2
 !     FX2 = 4.*STK**3 + (QC1 + QGHF*AAA)/QLW2
 !     DELTA = FX1/FX2
 !
 !     ! Convergence criteria, 0.1 deg C
 !     IF(ABS(DELTA) .GT. 0.1)GOTO 40
      
      
      do 
          STK = STK - DELTA
          FX1 = STK**4 + (QC1 + QGHF*AAA)/QLW2*STK - (QLW1*TEMPK**4.- QEVF+ QC1*TEMPK + QSWR + QGHF*BBB)/QLW2
          FX2 = 4.*STK**3 + (QC1 + QGHF*AAA)/QLW2
          DELTA = FX1/FX2
         
          IF(ABS(DELTA) <= 0.1) exit ! Convergence criteria, 0.1 deg C
      end do
      
      

      UBT = STK - 273.18

!     This portion of the routine simulates the Soil Temperature Profile
!     when Upper Boundary, Bottom Boundary, and Initial temperatures are
!     provided. Top Boundary Layer:

         TA(1)=0.0
         TC(1)=-(DIFFCO(1)+DIFFCO(2))*.5/(DELX(1)*(DELX(1)+DELX(2))*0.5)
         TB(1)=1.0+DIFFCO(1)/(DELX(1)**2)-TC(1)
         TF(1)=soil_temp(1)+DIFFCO(1)/(DELX(1)**2)*UBT
         

         !Non Boundary Layer:
         do I= 2, NCOM2-1
          TA(I) = -(DIFFCO(I-1)+DIFFCO(I))*0.5/(DELX(I)*(DELX(I)+DELX(I-1))*0.5)
          TC(I) = -(DIFFCO(I)+DIFFCO(I+1))*0.5/(DELX(I)*(DELX(I)+DELX(I+1))*0.5)
          TB(I) = 1.0 - (TA(I)+TC(I))
          TF(I) = soil_temp(I)
         end do
         
         !Bottom Boundary Layer:

         TA(NCOM2) = -(DIFFCO(NCOM2-1)+DIFFCO(NCOM2))*0.5/(DELX(NCOM2)*(DELX(NCOM2)+DELX(NCOM2-1))*0.5)
         TB(NCOM2) = 1.0 -TA(NCOM2)+DIFFCO(NCOM2)/(DELX(NCOM2)**2)
         TC(NCOM2) = 0.0
         TF(NCOM2) =soil_temp(NCOM2)+DIFFCO(NCOM2)/(DELX(NCOM2)**2)*LBTEMP

         CALL tridiagonal_solution (TA,TB,TC,soil_temp,TF,NCOM2)
         
         where (soil_temp <0.0) soil_temp = 0.0    !minimum soil temperature is 0.0 degrees
       
  END SUBROUTINE SLTEMP
    

end module Temperatue_Calcs
    