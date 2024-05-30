module erosion
    implicit none 
    contains
    
    SUBROUTINE EROSN(julday)
     !Determines loss of pesticide due to erosion by a variation of USLE and an enrichment ratio.
      use constants_and_Variables,ONLY: soil_temp,is_temperature_simulated,AFIELD,USLEK,USLELS,USLEP,cfac, &
                                        runof,erflag,sedl,enriched_eroded_solids,  julday1900,model_erosion,data_date , is_TC_lag_method
      use utilities
      implicit none

      integer,intent(in) :: julday  !used to determine the rainfall characteristics
      
      REAL ::  Q,QQP,SLKGHA,ENRICH
      REAL ::  EC0,EC1,EC2,TC,QP,QU
      
      SEDL=0.0
      
      !ELTT = 0 
      !check to see if first compartment frozen
      IF((is_temperature_simulated).AND.(soil_temp(1).LE.0.0)) Return
      
      ! Get Coefficients from Table F-1 in TR-55

      CALL TMCOEF(EC0,EC1,EC2, julday)  
  

    if (is_TC_lag_method) then
         call Time_of_conc_lag_method(TC)
    else
         CALL TMCONC_PRZM5(TC)
    end if
    
      
     

      

    
      
      
      !if (FLAG4) then
      !    CALL TMCONC_PRZM5(TC)
      !else 
      !    CALL TMCONC_PRZM3(TC)
      !end if
    
      QU=EC0+EC1*ALOG10(TC)+EC2*(ALOG10(TC))**2.
      QU=10.0**QU
      QP=(QU*(AFIELD*.00386)*(RUNOF*.3937))*0.02832
      QP=(QP/AFIELD)*360.
      Q=RUNOF*10.
      QQP=Q*QP
      

      
      
      ! ERFLAG=2: MUSLE
      ! ERFLAG=3: MUST
      ! ERFLAG=4: MUSS

      IF(ERFLAG.EQ.2)THEN
        SEDL=1.586*(QQP**0.56)*(AFIELD**0.12)
      ELSEIF(ERFLAG.EQ.3)THEN
        SEDL=2.5*(QQP**0.5)
      ELSEIF(ERFLAG.EQ.4)THEN
        SEDL=0.79*(QQP**0.65)*(AFIELD**0.009)
      ENDIF
      
      



      SEDL   = (SEDL* USLEK* USLELS* CFAC* USLEP)*AFIELD  !metric tons per day
   
      SLKGHA = (SEDL* 1000.)/AFIELD  !kg/ha
      
     
      

      !Used For Calibration routine option
      !Captures the erosion on specific days per calibration file
      where (data_date == julday1900)   model_erosion = SLKGHA
      
      
      
      IF(SLKGHA.EQ.0.0)THEN
        ENRICH=1.0
      ELSE
        ENRICH = 2.0- (0.2* log(SLKGHA)) 
        ENRICH= EXPCHK(ENRICH)
      ENDIF

      !Compute loss term for pesticide balance
      !delx(1) is in here and will cause problems later when declining erosion extraction is used
      
    !  ELTT=  (SLKGHA/(100000.*DELX(1)))*ENRICH   !grams/cm3
      enriched_eroded_solids=  (SLKGHA/(100000.))*ENRICH   !grams/cm2  (ha/10000 m2)(1000 g/kg)(m2/10000 cm2) = 1/100000
  
       
      
      
    END  SUBROUTINE EROSN
    
     
    
        !******************************************************************************** 
    SUBROUTINE TMCONC_PRZM5(TC)
      !PRZM5 Corrects an error in the sheet flow calculation where Rain should be used rather than runoff
      !Calculate time of concentration based on TR-55 method
      !TC = time of concentration (hrs)

       use  constants_and_Variables, ONLY: HL,SLP,N1,effective_rain 
       implicit none
       real, intent(out) :: TC
       REAL S1,S2,HL1,HL2,WATER,TT1,V2,TT2

!     ASSUME S2=S1, R2=0.4 FT, N2=0.05.  LIMIT HL1 TO 300'
      S1=SLP/100.
      S2=S1
!      R2=0.4
!      N2=0.08
 
      HL1=AMIN1(HL*3.28,300.)          !ft 300 max for sheet
      HL2=AMAX1(0.0,(HL*3.28)-300)     !remainder for conc flow

      
      water = effective_rain /2.54  !PRZM5 repair
      
    
      
     ! WATER=(RUNOF)/2.54                !inches but this is Runoff and it should be rain

      TT1=(0.007*(N1*HL1)**0.8) / ((WATER**0.5)*(S1**0.4))
      V2=16.1345*(S2)**0.5
      TT2=HL2/(3600.*V2)
      TC=TT1+TT2
  

      
    END SUBROUTINE TMCONC_PRZM5
    
    
    
   SUBROUTINE Time_of_conc_lag_method(TC)

      !Calculate time of concentration based on Watershed Lag Method ion NEH-4 Chapter 4
      !TC = time of concentration (hrs)
      !HL is in meters so must convert to feet forthis method
      !Slp is in perecent so no conversion needed
      
       use  constants_and_Variables, ONLY: HL,SLP,curve_number_daily
       implicit none
       real, intent(out) :: TC
       REAL HL1, S              

       HL1 = HL*3.28                       !convert to feet
       S = 1000./curve_number_daily - 10.0
    
       TC = HL1**.8 * (S  +1.)**0.7 / 1140.0 /slp**0.5

    END SUBROUTINE Time_of_conc_lag_method
    
    
    
    
    
  
! **********************************************
  SUBROUTINE TMCOEF(EC0,EC1,EC2,julday)
      !Gets Coefficients fro Table F-1 in TR-55
      use  constants_and_Variables, ONLY: PRECIP_rain, thrufl, ireg, inabs, smelt, is_true_rain_distribution

      implicit none 

      integer,intent(in) :: julday
       real, intent(out) :: EC0,EC1,EC2
      
      INTEGER  IFND,J,IREGOLD
      INTEGER  NBG(4),NEN(4)
      REAL     CC(32),CC0(32),CC1(32),CC2(32)
      REAL     CTEMP,IAP
     

      DATA NBG /1,9,17,25/
      DATA NEN /8,13,22,30/
      DATA CC  /0.10,0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.10,0.20,0.25,0.30,0.50,0.00,0.00,0.00, &
                0.10,0.30,0.35,0.40,0.45,0.50,0.00,0.00,0.10,0.30,0.35,0.40,0.45,0.50,0.00,0.00/
      DATA CC0 /2.30550,2.23537,2.18219,2.10624,2.00303,1.87733,1.76312,1.67889,2.03250,1.91978,1.83842,1.72657,1.63417, &
                0.0,0.0,0.0,2.55323,2.46532,2.41896,2.36409,2.29238,2.20282,0.0,0.0,                                     &
                2.47317,2.39628,2.35477,2.30726,2.24876,2.17772,0.0,0.0/
      DATA CC1 /-0.51429,-0.50387,-0.48488,-0.45695,-0.40769,-0.32274,-0.15644,-0.06930,                                &
                -0.31583,-0.28215,-0.25543,-0.19826,-0.09100,0.0,0.0,0.0,-0.61512,-0.62257,-0.61594,-0.59857,-0.57005,  &
                -0.51599,0.0,0.0,-0.51848,-0.51202,-0.49735,-0.46541,-0.41314,-0.36803,0.0,0.0/
      DATA CC2 /-0.11750,-0.08929,-0.06589,-0.02835,0.01983,0.05754,0.00453,0.0,-0.13748,-0.07020,-0.02597,0.02633,0.0,  &
                 0.0,0.0,0.0,-0.16403,-0.11657,-0.08820,-0.05621,-0.02281,-0.01259,0.0,0.0,                              &
                 -0.17083,-0.13245,-0.11985,-0.11094,-0.11508,-0.09525,0.0,0.0/

      
      
      IREGOLD=IREG
      
      !not sure where these ideas came from, they seem contrived to reduce peak runoff.  No reference given in manual. I am adding this option
      !to bypass relations and allow for some examination.  dfy Feb 2019
      If (.not. is_true_rain_distribution) then                                               
             IF(IREG.NE.2)THEN
               IF((JULDAY.LE.121).OR.(JULDAY.GE.258))THEN  !May 1 to Sep 16,  IREG = IREG, else IREG =2
                 IREG=2
               ELSEIF(PRECIP_rain .GT. 5.08)THEN  !not sure what this is about
                 IREG=1
               ENDIF
             ENDIF
      end if
      
      
      
      IFND=0
      IAP=INABS/(THRUFL+SMELT)

      
      
      IF(IAP.LE.CC(NBG(IREG)))THEN
        EC0=CC0(NBG(IREG))
        EC1=CC1(NBG(IREG))
        EC2=CC2(NBG(IREG))
      ELSE
        do J=NBG(IREG),NEN(IREG)
          IF((IAP.LE.CC(J)).AND.(IFND.EQ.0))THEN
            CTEMP=(IAP-CC(J-1)) / (CC(J)-CC(J-1))
            EC0=CTEMP * (CC0(J)-CC0(J-1)) + CC0(J-1)
            EC1=CTEMP * (CC1(J)-CC1(J-1)) + CC1(J-1)
            EC2=CTEMP * (CC2(J)-CC2(J-1)) + CC2(J-1)
            IFND=1
          ENDIF
        end do
        IF(IFND.EQ.0)THEN
          EC0=CC0(NEN(IREG))
          EC1=CC1(NEN(IREG))
          EC2=CC2(NEN(IREG))
        ENDIF
      ENDIF


      IREG=IREGOLD

  END SUBROUTINE TMCOEF
  
         
         
  
     
    
end module erosion
    
    
    