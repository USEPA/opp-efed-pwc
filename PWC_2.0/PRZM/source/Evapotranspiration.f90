module evapotranspiration
    implicit none
    contains

    
    SUBROUTINE EVPOTR
      use  constants_and_Variables, ONLY:  air_TEMP, pfac,sttdet ,PEVP, &
           wiltpoint_water,fieldcap_water,soilwater,cint,EvapoTran,DELX,cevap,evapo_root_node_daily,TDET
      implicit none
!     Computes daily potential evapotranspiration,
!     canopy evaporation, and actual evapotranspiration from each
!     soil layer
      
!=========================================================================================================================================================      
!      REAL ::      FRAC(evapo_root_node_daily)
!      REAL         PET,PETP,ANUM,DENOM,AW,TSW
!      Real         TWP,TFRAC,RNSUM,STDELX,EDEPTH
!      INTEGER      ITEMP,I,NSUM
!
!!     Compute potential evapotranspiration
!
!      ITEMP= INT(air_TEMP)
!      IF (ITEMP.LT.1)  ITEMP= 1
!      IF (ITEMP.GT.40) ITEMP= 40
!      
!      !pan evaporation now forced. no Hamon option since units are wrong
!      PET= PEVP*PFAC
!      
!!     Subtract canopy evaporation from potential evapotranspiration
!
!      PETP= AMAX1(0.0,PET-CINT)
!      IF (PET .GT. CINT) THEN
!        CEVAP= CINT
!      ELSE
!        CEVAP= PET
!      ENDIF
!      CINT= CINT-CEVAP
!
!!     Compute evapotranspiration from each soil layer
!      EDEPTH = 5.0
!      STDELX = delx(1)
!      STTDET = 0.0
!
!      TDET = 0.0
!      EvapoTran= 0.0
!      ANUM = 0.0
!      DENOM= 0.0
!      FRAC = 0.0
!  
!      do I= 1, evapo_root_node_daily
!        ANUM = ANUM  + AMAX1(0.0,soilwater(I)- wiltpoint_water(I))
!        DENOM= DENOM + AMAX1(0.0,fieldcap_water(I)- wiltpoint_water(I))
!      end do
!      
!      AW   = ANUM/DENOM
!      IF (AW.LT.0.6) PETP= AW*PETP/0.6
!      NSUM = 0
!      TSW  = 0.0
!      TWP  = 0.0
!   
!      do I= 1, evapo_root_node_daily
!        NSUM = NSUM + I
!        TSW  = TSW + SoilWater(I)
!        TWP  = TWP + wiltpoint_water(I)       
!      end do
!      
! 
!      RNSUM= FLOAT(NSUM)
!      IF (RNSUM .GT. 0.00 .AND. TSW .GT. TWP) THEN
!        TFRAC= 0.0
!        
!        do I= 1, evapo_root_node_daily
!          FRAC(I)=FLOAT(evapo_root_node_daily-I+1)*(SoilWater(I)-wiltpoint_water(I))/(RNSUM*(TSW-TWP))
!          TFRAC  =TFRAC + FRAC(I) 
!        end do

!============================================================================================================================================================
!############################################################################################################################################################      
      REAL ::      FRAC(evapo_root_node_daily), DEP(evapo_root_node_daily) !!!add DEP() as the depth of each compartment (top)!!!
      REAL         PET,PETP,ANUM,DENOM,AW,TSW
      Real         TWP,TFRAC,STDELX,EDEPTH, depth, DEPtotal !!!add depth to calculate top of each compartment, DEPtotal to calculate sum of DEP()!!!
      INTEGER      ITEMP,I

!     Compute potential evapotranspiration

      ITEMP= INT(air_TEMP)
      IF (ITEMP.LT.1)  ITEMP= 1
      IF (ITEMP.GT.40) ITEMP= 40
      
      !pan evaporation now forced. no Hamon option since units are wrong
      PET= PEVP*PFAC
      
!     Subtract canopy evaporation from potential evapotranspiration

      PETP= AMAX1(0.0,PET-CINT)
      IF (PET .GT. CINT) THEN
        CEVAP= CINT
      ELSE
        CEVAP= PET
      ENDIF
      CINT= CINT-CEVAP

!     Compute evapotranspiration from each soil layer
      EDEPTH = 5.0
      STDELX = delx(1)
      STTDET = 0.0

      TDET = 0.0
      EvapoTran= 0.0
      ANUM = 0.0
      DENOM= 0.0
      FRAC = 0.0
  
      do I= 1, evapo_root_node_daily
        ANUM = ANUM  + AMAX1(0.0,soilwater(I)- wiltpoint_water(I))
        DENOM= DENOM + AMAX1(0.0,fieldcap_water(I)- wiltpoint_water(I))
      end do
      
      AW   = ANUM/DENOM
      IF (AW.LT.0.6) PETP= AW*PETP/0.6
      TSW  = 0.0
      TWP  = 0.0
      DEPtotal = 0.0  !!! initial DEPtotal !!!
      depth = 0.0     !!! initial depth !!!
      
      do I= 1, evapo_root_node_daily 
        DEP(I) = depth               !!! DEP(I) calculation !!!
        DEPtotal = DEPtotal + DEP(I) !!! DEP calculation !!!
        TSW  = TSW + SoilWater(I)
        TWP  = TWP + wiltpoint_water(I)
        depth = depth + DELX(I)      !!! depth calculation !!!
      end do   

      IF ((evapo_root_node_daily*depth-DEPtotal) .GT. 0.00 .AND. TSW .GT. TWP) THEN
        TFRAC= 0.0        
        do I= 1, evapo_root_node_daily
          FRAC(I)=(DEP(I)-depth)/(DEPtotal - evapo_root_node_daily*depth)*(SoilWater(I)-wiltpoint_water(I))/(TSW-TWP)  !!! depth related FRAC calculation !!!
          TFRAC  =TFRAC + FRAC(I)
        end do
!###########################################################################################################################################################
    
        DO  I = 1, evapo_root_node_daily
          EvapoTran(I) = AMIN1((SoilWater(I)-wiltpoint_water(I)),PETP*FRAC(I)/TFRAC)
          EvapoTran(I) = AMAX1(EvapoTran(I),0.0)

          TDET  = TDET + EvapoTran(I)
!         This code is added by C.S.Raju to estimate the evaporation
!         through the top 5cm depth of soil.
          IF (STDELX .LE. EDEPTH)THEN
            STTDET = STTDET + EvapoTran(I)  !used in temperature routine
            STDELX = STDELX + DELX(I)
          END IF
        end do
      ENDIF
          
    END SUBROUTINE EVPOTR
    
 end module  evapotranspiration
    
    
!    
!    module evapotranspiration
!    implicit none
!    contains
!
!    
!    SUBROUTINE EVPOTR
!      use  constants_and_Variables, ONLY:  air_TEMP, pfac,sttdet ,PEVP, &
!           wiltpoint_water,fieldcap_water,soilwater,cint,EvapoTran,DELX,cevap,evapo_root_node_daily,TDET 
!
!      implicit none
!!     Computes daily potential evapotranspiration,
!!     canopy evaporation, and actual evapotranspiration from each
!!     soil layer
!   
!      REAL ::      FRAC(evapo_root_node_daily)
!      REAL         PET,PETP,ANUM,DENOM,AW,TSW
!      Real         TWP,TFRAC,RNSUM,STDELX,EDEPTH
!      INTEGER      ITEMP,I,NSUM
!
!!     Compute potential evapotranspiration
!
!      ITEMP= INT(air_TEMP)
!      IF (ITEMP.LT.1)  ITEMP= 1
!      IF (ITEMP.GT.40) ITEMP= 40
!      
!      !pan evaporation now forced. no Hamon option since units are wrong
!      PET= PEVP*PFAC
!      
!!     Subtract canopy evaporation from potential evapotranspiration
!
!      PETP= AMAX1(0.0,PET-CINT)
!      IF (PET .GT. CINT) THEN
!        CEVAP= CINT
!      ELSE
!        CEVAP= PET
!      ENDIF
!      CINT= CINT-CEVAP
!
!!     Compute evapotranspiration from each soil layer
!      EDEPTH = 5.0
!      STDELX = delx(1)
!      STTDET = 0.0
!
!      TDET = 0.0
!      EvapoTran= 0.0
!      ANUM = 0.0
!      DENOM= 0.0
!      FRAC = 0.0
!  
!      do I= 1, evapo_root_node_daily
!        ANUM = ANUM  + AMAX1(0.0,soilwater(I)- wiltpoint_water(I))
!        DENOM= DENOM + AMAX1(0.0,fieldcap_water(I)- wiltpoint_water(I))
!      end do
!      
!      AW   = ANUM/DENOM
!      IF (AW.LT.0.6) PETP= AW*PETP/0.6
!      NSUM = 0
!      TSW  = 0.0
!      TWP  = 0.0
!   
!      do I= 1, evapo_root_node_daily
!        NSUM = NSUM + I
!        TSW  = TSW + SoilWater(I)
!        TWP  = TWP + wiltpoint_water(I)   
!
!        
!      end do
!
!      
!      
! 
!      RNSUM= FLOAT(NSUM)
!      IF (RNSUM .GT. 0.00 .AND. TSW .GT. TWP) THEN
!        TFRAC= 0.0
!        
!        do I= 1, evapo_root_node_daily
!          FRAC(I)=FLOAT(evapo_root_node_daily-I+1)*(SoilWater(I)-wiltpoint_water(I))/(RNSUM*(TSW-TWP))
!          TFRAC  =TFRAC + FRAC(I) 
!        end do
!        
!        DO  I = 1, evapo_root_node_daily
!          EvapoTran(I) = AMIN1((SoilWater(I)-wiltpoint_water(I)),PETP*FRAC(I)/TFRAC)
!          EvapoTran(I) = AMAX1(EvapoTran(I),0.0)
!          
!          TDET  = TDET + EvapoTran(I)
!!         This code is added by C.S.Raju to estimate the evaporation
!!         through the top 5cm depth of soil.
!          IF (STDELX .LE. EDEPTH)THEN
!            STTDET = STTDET + EvapoTran(I)  !used in temperature routine
!            STDELX = STDELX + DELX(I)
!          END IF
!        end do
!      ENDIF
!          
!    END SUBROUTINE EVPOTR
!    
! end module evapotranspiration