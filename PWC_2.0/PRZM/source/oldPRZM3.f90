    module oldPRZM3
       use constants_and_variables, ONLY: NCMPTS
       real,private ::  PFRAC
       real,private ::  DRI(NCMPTS)  !PRZM3 the runoff extraction fraction
       real,private, parameter  :: PRDPTH = 2.0   !PRZM3 runoff extraction depth (cm)
       contains
    
       !subroutine PRZM3_Extraction_Initialization
       !
       !use constants_and_Variables, ONLY: delx, rncmpt
       !implicit none
       !  
       !  integer :: klin,i
       !  real :: DDLN,DELMID
       !  real :: midtot
       !
       !  KLIN = 0
       !  DDLN = 0.0
       !  do
       !    KLIN = KLIN + 1           !number of nodes representing runoff depth (prdpth)
       !    DDLN = DDLN + DELX(KLIN)  !actual numerical depth used for prdepth
       !    IF (DDLN >= PRDPTH) exit  !prdpth = 2.0 ! PRZM3 hard code
       !  end do      
       !  RNCMPT = KLIN                        !number of nodes representing runoffdeepth (prdpth)
       !  PFRAC = PRDPTH - (DDLN-DELX(KLIN))  
       !  midtot=0.0 
       !  do I=1,RNCMPT
       !     DELMID=DELX(I)/2.
       !     MIDTOT=MIDTOT+DELMID
       !     DRI(I)=.7*(1./((2.*MIDTOT)+.9))**2
       !     
       !  end do  
       !
       !end subroutine PRZM3_Extraction_Initialization
       
     
 
       SUBROUTINE SLPST0(K)
         use  constants_and_Variables, ONLY: NAPP,NC,MXCPD, NCMPTS,NCOMRZ,SoilWater,et,THETN,DELX,THETO,BD,runof,thetas,       &
         RNCMPT,ncom2,vel,PVFLUX,ADFLUX,DFFLUX,SDKFLX, SUPFLX, UPFLUX, DWRATE,DSRATE,DGRATE,KD,spestr,gamma1, &
         DISP,DKRW12,DKRW13,DKRW23,DAIR,OKH,KH,wterm,ELTERM,CONDUC,DKFLUX,ERFLUX, WOFLUX,ROFLUX,RZFLUX,TRFLUX,SRCFLX,   &
         TTRFLX,TSRCFX,X,delt,DCOFLX
       
    
        use utilities
        implicit none

!       Sets up the coefficient matrix for the solution of the soil pesticide transport equation. It then calls an equation
!       solver for the tridiagonal matrix and sets up pesticide flux terms using the new concentrations.
!       Modification date: 2/18/92 JAM
!       Further modified at AQUA TERRA Consultants to hard code the
!       pesticide extraction depth to 1 cm. 9/93

        INTEGER     K

!     K      - chemical number being simulated (1-3)
!     DKBIO  - array containing rate of biodegradation
!     PRDPTH - runoff pesticide extraction depth
!     PFRAC  - pesticide to be distributed in the remaining depthfraction
!     CMPT   - number of compartments which make up the pesticide runoff extraction depth

      INTEGER      I,J

      REAL         THAIR(NCMPTS),DGAIR(NCMPTS)
      REAL         VTERM
      REAL ::   DKBIO(3,NCMPTS)  !NOT USED
      real ::   A(NCMPTS),B(NCMPTS),C(NCMPTS),F(NCMPTS)
 
      
      real ::  RFFLUX(3,NCMPTS)
      
      integer :: ncom2m
      NCOM2M= NCOM2- 1
      
      dkbio = 0.0
      
      do J=1,NCOM2
        SRCFLX(1,J)=0.0
        THAIR(J)=THETAS(J)-THETN(J)
        IF (THAIR(J) .LT. 0.0) THAIR(J) = 0.0
!       DGAIR now includes correction for air to bulk volume
        DGAIR(J)=(THAIR(J)**(10./3)/THETAS(J)**2)*DAIR(K) * THAIR(J)
      end do
     
!     Set up coefficients for surface layer
      ! Set up tridiagonal system
      ! a(i) y(i-1)  +  b(i) y(i)  +  c(i) y(i+1) = f(i)
      ! a(1) = c(n) = 0

      J=1

      A(1)= 0.0
      B(1)= ((DISP(K,1)*THETN(1)+KH(K,1)*DGAIR(1))/(DELX(1)* DELX(1))+VEL(1)*THETN(1)/DELX(1)               &
             +(DWRATE(K,1)*THETN(1))+(DSRATE(K,1)*KD(K,1)*BD(1))+(DKBIO(K,1)*(THETN(1)+KD(K,1)*BD(1)))      &
             +(DGRATE(K,1)*THAIR(1)*KH(K,1)) +ELTERM(K)  ) *DELT                                       &
             +THETN(1) + KD(K,1)*BD(1) + THAIR(1)*KH(K,1)+CONDUC(K)*KH(K,1)*DELT/DELX(1)

!     Add runoff term based on the number of compartments which make up 1 cm depth.
       

      IF (RNCMPT .EQ. 1) THEN
        B(1) = B(1) + ((RUNOF*DRI(1))/DELX(1))
      ELSE
        B(1) = B(1) + ((RUNOF*DRI(1))/PRDPTH)
      ENDIF

      C(1)= -(DISP(K,2)*THETN(2)+KH(K,1)*DGAIR(2))*DELT/(DELX(1)* 0.5*(DELX(1)+DELX(2)))
      F(1)= (THETO(1)+KD(K,1)*BD(1)+(THETAS(1)-THETO(1))*OKH(K,1))*SPESTR(K,1) +(WTERM(K,1)*DELT/DELX(1))+SRCFLX(K,1)/DELX(1)*DELT 

!     Calculate coefficient of non-boundary soil layers

                  


      do I=2,NCOM2M
        A(I)= (-(DISP(K,I-1)*THETN(I-1)+KH(K,I-1)*DGAIR(I-1))/(DELX(I)*0.5*(DELX(I-1)+DELX(I)))-VEL(I-1)*THETN(I-1)/DELX(I))*DELT
        B(I)= ((DISP(K,I)*THETN(I)+KH(K,I)*DGAIR(I))/(DELX(I)*0.5*(DELX(I-1)+DELX(I)))                  &
              + (DISP(K,I)*THETN(I)+KH(K,I)*DGAIR(I))/(DELX(I)*0.5*(DELX(I)+DELX(I+1)))                 &
              +VEL(I)*THETN(I)/DELX(I)+(DWRATE(K,I)*THETN(I))+(DSRATE(K,I)*KD(K,I)*BD(I))               &
              +(DKBIO(K,I)*(THETN(I)+KD(K,I)*BD(I)))+ (DGRATE(K,I)*THAIR(I)*KH(K,I))                    &
              +GAMMA1(K,I)*ET(I)*THETN(I)/SoilWater(I))*DELT+THETN(I)+KD(K,I)*BD(I)+THAIR(I)*KH(K,I)
        
!       Add runoff term if current compartment number is less than or
!       equal to the number of compartments which make up 1 cm depth.
        IF (I .LT. RNCMPT) THEN
          B(I) = B(I) + RUNOF*DRI(I) /PRDPTH
          
        ELSE
          IF (I .EQ. RNCMPT)THEN
             B(I) = B(I) + ((RUNOF*DRI(I))/DELX(I))*(PFRAC/PRDPTH)
          ENDIF
        ENDIF

      C(I)= -(DISP(K,I+1)*THETN(I+1)+KH(K,I+1)*DGAIR(I+1))*DELT/(DELX(I)*0.5*(DELX(I)+DELX(I+1)))
      F(I)=(THETO(I)+KD(K,I)*BD(I)+(THETAS(I)-THETO(I))*OKH(K,I))*SPESTR(K,I)+(WTERM(K,I)*DELT/DELX(I))+SRCFLX(K,I)/DELX(I)*DELT

      end do

!     Calculate coefficients of bottom layer

      VTERM   = VEL(NCOM2) * THETN(NCOM2) / DELX(NCOM2)

      A(NCOM2)=(-(DISP(K,NCOM2M)*THETN(NCOM2M)+KH(K,NCOM2M)*DGAIR(NCOM2M))                 &
              /(DELX(NCOM2)*0.5*(DELX(NCOM2M)+DELX(NCOM2)))-VEL(NCOM2M)*THETN(NCOM2M)/DELX(NCOM2))*DELT
      
      B(NCOM2)= ((DISP(K,NCOM2)*THETN(NCOM2)+KH(K,NCOM2)*DGAIR(NCOM2))/(DELX(NCOM2)*DELX(NCOM2))+ VTERM      &
              +(DWRATE(K,NCOM2)*THETN(NCOM2))+(DKBIO(K,NCOM2)*(THETN(NCOM2)+KD(K,NCOM2)*BD(NCOM2)))         &
              +(DSRATE(K,NCOM2)*KD(K,NCOM2)*BD(NCOM2))+DGRATE(K,NCOM2)*THAIR(NCOM2)*KH(K,NCOM2))*DELT       &
              +THETN(NCOM2)+KD(K,NCOM2)*BD(NCOM2)+THAIR(NCOM2)*KH(K,NCOM2)

      C(NCOM2)= 0.0
      F(NCOM2)= (THETO(NCOM2)+KD(K,NCOM2)*BD(NCOM2)+(THETAS(NCOM2)-THETO(NCOM2))*OKH(K,NCOM2))*SPESTR(K,NCOM2)  &
               +(WTERM(K,NCOM2)*DELT/DELX(NCOM2))+SRCFLX(K,NCOM2)/DELX(NCOM2)*DELT
   
      
      CALL TRDIAG (A,B,C,X,F,NCOM2)
            
!     Calculate pesticide fluxes

      ! PVFLUX: Daily Soil Pesticide Volatilization Flux (g cm^-2 day^-1)
      PVFLUX(K,1) = -CONDUC(K)*X(1)*KH(K,1)
      IF(ABS(PVFLUX(K,1)).LT.1.E-34)PVFLUX(K,1)=0.0
      DFFLUX(K,1)=DISP(K,1)/(0.5*(DELX(1)+DELX(2)))*X(1)*THETN(1)-DISP(K,2)/(0.5*(DELX(1)+DELX(2)))*X(2)*THETN(2)
      ADFLUX(K,1)=VEL(1)*X(1)*THETN(1)

      DKFLUX(K,1)=DELX(1)*X(1)*(DWRATE(K,1)*THETN(1)+DSRATE(K,1)*BD(1)*KD(K,1)+DGRATE(K,1)*THAIR(1)*KH(K,1)     &
         +DKBIO(K,1)*(THETN(1)+KD(K,1)*BD(1)))

!*****************************************************************************     
!Note from Dirk Young October 7, 2010:  the following code appeared after version 3.2beta
!in an apparent attempt to standardize or correct a degradation issue by ORD/CEAM.  
!This code in combination with the code in Rsinp2.for (przmrd) causes a double
!counting of degradation.  This issue was fixed by removing code in routine przmrd
!See October 7, 2010 comment in that routine for additional information.



      if(k.eq.1)then
        trflux(1,1)=(dkrw12(1)+dkrw13(1))*dkflux(1,1)
        srcflx(1,1)=0.0
        srcflx(2,1)=dkrw12(1)*dkflux(1,1) 
        srcflx(3,1)=dkrw13(1)*dkflux(1,1)
        dkflux(1,1)=dkflux(1,1)-trflux(1,1)             
      elseif(k.eq.2)then
        trflux(2,1)=dkrw23(1)*dkflux(2,1)
        srcflx(3,1)=srcflx(3,1)+trflux(2,1)
        dkflux(2,1)=dkflux(2,1)-trflux(2,1)
      endif
!*******************************************************************

      IF (RNCMPT .EQ. 1) THEN
        RFFLUX(K,1) =RUNOF*DRI(1)*X(1)
      ELSE
        RFFLUX(K,1) =RUNOF*X(1)*DRI(1)*(DELX(1)/PRDPTH)
      ENDIF
      ERFLUX(K)   =ELTERM(K)*DELX(1)*X(1)


      
      
      do I=2,NCOM2M
        RFFLUX(K,I) = 0.0
        IF (I .LT. RNCMPT) THEN
          RFFLUX(K,I)=RUNOF*DRI(I)*X(I)*(DELX(I)/PRDPTH)
        ELSE
          IF (I.EQ.RNCMPT) RFFLUX(K,I)=RUNOF*DRI(I)*X(I)*(PFRAC/PRDPTH)
        ENDIF
        PVFLUX(K,I)=DGAIR(I)*KH(K,I)/(0.5*(DELX(I)+DELX(I+1)))*X(I)-DGAIR(I+1)*KH(K,I+1)/(0.5*(DELX(I)+DELX(I+1)))*X(I+1)
        IF(ABS(PVFLUX(K,I)).LT.1.E-34)PVFLUX(K,I)=0.0
        DFFLUX(K,I)=DISP(K,I)/(0.5*(DELX(I)+DELX(I+1)))*THETN(I)*X(I)-DISP(K,I+1)/(0.5*(DELX(I)+DELX(I+1)))*THETN(I+1)*X(I+1)
        ADFLUX(K,I)=VEL(I)*X(I)*THETN(I)
        
!        LTFLUX(K,I)=OUTFLO(I)*X(I)
        
        DKFLUX(K,I)=DELX(I)*X(I)*(DWRATE(K,I)*THETN(I)+DSRATE(K,I)*BD(I)*KD(K,I)+DGRATE(K,I)*THAIR(I)*KH(K,I)   &
                  +DKBIO(K,I)*(THETN(I)+KD(K,I)*BD(I)))
        UPFLUX(K,I)=GAMMA1(K,I)*ET(I)*X(I)
        if(k.eq.1)then
            trflux(1,i)=(dkrw12(i)+dkrw13(i))*dkflux(1,i)
            srcflx(1,i)=0.0
            srcflx(2,i)=dkrw12(i)*dkflux(1,i)
            srcflx(3,i)=dkrw13(i)*dkflux(1,i)
            dkflux(1,i)=dkflux(1,i)-trflux(1,i)
        elseif(k.eq.2)then
           trflux(2,i)=dkrw23(i)*dkflux(2,i)
           srcflx(3,i)=srcflx(3,i)+trflux(2,i)
           dkflux(2,i)=dkflux(2,i)-trflux(2,i)
        endif
      end do

      
      RZFLUX(K)= DISP(K,NCOMRZ)/(0.5*(DELX(NCOMRZ)+DELX(NCOMRZ+1)))*THETN(NCOMRZ)*X(NCOMRZ)-DISP(K,NCOMRZ+1)/(0.5*(DELX(NCOMRZ+1) &
       +DELX(NCOMRZ)))*THETN(NCOMRZ+1)*X(NCOMRZ+1)+(VEL(NCOMRZ)*X(NCOMRZ)*THETN(NCOMRZ))
      RFFLUX(K,NCOM2)=0.
      DFFLUX(K,NCOM2)=0.
      PVFLUX(K,NCOM2)=0.
      UPFLUX(K,NCOM2)=0.
      ADFLUX(K,NCOM2) = VTERM * DELX(NCOM2) * X(NCOM2)
      
!      LTFLUX(K,NCOM2)=OUTFLO(NCOM2)*X(NCOM2)
      
      DKFLUX(K,NCOM2)=DELX(NCOM2)*X(NCOM2)*(DWRATE(K,NCOM2)*THETN(NCOM2)+DSRATE(K,NCOM2)*BD(NCOM2)*KD(K,NCOM2)   &
                 +DGRATE(K,NCOM2)*THAIR(NCOM2)*KH(K,NCOM2)+DKBIO(K,NCOM2)*(THETN(NCOM2)+KD(K,NCOM2)*BD(NCOM2)))
      if(k.eq.1)then
        trflux(1,ncom2)=(dkrw12(ncom2)+dkrw13(ncom2))*dkflux(1,ncom2)
        srcflx(1,ncom2)=0.0
        srcflx(2,ncom2)=dkrw12(ncom2)*dkflux(1,ncom2)
        srcflx(3,ncom2)=dkrw13(ncom2)*dkflux(1,ncom2)
        dkflux(1,ncom2)=dkflux(1,ncom2)-trflux(1,ncom2)
      elseif(k.eq.2)then
        trflux(2,ncom2)=dkrw23(ncom2)*dkflux(2,ncom2)
        srcflx(3,ncom2)=srcflx(3,ncom2)+trflux(2,ncom2)
        dkflux(2,ncom2)=dkflux(2,ncom2)-trflux(2,ncom2)
      endif

!     Calculate core flux values.
!     Multiply internal units of GR/CM**2 by 10**5 so output
!     is expressed in units of KG/HA (as in the input).
!
      DCOFLX(K) = DISP(K,NCOM2)/DELX(NCOM2)*THETN(NCOM2)*X(NCOM2)- DISP(K,NCOM2)/DELX(NCOM2)*THETN(NCOM2)*X(NCOM2)   &
               + (VEL(NCOM2)*X(NCOM2)*THETN(NCOM2))
      DCOFLX(K) = DCOFLX(K) * 1.0E5

!     Accumulate fluxes from soil layers for output

      WOFLUX(K)= 0.0
      ROFLUX(K)= 0.0
      SUPFLX(K)= 0.0
      SDKFLX(K)= 0.0
      TTRFLX(K)= 0.0
      TSRCFX(K)= 0.0

      do I=1,NCOM2
         WOFLUX(K)= WOFLUX(K)+WTERM(K,I)
         ROFLUX(K)= ROFLUX(K)+RFFLUX(K,I)
         SDKFLX(K)= SDKFLX(K)+DKFLUX(K,I)
         SUPFLX(K)= SUPFLX(K)+UPFLUX(K,I)
         TTRFLX(K)= TTRFLX(K)+TRFLUX(K,I)
         TSRCFX(K)= TSRCFX(K)+SRCFLX(K,I)
      end do

       END SUBROUTINE SLPST0

       

        !******************************************************************************************* 
    Subroutine Calculate_Runoff_PRZM3(curvn)
       use constants_and_Variables, ONLY:thrufl, smelt,RUNOF,precip,inabs, EchoFileUnit
       implicit none
       real, intent(in) :: curvn
       logical :: Issue_Error
       Issue_Error = .False.
       ! the constant 0.508 is derived from 0.2 * 2.54 cm/in and 0.2 is from INABS = 0.2 * S, where S is (1000./CURVN-10.).
       ! INABS: Initial Abstraction of Water from Potential Surface Runoff

       !Atempts to make initial abstraction a function of canopy holdup, 
       !Violation since Curve Number Tables are based on assumption of .02*S
       !A more appropriate way would be to make whatever precip is a vailable after runoff avaialble to the canopy
       !THe Original NRCS Curve Number nmethod already accounts for canopy holdup.  -DFY

       INABS = AMAX1(0.508* (1000./CURVN-10.),PRECIP-THRUFL)  
       IF (PRECIP+SMELT .GT. 0.0) THEN
         IF (PRECIP+SMELT .GT. INABS) RUNOF= (PRECIP+SMELT-INABS)**2/ (PRECIP+ SMELT+ (4* INABS))
         IF (THRUFL+SMELT .LT. RUNOF) THEN
           RUNOF = THRUFL+SMELT
           Issue_Error = .True.
         ENDIF
       ENDIF
            
       If (Issue_Error) WRITE (EchoFileUnit,*)      &
          'PRZM attempts to calculate initial abstraction from the canopy (nonstandard) and fails in this case.'
    end  Subroutine Calculate_Runoff_PRZM3
    
    
    
       Subroutine Curve_Number_PRZM3(twlvl,curvn)
     use constants_and_Variables, ONLY:thrufl,theth,smelt,RUNOF,precip,ncrop,inabs, CN_input, CN_index,EchoFileUnit
     use curve_number_table
     
     implicit none
      
     real,intent(in) :: twlvl
     real,intent(out) :: curvn
     
     logical :: Issue_Error
     Issue_Error = .False.
     
     CURVN = CN_input(NCROP,CN_index,2)+ (TWLVL-THETH)* (CN_input(NCROP,CN_index,3)-CN_input(NCROP,CN_index,2))/ THETH
     
     
     !Why was this done?  PRZM doesnt allow CN3 to be exceeded anyway with or without this
     IF (CURVN .GT. CN_input(NCROP,CN_index,3)) CURVN = CN_input(NCROP,CN_index,3)
    
     IF (TWLVL .LT. THETH) CURVN= CN_input(NCROP,CN_index,1)+TWLVL*(  CN_input(NCROP,CN_index,2)-CN_input(NCROP,CN_index,1)  )/THETH


      ! the constant 0.508 is derived from 0.2 * 2.54 cm/in
      ! and 0.2 is from INABS = 0.2 * S, where S is (1000./CURVN-10.) below.
      ! INABS: Initial Abstraction of Water from Potential Surface Runoff

      
     !Atempts to make initial abstraction a function of canopy holdup, 
     !Violation since Curve Number Tables are based on assumption of .02*S
     !A more appropriate way would be to make whatever precip is a vailable after runoff avaialble to the canopy
     !THe Original NRCS Curve Number nmethod already accounts for canopy holdup.  -DFY
     

     INABS = AMAX1(0.508* (1000./CURVN-10.),PRECIP-THRUFL)  
     IF (PRECIP+SMELT .GT. 0.0) THEN
       IF (PRECIP+SMELT .GT. INABS) RUNOF= (PRECIP+SMELT-INABS)**2/ (PRECIP+ SMELT+ (4* INABS))
       
       IF (THRUFL+SMELT .LT. RUNOF) THEN
         RUNOF = THRUFL+SMELT
         Issue_Error = .True.
       ENDIF
     ENDIF

                
         If (Issue_Error) WRITE (EchoFileUnit,*)      &
         'PRZM3 attempts to calculate initial abstraction from the canopy (nonstandard) and fails in this case.'

   End Subroutine Curve_Number_PRZM3

    end module oldPRZM3