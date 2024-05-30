Module Output_File
  implicit none
  contains
  
  subroutine write_outputfile_header
      !called immediately after reading input file
      use  constants_and_Variables, ONLY:TimeSeriesUnit,Version_Number,PLNAME,chem_id,NPLOTS
      integer :: i
      
      WRITE(TimeSeriesUnit,'(A43,1X,F5.3)')'United States of America, EPA PRZM5 Version', Version_Number  
      WRITE(TimeSeriesUnit,*)      
     ! WRITE(TimeSeriesUnit,'(19x, 100(A4,I1,9X))') (PLNAME(I),chem_id(I),I=1,NPLOTS)     !Header List
  
      WRITE(TimeSeriesUnit,'(A4,1x,A2,1x,A2,9x, 100(A4,I1,9X))') "Year","Mo","Dy", (PLNAME(I),chem_id(I),I=1,NPLOTS)     !Header List
      
  end subroutine write_outputfile_header
    
    

  SUBROUTINE write_outputfile
    use  constants_and_Variables, ONLY:TimeSeriesUnit, precipitation, snowfl,soil_temp,THRUFL,IRRR,SoilWater,cint,EvapoTran,delx, &
    theta_end, CEVAP ,runof ,curve_number_daily,theta_sat,ainf,ncom2,snow,sedl,TDET,PVFLUX,DFFLUX,adflux,sdkflx,SUPFLX,UPFLUX,kd_new, &
    conc_total_per_water, mass_in_compartment, mass_in_compartment2,HEIGHT,  &
    conc_porewater,SOILAP, FOLPST,Foliar_degrade_loss,new_henry,Foliar_volatile_loss,plant_app, &
    NCHEM,TCNC,DKFLUX,ERFLUX, WOFLUX,ROFLUX,const,OUTPUJ,    &
    OUTPJJ,chem_id,NPLOTS, ARG,IARG2,PLNAME,MODE,DCOFLX,max_number_plots,Version_Number,julday1900, EchoFileUnit
     
    
    use utilities
    
!   Outputs user specified time series to time series plotting files
    implicit none

    integer :: start_compartment, end_compartment
    INTEGER  I,K
    real ::  OUTPUT(max_number_plots)
    REAL ::  PNBRN(max_number_plots)
    real ::  PRTBUF(max_number_plots)

    REAL   RMULT,XSOIL(3)
    REAL   TTTOT,DPTOT

    CHARACTER(len=4) :: TSUM,TAVE,TSER,TCUM

    integer :: current_year,current_month,current_day
    
    TSUM = 'TSUM'
    TAVE = 'TAVE'
    TCUM = 'TCUM'
    TSER = 'TSER'
      
      !DATA ,THET/'THET'/,PRCP/'PRCP'/,SNOF/'SNOF'/,THRF/'THRF'/,INFL/'INFL'/,RUNF/'RUNF'/,  &
      !     CEVP/'CEVP'/,SLET/'SLET'/,TETD/'TETD'/,ESLS/'ESLS'/,FPST/'FPST'/,TPST/'TPST'/,SPST/'SPST'/,TPAP/'TPAP'/,  &
      !     FPDL/'FPDL'/,WFLX/'WFLX'/,DFLX/'DFLX'/,AFLX/'AFLX'/,DKFX/'DKFX'/,UFLX/'UFLX'/,RFLX/'RFLX'/,EFLX/'EFLX'/,  &
      !     RZFX/'RZFX'/,TUPX/'TUPX'/,TDKF/'TDKF'/,SNOP/'SNOP'/,STMP/'STMP'/,CHGT/'CHGT'/,  &
      !     VFLX/'VFLX'/,PCNC/'PCNC'/,FPVL/'FPVL'/,COFX/'COFX'/,IRRG/'IRRG'/,MASS/'MASS'/
      !
      !DATA ACON/'ACON'/,GCON/'GCON'/,TCON/'TCON'/,DLYS/'DLYS'/,CMSS/'CMSS'/,INCS/'INCS'/,CURV/'CURV'/
      !INTS = water on canopy
    
    


    
    
    
    DO K=1,NCHEM
      XSOIL(K) = 0.00
      DO i = 1, NCOM2
        XSOIL(K) = XSOIL(K) + SOILAP(K,i)  !total soil application
      END DO
    END DO

! ******************************************************************

   OUTPUT= 0.   
      
      
   DO i=1,NPLOTS

       
      start_compartment= ARG(i)
     end_compartment = IARG2(i)
      
      !ID1 = 1
      !IF (chem_id(I) == 2) ID1 = 2
      !IF (chem_id(I) == 3) ID1 = 3

      DPTOT=0.0
      TTTOT=0.0

      IF ((MODE(i).EQ.TAVE).OR.(MODE(i).EQ.TSUM))THEN
          select case (PLNAME(i))
          case('SWTR')!   Water storages  (units of CM)
              do K =start_compartment,end_compartment
                    IF(MODE(I).EQ.TAVE)THEN
                      TTTOT= TTTOT+SoilWater(K)*DELX(K)
                    ELSE
                      TTTOT= TTTOT+SoilWater(K)
                    ENDIF
              end do
              PNBRN(I)=TTTOT
          case('INFL')
              do K =start_compartment,end_compartment
                 IF(MODE(I).EQ.TAVE)THEN
                    TTTOT= TTTOT+AINF(K)*DELX(K)
                 ELSE
                    TTTOT= TTTOT+AINF(K)
                 ENDIF           
              end do                   
              PNBRN(I)=TTTOT       
          case('SLET') 
              do K =start_compartment,end_compartment
                 IF(MODE(I).EQ.TAVE)THEN
                   TTTOT= TTTOT+EvapoTran(K)*DELX(K)
                 ELSE
                   TTTOT= TTTOT+EvapoTran(K)
                 ENDIF
              end do
              PNBRN(I)=TTTOT
                   
          case('STMP')
              do K =start_compartment,end_compartment
                     IF(MODE(I).EQ.TAVE)THEN
                       TTTOT=TTTOT+soil_temp(K)*DELX(K)
                     ELSE
                       TTTOT=TTTOT+soil_temp(K)
                     ENDIF
              end do
              PNBRN(I)=TTTOT    
          case ('TPST')  !Pesticide storages (units of GRAMS/(CM**2)             
              DO K =start_compartment,end_compartment
                IF(MODE(I).EQ.TAVE)THEN
                  TTTOT=TTTOT + (conc_total_per_water(chem_id(i),K)*DELX(K)*theta_end(K))   *DELX(K)
                ELSE
                  TTTOT=TTTOT + (conc_total_per_water(chem_id(i),K)*DELX(K)*theta_end(K))        
                ENDIF
              END DO
              PNBRN(I)=TTTOT                      
          case('MASS')              
              DO K =start_compartment,end_compartment
                  IF(MODE(I).EQ.TAVE)THEN
                    TTTOT=TTTOT + mass_in_compartment(chem_id(i),K)
                  ELSE
                    TTTOT=TTTOT + mass_in_compartment(chem_id(i),K)                    
                  ENDIF
              END Do
              PNBRN(I)=TTTOT               
          case('MAS2' )             
              DO K =start_compartment,end_compartment
                IF(MODE(I).EQ.TAVE)THEN
                  TTTOT=TTTOT + mass_in_compartment2(chem_id(i),K)
                ELSE
                  TTTOT=TTTOT + mass_in_compartment2(chem_id(i),K)          
                ENDIF
              END DO
              PNBRN(I)=TTTOT                    
          case('SPST')             
              DO  K =start_compartment,end_compartment
                IF(MODE(I).EQ.TAVE)THEN
                  TTTOT=TTTOT+( conc_porewater(chem_id(i),K)*DELX(K)*theta_end(K))*DELX(K)
                ELSE
                  TTTOT=TTTOT+( conc_porewater(chem_id(i),K)*DELX(K)*theta_end(K))
                ENDIF
              end do
              PNBRN(I)=TTTOT                     
          case('DFLX')             
              DO  K =start_compartment,end_compartment
                IF(MODE(I).EQ.TAVE)THEN
                  TTTOT= TTTOT+DFFLUX(chem_id(i),K)*DELX(K)
                ELSE
                  TTTOT= TTTOT+DFFLUX(chem_id(i),K)
                ENDIF
              end do
              PNBRN(I)=TTTOT                  
          case('AFLX')             
              DO K =start_compartment,end_compartment
                IF(MODE(I).EQ.TAVE)THEN
                  TTTOT= TTTOT+ADFLUX(chem_id(i),K)*DELX(K)
                ELSE
                  TTTOT= TTTOT+ADFLUX(chem_id(i),K)
                ENDIF
              end do
              PNBRN(I)=TTTOT                                     
          case('DKFX')              
              DO K =start_compartment,end_compartment
                IF(MODE(I).EQ.TAVE)THEN
                  TTTOT= TTTOT+DKFLUX(chem_id(i),K)*DELX(K)
                ELSE
                  TTTOT= TTTOT+DKFLUX(chem_id(i),K)
                ENDIF
              end do
              PNBRN(I)=TTTOT                   
          case('UFLX')              
              DO K =start_compartment,end_compartment
                IF(MODE(I).EQ.TAVE)THEN
                  TTTOT= TTTOT+UPFLUX(chem_id(i),K)*DELX(K)
                ELSE
                  TTTOT= TTTOT+UPFLUX(chem_id(i),K)
                ENDIF
              end do
              PNBRN(I)=TTTOT                     
          case('DCON')             
              DO K =start_compartment,end_compartment
                  
                IF(MODE(I).EQ.TAVE)THEN
                  TTTOT= TTTOT+( conc_porewater(chem_id(i),K)*1.E6)*DELX(K)
                ELSE
                  TTTOT= TTTOT+( conc_porewater(chem_id(i),K)*1.E6)
                ENDIF
              end do         
              PNBRN(I)=TTTOT  
          case('ACON')             
             DO K =start_compartment,end_compartment
               IF(MODE(I).EQ.TAVE)THEN
                 TTTOT= TTTOT+(( conc_porewater(chem_id(i),K)*1.E6)*kd_new(chem_id(i),K))*DELX(K)
               ELSE
                 TTTOT= TTTOT+(( conc_porewater(chem_id(i),K)*1.E6)*kd_new(chem_id(i),K))
               ENDIF
             end do            
             PNBRN(I)=TTTOT                                   
          case('GCON') 
             DO K =start_compartment,end_compartment
               IF(MODE(I).EQ.TAVE)THEN
                 TTTOT= TTTOT+((( conc_porewater(chem_id(i),K)*1.E6)*new_henry(chem_id(i),K)))*DELX(K)
               ELSE
                 TTTOT= TTTOT+((( conc_porewater(chem_id(i),K)*1.E6)*new_henry(chem_id(i),K)))
               ENDIF
             end do  
             PNBRN(I)=TTTOT                                       
          case('TCON')      
             do K =start_compartment,end_compartment
               IF(MODE(I).EQ.TAVE)THEN
                 TTTOT= TTTOT+( conc_porewater(chem_id(i),K)*1.E6*theta_end(K)+           &
                                conc_porewater(chem_id(i),K)*1.E6*kd_new(chem_id(i),K)+   &
                                conc_porewater(chem_id(i),K)*1.E6*new_henry(chem_id(i),K)*(theta_sat(K)-theta_end(K)))*DELX(K)
               ELSE
                 TTTOT= TTTOT+( conc_porewater(chem_id(i),K)*1.E6*theta_end(K)+           &
                                conc_porewater(chem_id(i),K)*1.E6*kd_new(chem_id(i),K)+   &
                                conc_porewater(chem_id(i),K)*1.E6*new_henry(chem_id(i),K)*(theta_sat(K)-theta_end(K)))
               ENDIF
             end do
             PNBRN(I)=TTTOT        
          case default       
          end select
          
          
      ELSE  !DAILY TIME SERIES    
        !swtr with tser only gives time series for single point
        IF (PLNAME(I) == 'SWTR') PNBRN(I)= SoilWater(start_compartment)

        IF (PLNAME(I) == 'THET') PNBRN(I)= theta_end(start_compartment) ! Soil water storage (dimensionless)
        
!       Water fluxes (units of CM/DAY)
        IF (PLNAME(I) == 'INFL') PNBRN(I)= AINF(start_compartment)
        IF (PLNAME(I) == 'SLET') PNBRN(I)= EvapoTran(start_compartment)
        IF (PLNAME(I) == 'STMP') PNBRN(I)= soil_temp(start_compartment)         !     Soil temperature (oC)
                 
        ! Pesticide storages (units of GRAMS/(CM**2)
        IF (PLNAME(I) == 'TPST') PNBRN(I)= conc_total_per_water(chem_id(i),start_compartment)*   &
                                           DELX(start_compartment)*theta_end(start_compartment) 
        
        IF (PLNAME(I) == 'MASS') PNBRN(I)= mass_in_compartment(chem_id(i),start_compartment)
        IF (PLNAME(I) == 'SPST') PNBRN(I)= conc_porewater(chem_id(i),start_compartment)*   &
                                           theta_end(start_compartment)*DELX(start_compartment)
          
        !  Pesticide fluxes (units of GRAMS/(CM**2-DAY)
        IF (PLNAME(I)=='DFLX') PNBRN(I)=DFFLUX(chem_id(i),start_compartment)
        IF (PLNAME(I)=='AFLX') PNBRN(I)=ADFLUX(chem_id(i),start_compartment)
        IF (PLNAME(I)=='DKFX') PNBRN(I)=DKFLUX(chem_id(i),start_compartment)
        IF (PLNAME(I)=='UFLX') PNBRN(I)=UPFLUX(chem_id(i),start_compartment)
        
        IF (PLNAME(I)=='DCON') PNBRN(I)=conc_porewater(chem_id(i),start_compartment)*1.E6
        IF (PLNAME(I)=='ACON') PNBRN(I)=conc_porewater(chem_id(i),start_compartment)*1.E6 *kd_new(chem_id(i),start_compartment)
        IF (PLNAME(I)=='GCON') PNBRN(I)=conc_porewater(chem_id(i),start_compartment)*1.E6*new_henry(chem_id(i),start_compartment)
        
        IF (PLNAME(I)=='TCON') PNBRN(I)=conc_porewater(chem_id(i),start_compartment)*1.E6*theta_end(start_compartment)+ &
                                        conc_porewater(chem_id(i),start_compartment)*1.E6*kd_new(chem_id(i),start_compartment)+  &
                                        conc_porewater(chem_id(i),start_compartment)*1.E6*new_henry(chem_id(i),start_compartment)* &
                                        (theta_sat(start_compartment)-theta_end(start_compartment))
      ENDIF   
        
 
      
      ! Water storages  (units of CM)
      IF (PLNAME(I) .EQ. 'INTS') PNBRN(I)= CINT
      IF (PLNAME(I) .EQ. 'SNOP') PNBRN(I)= SNOW
      
      ! Water fluxes (units of CM/DAY)
      IF (PLNAME(I) .EQ. 'PRCP') PNBRN(I)= precipitation
      IF (PLNAME(I) .EQ. 'IRRG') PNBRN(I)= IRRR
      IF (PLNAME(I) .EQ. 'SNOF') PNBRN(I)= SNOWFL
      IF (PLNAME(I) .EQ. 'THRF') PNBRN(I)= THRUFL
      IF (PLNAME(I) .EQ. 'RUNF') PNBRN(I)= RUNOF
        
      IF (PLNAME(I) .EQ. 'CEVP') PNBRN(I)= CEVAP
      IF (PLNAME(I) .EQ. 'TETD') PNBRN(I)= TDET
      
      ! IF (PLNAME(I) .EQ. OUTF) PNBRN(I)= OUTFLOW
      
      IF (PLNAME(I) .EQ. 'CURV') PNBRN(I)= curve_number_daily
      IF (PLNAME(I) .EQ. 'ESLS') PNBRN(I)= SEDL      ! Sediment flux (metric tons)
      IF (PLNAME(I) .EQ. 'CHGT') PNBRN(I)= HEIGHT    !  Crop height (CM)
      
      !  Pesticide storages (units of GRAMS/(CM**2)
      IF (PLNAME(I) .EQ. 'FPST') PNBRN(I)=FOLPST(chem_id(i))
      IF (PLNAME(I) .EQ. 'PCNC') PNBRN(I)=TCNC(chem_id(i))
      IF (PLNAME(I) .EQ. 'TPAP') PNBRN(I)= XSOIL(chem_id(i)) + plant_app   
      
      
      IF (PLNAME(I) .EQ. 'FPDL') PNBRN(I)= Foliar_degrade_loss(chem_id(i))
      IF (PLNAME(I) .EQ. 'WFLX') PNBRN(I)= WOFLUX(chem_id(i))
      IF (PLNAME(I) .EQ. 'RFLX') PNBRN(I)= ROFLUX(chem_id(i))
      IF (PLNAME(I) .EQ. 'EFLX') PNBRN(I)= ERFLUX(chem_id(i))
  !    IF (PLNAME(I) .EQ. 'RZFX') PNBRN(I)= RZFLUX(chem_id(i))  redundant
      IF (PLNAME(I) .EQ. 'TUPX') PNBRN(I)= SUPFLX(chem_id(i))
      IF (PLNAME(I) .EQ. 'TDKF') PNBRN(I)= SDKFLX(chem_id(i))
      IF (PLNAME(I) .EQ. 'COFX') PNBRN(I)= DCOFLX(chem_id(i))
      
      ! Add VFLX option to plot PVFLUX
      IF (PLNAME(I) .EQ. 'VFLX') PNBRN(I)= -PVFLUX(chem_id(i),1)
      IF (PLNAME(I) .EQ. 'FPVL') PNBRN(I)= Foliar_volatile_loss(chem_id(i))
      
      !Change units of output using user supplied constant
      RMULT= CONST(I)
      
      !IF(PLNAME(I).EQ.'INCS')THEN
      !   RMULT= 1.0
      !ENDIF
      PNBRN(I)= PNBRN(I)* RMULT
          
      
      IF((MODE(I).EQ.TSER).OR.(MODE(I).EQ.TSUM))THEN
          OUTPUT(I)= PNBRN(I)
      ELSEIF (MODE(I).EQ.TCUM) THEN          !            Accumulate output variable if a cumulative plot is requested
          OUTPUJ(I)= OUTPUT(I) + PNBRN(I)
          OUTPJJ(I)= OUTPJJ(I) + OUTPUJ(I)
      ELSEIF(MODE(I).EQ.TAVE)THEN 
          DO   K=start_compartment,end_compartment           !Accumulate Total Depth for use in depth weighted average
             DPTOT=DPTOT+DELX(K)
          end do
            OUTPUT(I)=PNBRN(I)/DPTOT
      ENDIF
      
       
        IF ((MODE(I).EQ.TSER).OR.(MODE(I).EQ.TSUM).OR.(MODE(I).EQ.TAVE))PRTBUF(i)= OUTPUT(I)
        IF (MODE(I).EQ.TCUM) PRTBUF(i)= OUTPJJ(I)

   end do
    
   call get_date(julday1900, current_year,current_month,current_day)
  
   
   WRITE(TimeSeriesUnit,18) current_year,current_month,current_day,(PRTBUF(I),I=1,NPLOTS)  
         !E3 format cuz program was leaving out the "E" on number <  E-99
18       FORMAT (I4,I3,I3, 4X,100(2X,ES12.4E3))  


   
  END SUBROUTINE write_outputfile
  
      
end Module Output_File
      
    
    
    
    
  !NOT Sure what this output was, it was undocumented  
                     
   ! IF (PLNAME(I) .EQ. DLYS)THEN                    
                     !SPTOT=0.0
                     !NUMCOM=(IARG3-IARG1)+1
                     !DELTOT=DELX(IARG1)*NUMCOM
                     !IF(MOD(NUMCOM,2).EQ.0)THEN
                     !  SPSTRT=((DELTOT/2.0)-(DELX(IARG1)/2.0))/2.5
                     !  IF(SPSTRT.LT.1.0)SPSTRT=1.0
                     !  MIDCOM1=(IARG1+IARG3)/2
                     !  MIDCOM2=MIDCOM1+1
                     !ELSE
                     !  SPSTRT=((DELTOT/2.0)/2.5)
                     !  IF(SPSTRT.LT.1.0)SPSTRT=1.0
                     !  MIDCOM1=(IARG1+IARG3)/2
                     !  MIDCOM2=MIDCOM1
                     !ENDIF
                     !DO K=IARG1,IARG3
                     !  SPWGHT=(2.0/((SPSTRT**2)+1.0))
                     !  TTTOT= TTTOT+((conc_porewater(ID1,K)*1.E6)*SPWGHT)
                     !  SPTOT=SPTOT+SPWGHT
                     !  IF(((K.LT.MIDCOM1).OR.(K.LT.MIDCOM2)).AND.((DELX(K)/2.5).GE.2.0))THEN
                     !    SPSTRT=(DELX(K)/2.5)/2.0
                     !  ELSEIF(((K.LT.MIDCOM1).OR.(K.LT.MIDCOM2)).AND.((DELX(K)/2.5).LT.2.0))THEN
                     !    SPSTRT=1.0
                     !  ELSEIF(((K.EQ.MIDCOM1).OR.(K.EQ.MIDCOM2)).AND.((DELX(K)/2.5).GE.2.0))THEN
                     !    SPSTRT=(DELX(K)/2.5)/2.0
                     !  ELSEIF(((K.EQ.MIDCOM1).OR.(K.EQ.MIDCOM2)).AND.((DELX(K)/2.5).LT.2.0))THEN
                     !    SPSTRT=1.0
                     !  ELSEIF(((K.GT.MIDCOM1).OR.(K.GT.MIDCOM2)).AND.((DELX(K)/2.5).GE.2.0))THEN
                     !    SPSTRT=SPSTRT+(DELX(K)/2.5)
                     !  ELSEIF(((K.GT.MIDCOM1).OR.(K.GT.MIDCOM2)).AND.((DELX(K)/2.5).LT.2.0))THEN
                     !    SPSTRT=1.0
                     !  ENDIF
                     !end do                 
                     !PNBRN(I)=TTTOT     
                 !ELSEIF (PLNAME(I) .EQ. CMSS)THEN    !cmss is not documented      
                 !    CMDPTH=0.0
                 !    DO K=IARG1,IARG3
                 !      TTTOT=TTTOT+(conc_total_per_water(ID1,K)*DELX(K)*theta_end(K))*DELX(K)
                 !    end do
                 !    CMTOT=0.0
                 !    CMSS1=TTTOT/2.
                 !    TTTOT=0.0
                 !    DO K=IARG1,IARG3
                 !      CMTOT=CMTOT+DELX(K)
                 !      TTTOT=TTTOT+(conc_total_per_water(ID1,K)*DELX(K)*theta_end(K))*DELX(K)
                 !      IF(TTTOT.EQ.0.0)THEN
                 !        CMDPTH=0.0
                 !      ELSEIF(TTTOT.LE.CMSS1)THEN
                 !        CMDPTH=CMTOT
                 !      ENDIF
                 !    end do
                 !    PNBRN(I)=CMDPTH
                 !ELSEIF (PLNAME(I) .EQ. INCS)THEN   !INCS is not documented       
                 !    TTTOT=0.0
                 !    CMTOT=0.0
                 !    CMDPTH=0.0                 
                 !    do  K=IARG1,IARG3
                 !      CMTOT=CMTOT+DELX(K)
                 !      TTTOT=(conc_total_per_water(ID1,K)*DELX(K)*theta_end(K))*DELX(K)
                 !      IF(TTTOT.GT.CONST(I))CMDPTH=CMTOT              !the use of CONST doesnt make sence to me here
                 !    end do                  
                 !    PNBRN(I)=CMDPTH
                 !ENDIF  