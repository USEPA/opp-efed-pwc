module Calibration
implicit none
    contains
    

subroutine read_calibration_data
  use  constants_and_variables, ONLY: data_date,data_erosion,data_number,data_runoff,data_mgsed,data_mgh2o, &
    CalibrationDataUnit
    use utilities
    implicit none
    
    real    :: dummy, read_runoff, read_erosion, mg_sed, mg_h2o
    integer :: day,month,year, n, i,j
    
    
    
    data_date = 0
    data_erosion = 0.0
    
  !  OPEN(Unit=88,FILE='P1data.txt',STATUS='OLD')
    read (CalibrationDataUnit,*) n
    read (CalibrationDataUnit,*)
    j=0
    do i = 1, n
       read (CalibrationDataUnit,*) month,day,year, dummy, read_runoff, read_erosion, mg_sed, mg_h2o         
       if (read_runoff > 0.0) then
           j=j+1
        data_date(j) = jd (YEAR,MONTH,DAY)
        data_erosion(j) = read_erosion  !kg/ha
        data_runoff(j) = read_runoff    !cm
        data_mgsed(j) = mg_sed
        data_mgh2o(j) = mg_h2o
       end if
    end do
    
    data_number = j

end subroutine read_calibration_data
    



    
!*********************************************************************************    
subroutine write_calibration_data
use  constants_and_variables, ONLY: data_date,data_erosion, model_erosion,data_number,data_runoff,model_runoff,  &
                         model_mg_ro, model_mg_er ,data_mgsed,data_mgh2o ,AFIELD,  &
                         application_rate,USLEC,Calibrated_curve_Number, cn_moisture, CalibrationFilenameOutput
use utilities
implicit none

integer :: i,YEAR,MONTH,DAY
real :: mo_mg_ro, mo_mg_er, diff_mg_ro,   diff_mg_er, r2_mg_ro,   r2_mg_er 

real :: total_data_ro, total_model_ro
real :: total_data_er, total_model_er
real :: total_applied

real :: total_applied_normalized_ro_ssq, unweighted_ssq, data_normalized_ssq


 real :: total_model_ro_withdata
 real :: total_model_er_withdata

 real :: printUSLE
 
 integer :: data_count,number_data_greater_zero
 
  !Calculate total applied
 total_applied = sum (application_rate)*AFIELD*1000000.
 
 
 
 
 OPEN(Unit=66,FILE= CalibrationFilenameOutput, STATUS='UNKNOWN')
 !OPEN(Unit=66,FILE= CalibrationFilename(1:len_trim(CalibrationFilename)-4)//  '_CalibrateOut.txt' , STATUS='UNKNOWN')

 
 

 write (66,*) "                    Sum Abs. Diff.   Greatest Value   Location#" 
 write (66,"(A20,G12.3,5x,G12.3,5x, I6)") "Runoff  Calibration ", & 
                                         sum(abs(data_runoff(1:data_number)- model_runoff(1:data_number))), &
                                         maxval(abs(data_runoff(1:data_number)- model_runoff(1:data_number))), &
                                         maxloc(abs(data_runoff(1:data_number)- model_runoff(1:data_number)))
    
 write (66,"(A20,G12.3,5x,G12.3,5x, I6)") "Erosion Calibration ", &
                                         sum(abs(data_erosion(1:data_number)- model_erosion(1:data_number))), &
                                         maxval(abs(data_erosion(1:data_number)- model_erosion(1:data_number))),&
                                         maxloc(abs(data_erosion(1:data_number)- model_erosion(1:data_number)))
    
 write(66,'(a180)') &
 '                |              EROSION                  |               RUNOFF                 |           Runoff Mass (mg)                  |      EROSION Mass (mg)                  '
 write(66,'(a180)') &
 '  # Date         Data             Model        Diff         Data           Model          Diff           Data         Model          Diff          Data          Model         Diff     '
 
  
 r2_mg_ro = 0.
 r2_mg_er = 0.
 total_data_ro = 0.
 total_data_er = 0.    
 total_model_ro = 0.
 total_model_er = 0.
 
 total_model_ro_withdata=0.0
 total_model_er_withdata=0.0
 
 total_applied_normalized_ro_ssq = 0.0
 data_normalized_ssq = 0.0 
 unweighted_ssq = 0.0
 
 data_count = 0
 number_data_greater_zero=0

 do i = 1, data_number        
         mo_mg_ro = model_mg_ro(i) * 1.0e11 * AFIELD 
         mo_mg_er = model_mg_er(i) * 1.0e11 * AFIELD       
 
         total_model_ro = total_model_ro + mo_mg_ro
         total_model_er = total_model_er + mo_mg_er

         !-99 means that no data was gathered 
         if (data_mgh2o(i) >-99) then
             diff_mg_ro = (data_mgh2o(i) - mo_mg_ro)                          
             
             !changed to data-normalized squares 6/15/16
             if (data_mgh2o(i) > 0.0) then               
                data_normalized_ssq = data_normalized_ssq + (diff_mg_ro/data_mgh2o(i))**2
                number_data_greater_zero = number_data_greater_zero + 1
             end if
             
             !unweighted SSQ
             unweighted_ssq = unweighted_ssq + diff_mg_ro**2
             
             !added 9/19/2016 Input mass normalization ssq
             total_applied_normalized_ro_ssq = total_applied_normalized_ro_ssq + (diff_mg_ro/ total_applied)**2
             data_count = data_count +1
                   
             total_data_ro = total_data_ro + data_mgh2o(i)           
             total_model_ro_withdata= total_model_ro_withdata+ mo_mg_ro
         else
             diff_mg_ro =-9999
         end if        
       
         if (data_mgsed(i) >-99) then
             diff_mg_er = data_mgsed(i) - mo_mg_er
             r2_mg_er = r2_mg_er + diff_mg_er*diff_mg_er
             total_data_er = total_data_er + data_mgsed(i)            
             
             total_model_er_withdata = total_model_er_withdata+mo_mg_er
         else
             diff_mg_er = -9999
         end if
        
        call get_date (data_date(i), YEAR,MONTH,DAY)
        write(66,'(i3, 2I3,I5, 4(G15.7,G15.7, G11.3, 2X))')i, Month,day,YEAR, &
                 data_erosion(i), model_erosion(i), data_erosion(i)- model_erosion(i),&
                 data_runoff(i), model_runoff(i),  data_runoff(i)- model_runoff(i),  &
                 data_mgh2o(i), mo_mg_ro, diff_mg_ro, & 
                 data_mgsed(i), mo_mg_er, diff_mg_er
 end do 
 
 write (66,*)               "********************************************************"   
 write (66,'(a39, G12.3)')  "Data-normalized SSQ runoff pesticide = ", data_normalized_ssq
 write (66,'(a39, I3)')     "Number data points for above (>0)    = ", number_data_greater_zero
 write (66,'(a39, G12.3)')  "App-normalized SSQ runoff pesticide  = ", total_applied_normalized_ro_ssq
 write (66,'(a39, G12.3)')  "Unweighted SSQ runoff pesticide      = ", unweighted_ssq
 write (66,'(a39, I3)')     "Number data points for above (w/0)   = ", data_count
 write (66,*)               "Total  Applied Pesticide (mg) = ", total_applied
 write (66,*)               "********************************************************"  
 write (66,'(a23, G12.3)')  "SSQ erosin pesticide = ", r2_mg_er
 write (66,*)"X"
 write (66,*)"X" 
 write (66,*)"X"
 write (66,*)"X"
 write (66,*) "********************************************************"  

 write (66,*) "Totals Runoff:"
 write (66,'(a23, G12.3)') "Pesticide Data (mg)  = ",  total_data_ro 
 write (66,'(a23, G12.3)') "Pesticide Model (mg) = ",  total_model_ro   
 write (66,'(a31, G14.6)') "Model:Data Pesticide Runoff = ",  total_model_ro/ total_data_ro  
 write (66,'(a31, G14.6)') "Data:Model Pesticide Runoff = ",total_data_ro/ total_model_ro   
 
 write (66,'(a46, G14.6)') "Modeled ro mass where data occured (mg)     = ",  total_model_ro_withdata
 write (66,'(a46, G14.6)') "Model:Data Pesticide Runoff WhereDataOccured= ",  total_model_ro_withdata/ total_data_ro  
 write (66,'(a31, G14.6)') "Data:Model Pesticide Runoff = ",total_data_ro/ total_model_ro_withdata 
  
  
  
 write (66,*)                                             
 write (66,*) "Totals Erosion: "                           
 write (66,'(a23, G12.3)') "Pesticide Data (mg)  = ", total_data_er 
 write (66,'(a23, G12.3)') "Pesticide Model (mg) = ", total_model_er
 write (66,'(a31, G14.6)') "Model:Data Pesticide Erosion = ",  total_model_er/ total_data_er
 write (66,'(a31, G14.6)') "Data:Model Pesticide Erosion = ",  total_data_er/total_model_er
 
 write (66,'(a46, G14.6)') "Modeled er mass where data occured (mg)     = ",total_model_er_withdata
 write (66,'(a46, G14.6)') "Model:Data Pesticide erosion WhereDataOccured= ",   total_model_er_withdata/ total_data_er
 write (66,'(a31, G14.6)') "Data:Model Pesticide erosion = ",                 total_data_er/ total_model_er_withdata
 
  

 write (66,*)                                             
 write (66,*) "Totals total: "                           
 write (66,'(a23, G12.3)') "Pesticide Data (mg)  = ", (total_data_er + total_data_ro)
 write (66,'(a23, G12.3)') "Pesticide Model (mg) = ", total_model_er + total_model_ro
 write (66,'(a31, G14.6)') "Model:Data Total = ",  (total_model_er + total_model_ro)/ (total_data_er + total_data_ro)
 write (66,'(a31, G14.6)') "Data:Model Total = ",  (total_data_er + total_data_ro)/(total_model_er + total_model_ro)
  write(66,*) 
  
  
  
  
 !Calculate total applied
 total_applied = sum (application_rate)*AFIELD*1000000.
 
 write(66,*) "Modeled Values: "
 write(66,'(A36, G12.4)') "Total Applied (mg)                = ", total_applied
 write(66,'(A36, G12.4)') "Fraction off field                = ", (total_model_ro + total_model_er)/total_applied
 write(66,'(A36, G12.4,G12.4)') "Fraction due to Runoff & Erosion  = ", total_model_ro /total_applied ,  total_model_er/total_applied
 write(66,'(A36, G12.4,G12.4)') "Runoff Fraction/Total offField    = ", total_model_ro /(total_model_ro + total_model_er) ,  total_model_er/(total_model_ro + total_model_er)
 
  write(66,*)
 write(66,*) "Data Values: "
 write(66,'(A36, G12.4)') "Fraction off field                = ", (total_data_ro + total_data_er)/total_applied
 write(66,'(A36, G12.4,G12.4)') "Fraction due to Runoff & Erosion  = ", total_data_ro /total_applied ,  total_data_er/total_applied
 write(66,'(A36, G12.4,G12.4)') "Runoff Fraction/Total offField  = ", total_data_ro /(total_data_ro + total_data_er) ,  total_data_er/(total_data_ro + total_data_er)
 
 
 
 
 write(66,*)

 write(66,*) "Optimized USLE-C Values given proper CN:"
  do i = 1, data_number
      if (model_erosion(i) <= 0.0) then
          printUSLE = 99999999.
      else
          printUSLE=USLEC(i)*data_erosion(i)/model_erosion(i)
      end if
      
      if (data_erosion(i) == 0.0) printUSLE = 0.0
          

      write(66,'(I4,E20.7)') i, printUSLE
  end do
  
  write(66,*)
  write(66,*) "Optimized CN;"
  
   do i = 1, data_number
            write(66,'(I4,F12.7, G20.7)')  i,  Calibrated_curve_Number(i), cn_moisture(i)
   enddo
 
 

end subroutine write_calibration_data
    
    
end module Calibration