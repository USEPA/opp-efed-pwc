module plantgrowth
    implicit none
    contains
    
    Subroutine Crop_Growth
    use utilities
    
    use constants_and_Variables, ONLY: num_records, crop_fraction_of_max, startday,num_crop_periods, &  
                                       emergence_date,maturity_date, harvest_date, &
                                        canopy_cover,canopy_height,canopy_holdup ,NCOM2,soil_depth, &
                                       is_harvest_day, evapo_root_node,root_depth, &
                                       max_root_depth,max_canopy_cover,max_canopy_holdup,max_canopy_height, &
                                       min_evap_node,root_node, atharvest_pest_app,foliar_disposition
            
      implicit none
      
  !    integer :: YEAR,MONTH,DAY
           
      integer i ,j
      
      integer :: start, last
      integer :: emerg_ref0(num_crop_periods)
      integer :: matur_ref0(num_crop_periods)
      integer :: harv_ref0(num_crop_periods)  
       
      allocate (crop_fraction_of_max (num_records)) 
      allocate (canopy_cover         (num_records)) 
      allocate (canopy_height        (num_records))
      allocate (canopy_holdup        (num_records))
      allocate (is_harvest_day       (num_records))
      allocate (evapo_root_node      (num_records))
      allocate (root_depth           (num_records))
      allocate (root_node            (num_records))
      allocate (atharvest_pest_app   (num_records)) 
      

      !Default Values (initialization)
      crop_fraction_of_max = 0.0  
      canopy_cover = 0.0
      canopy_height = 0.0
      canopy_holdup = 0.0
      root_depth = 0.0
      root_node  = 0
      is_harvest_day = .FALSE.
      evapo_root_node = min_evap_node
      atharvest_pest_app = 0
      
 
      emerg_ref0 = emergence_date -startday +1
      matur_ref0  = maturity_date  -startday +1
      harv_ref0   = harvest_date   -startday +1    

      !Do growth stage
        do i=1, num_crop_periods
            
          start = max(emerg_ref0(i), 1)
          if (start > num_records) cycle
          
          last  = min(matur_ref0(i),num_records)  
          if (last < 1) cycle
          
          do j=start, last       
              crop_fraction_of_max(j) =  real((j - emerg_ref0(i)))/real  ( (matur_ref0(i)-emerg_ref0(i)))
              canopy_cover(j)  = crop_fraction_of_max(j)*max_canopy_cover(i)
              canopy_height(j) = crop_fraction_of_max(j)*max_canopy_height(i)
              
              canopy_holdup(j) = canopy_cover(j)*max_canopy_holdup(i)
              
              root_depth(j)   = crop_fraction_of_max(j)*max_root_depth(i)
              
              root_node(j) =  find_depth_node(ncom2,soil_depth,root_depth(j))                         
          end do     
        end do


        !Do maturity stage
        do i=1, num_crop_periods    
            start = max(matur_ref0(i), 1)
            if (start > num_records) cycle
            last  = min(harv_ref0(i)-1,   num_records)  !day before harvest is last day at max canopy               
            
            
            !change from do concurrent (find depth is not elemental)
            do j = start, last
              crop_fraction_of_max(j) =  1.0
              canopy_cover(j)  = max_canopy_cover(i)
              canopy_height(j) = max_canopy_height(i)
              canopy_holdup(j) = max_canopy_holdup(i)*canopy_cover(j)
              root_depth(j)   = max_root_depth(i)
              root_node(j)     = find_depth_node(ncom2, soil_depth ,max_root_depth(i))
            end do
            
        end do

       !minimum evaporation zone is limited by min_evap_depth (anetd) parameter
        where (root_node > min_evap_node) evapo_root_node = root_node
   
     
        
        
        !find harvest days
        do i=1, num_crop_periods 
             if (harv_ref0(i)>0 .AND. harv_ref0(i)< num_records) then
                 is_harvest_day(harv_ref0(i)) = .TRUE. 
                 atharvest_pest_app(harv_ref0(i)) = foliar_disposition(i)
             end if           
        end do
  
        
    end subroutine Crop_Growth
    
 
end module plantgrowth
   