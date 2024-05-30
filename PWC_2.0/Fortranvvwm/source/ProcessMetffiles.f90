module ProcessMetfiles
    implicit none
    contains
    
    
    logical function new_weatherfile()
           use variables, ONLY: metfilename
           use utilities
           integer :: start
           character(len=3) :: extension
           
           new_weatherfile = .FALSE.
           start = scan(metfilename,'.', BACK = .TRUE.)          
           extension = metfilename(start+1:start+3)
           call To_uppercase(extension )          
           if (extension == "WEA") new_weatherfile = .TRUE.
           
          return 
    end function new_weatherfile
    !******************************************************************************************************
   subroutine read_new_weatherfile(eof)
    !This subroutine reads the metfile and returns the 
    !average previous 30 day temperature and daily wind speed
    use utilities
     use variables, ONLY: metfilename
     use nonInputVariables, Only: num_records, &
                                  wind,        &  !array of output wind speed (m/s)
                                  temp_avg,    &  !output: average of previous 30 day temperature
                                  evap,        &  !output: daily evaporation (m)
                                  precip,      &  !output: daily precipitation (m)
                                  num_years,   &  !output: number of years in simulation
                                  firstyear,   &
                                  startday1900,firstday, firstmon
    implicit none

    real :: temp30(30)        !array to hold previous 30 days of temperature 
    real :: tempsum            !sum of 30 day temperature values
    real :: temp                !temp as read from met file (C)
    integer :: i,j                !do loop counters
    integer :: lastyear, yr,  mon,day
    integer :: ierror

    integer,intent(out) ::  eof                    !end of file flag

    eof=0 
    ierror=0

    !*******check if met file exists*************************
    open (UNIT=19, FILE=metfilename,STATUS='old',ACTION='read',IOSTAT=ierror)
    if (ierror /=0) then 
        write(11,*) 'no weather file'
        return
    end if

    !***********************************************************************

    do i=1,num_records
        !********* read temp and wind speeds ***********************
        read (19,*, IOSTAT=eof) mon,day,yr,precip(i), evap(i),temp,wind(i)  !wind is used directly, temp is processed later
        
        if (eof /=0) then            !met file read error check
            write (11,*) 'weather file has problem; perhaps incomplete lines of data?.'
            return
        end if

        if (i==1) then   !get the first and last years dates
            firstyear = yr
            firstday = day
            firstmon = mon
        elseif (i==num_records) then
            lastyear = yr
        end if

        !********  read temperature *******************
        if (i==1) then !initially fill array with temperature of first array
            temp30=temp    
            tempsum=30.*temp
        end if

        tempsum = tempsum +temp-temp30(1)    !calculate new average termperature
        temp_avg(i)=tempsum/30.                !calculate new average termperature

        do j=1, 29                    !update new array
            temp30(j)= temp30(j+1)
        end do
        temp30(30) = temp
        !***********************************************
    end do

    
    startday1900 = jd(firstyear, firstmon,firstday)  !return the days since 1900 for first day of simulation
    
    wind=wind/100.        !whole array operation convert to m/s
    evap = evap/100.    !whole array operation convert to m
    precip = precip/100. !whole array operation convert to m

    num_years = lastyear-firstyear +1
    
    
    
       !Check to see if there is actually a last year
    !added 2/5/2020.  Since startday is not necessarily Jan 1, the count could be off by 1
    !might need another condition for partial years
    
    if (firstday /= 1 .or. firstmon /= 1) then        
        num_years = num_years-1
    end if
    

    close (UNIT=19)
    end subroutine read_new_weatherfile
    !******************************************************************************************************
    subroutine read_metfile(eof)
    !This subroutine reads the OLD STYLE metfile with 2 digit years  and returns the 
    !average previous 30 day temperature and daily wind speed
     use utilities
     use variables, ONLY: metfilename
     use nonInputVariables, Only: num_records, &
                                  wind,        &  !array of output wind speed (m/s)
                                  temp_avg,    &  !output: average of previous 30 day temperature
                                  evap,        &  !output: daily evaporation (m)
                                  precip,      &  !output: daily precipitation (m)
                                  num_years,   &  !output: number of years in simulation
                                  firstyear,   &
                                  startday1900,firstday, firstmon
    implicit none

    real :: temp30(30)        !array to hold previous 30 days of temperature 
    real :: tempsum            !sum of 30 day temperature values
    real :: temp                !temp as read from met file (C)
    integer :: i,j                !do loop counters
    integer :: lastyear, yr,  mon,day
    integer :: ierror

    integer,intent(out) ::  eof                    !end of file flag

    eof=0 
    ierror=0

    !*******check if met file exists*************************
    open (UNIT=19, FILE=metfilename,STATUS='old',ACTION='read',IOSTAT=ierror)
    if (ierror /=0) then 
        write(11,*) 'no met file'
        return
    end if
    !***********************************************************************

    do i=1,num_records
        !********* read temp and wind speeds ***********************
        read (19,100, IOSTAT=eof) mon,day,yr,precip(i), evap(i),temp,wind(i)  !wind is used directly, temp is processed later

        100 format(1X,3I2,5F10.0)  !see PRZM manual for definitions in met file p.4-2

        if (eof /=0) then            !met file read error check
            write (11,*) 'met file out of whack'
            return
        end if

        if (i==1) then   !get the first and last years dates
            firstyear = yr+1900
            firstday = day
            firstmon = mon
        elseif (i==num_records) then
            lastyear = yr + 1900
        end if

        !********  read temperature *******************
        if (i==1) then !initially fill array with temperature of first array
            temp30=temp    
            tempsum=30.*temp
        end if

        tempsum = tempsum +temp-temp30(1)    !calculate new average termperature
        temp_avg(i)=tempsum/30.                !calculate new average termperature

        do j=1, 29                    !update new array
            temp30(j)= temp30(j+1)
        end do
        temp30(30) = temp
        !***********************************************
    end do

    
    startday1900 = jd(firstyear, firstmon,firstday)  !return the days since 1900 for first day of simulation
    
    wind=wind/100.        !whole array operation convert to m/s
    evap = evap/100.    !whole array operation convert to m
    precip = precip/100. !whole array operation convert to m

    num_years = lastyear-firstyear +1
    

    


    close (UNIT=19)
    end subroutine read_metfile




!##############################################################################
subroutine count_met(ierror)
    !read met file and count records for array allocation, returns n
    use nonInputVariables, only: num_records
    use variables, only: metfilename
    implicit none
    integer, intent(out):: ierror                !open file error 0=success

    integer :: ieof                !signal for no more records '/=0' = no record 
    integer dummy

    open (UNIT=17, FILE=metfilename,STATUS='old',ACTION='read',IOSTAT=ierror)

    if (ierror ==0) then   !count records if file exists
        num_records=0
        do        
            read (17,*, IOSTAT=IEOF) dummy
            if (ieof /= 0)exit
            num_records=num_records+1
        end do
    end if
    
    
    close(UNIT=17)

end subroutine count_met



!###################################################################################
subroutine get_latitude (latitude)
!this routine takes the wban number from the metfile name and returns the associated latitude.
use variables, ONLY:metfilename

implicit none

real,intent(out) ::     latitude

character(len=5) :: wban_number_text
integer :: dot_position,wban_number_integer
integer :: ierror              

dot_position = index(metfilename,".")


wban_number_text = metfilename(dot_position-5:dot_position-1)

 
read (wban_number_text,*, IOSTAT= ierror) wban_number_integer
    if (ierror /=0) then 
    latitude =  40.00
        return
    end if
    


select case(wban_number_integer)
    case(3103)
        latitude =    35.13
    case(3812)
        latitude =    35.43
    case(3813)
        latitude =    32.70
    case(3820)
        latitude =    33.37
    case(3822)
        latitude =    32.13
    case(3856)
        latitude =    34.65
    case(3860)
        latitude =    38.37
    case(3870)
        latitude =    34.90
    case(3927)
        latitude =    32.90
    case(3928)
        latitude =    37.65
    case(3937)
        latitude =    30.12
    case(3940)
        latitude =    32.32
    case(3945)
        latitude =    38.82
    case(3947)
        latitude =    39.32
    case(4725)
        latitude =    42.22
    case(4751)
        latitude =    41.80
    case(11641)
        latitude =    18.43
    case(12832)
        latitude =    29.73
    case(12834)
        latitude =    29.18
    case(12836)
        latitude =    24.55
    case(12839)
        latitude =    25.80
    case(12842)
        latitude =    27.97
    case(12844)
        latitude =    26.68
    case(12912)
        latitude =    28.85
    case(12916)
        latitude =    29.98
    case(12917)
        latitude =    29.95
    case(12919)
        latitude =    25.90
    case(12921)
        latitude =    29.53
    case(12924)
        latitude =    27.77
    case(12960)
        latitude =    29.97
    case(13722)
        latitude =    35.87
    case(13723)
        latitude =    36.08
    case(13729)
        latitude =    38.88
    case(13733)
        latitude =    37.33
    case(13737)
        latitude =    36.90
    case(13739)
        latitude =    39.88
    case(13740)
        latitude =    37.50
    case(13741)
        latitude =    37.32
    case(13748)
        latitude =    34.27
    case(13781)
        latitude =    39.67
    case(13865)
        latitude =    32.33
    case(13866)
        latitude =    38.37
    case(13873)
        latitude =    33.95
    case(13874)
        latitude =    33.65
    case(13876)
        latitude =    33.57
    case(13877)
        latitude =    36.48
    case(13880)
        latitude =    32.90
    case(13881)
        latitude =    35.22
    case(13882)
        latitude =    35.03
    case(13883)
        latitude =    33.95
    case(13889)
        latitude =    30.50
    case(13891)
        latitude =    35.82
    case(13893)
        latitude =    35.05
    case(13894)
        latitude =    30.68
    case(13895)
        latitude =    32.30
    case(13897)
        latitude =    36.12
    case(13957)
        latitude =    32.47
    case(13958)
        latitude =    30.28
    case(13959)
        latitude =    31.62
    case(13962)
        latitude =    32.42
    case(13963)
        latitude =    34.73
    case(13964)
        latitude =    35.33
    case(13966)
        latitude =    33.97
    case(13967)
        latitude =    35.40
    case(13968)
        latitude =    36.20
    case(13970)
        latitude =    30.53
    case(13985)
        latitude =    37.77
    case(13994)
        latitude =    38.75
    case(13995)
        latitude =    37.23
    case(13996)
        latitude =    39.07
    case(14607)
        latitude =    46.87
    case(14732)
        latitude =    40.77
    case(14733)
        latitude =    42.93
    case(14734)
        latitude =    40.70
    case(14735)
        latitude =    42.75
    case(14737)
        latitude =    40.65
    case(14739)
        latitude =    42.37
    case(14740)
        latitude =    41.93
    case(14742)
        latitude =    44.47
    case(14745)
        latitude =    43.20
    case(14751)
        latitude =    40.22
    case(14764)
        latitude =    43.65
    case(14765)
        latitude =    41.73
    case(14768)
        latitude =    43.13
    case(14771)
        latitude =    43.12
    case(14777)
        latitude =    41.33
    case(14778)
        latitude =    41.25
    case(14820)
        latitude =    41.42
    case(14821)
        latitude =    40.00
    case(14826)
        latitude =    42.97
    case(14827)
        latitude =    41.00
    case(14836)
        latitude =    42.77
    case(14837)
        latitude =    43.13
    case(14839)
        latitude =    42.95
    case(14840)
        latitude =    43.17
    case(14842)
        latitude =    40.67
    case(14847)
        latitude =    46.47
    case(14848)
        latitude =    41.70
    case(14850)
        latitude =    44.73
    case(14852)
        latitude =    41.25
    case(14860)
        latitude =    42.08
    case(14891)
        latitude =    40.82
    case(14895)
        latitude =    40.92
    case(14898)
        latitude =    44.48
    case(14913)
        latitude =    46.83
    case(14914)
        latitude =    46.90
    case(14918)
        latitude =    48.57
    case(14920)
        latitude =    43.87
    case(14922)
        latitude =    44.88
    case(14923)
        latitude =    41.45
    case(14925)
        latitude =    43.92
    case(14926)
        latitude =    45.55
    case(14933)
        latitude =    41.53
    case(14935)
        latitude =    40.97
    case(14936)
        latitude =    44.38
    case(14940)
        latitude =    43.15
    case(14941)
        latitude =    41.98
    case(14943)
        latitude =    42.40
    case(14944)
        latitude =    43.57
    case(14991)
        latitude =    44.87
    case(21504)
        latitude =    19.72
    case(22516)
        latitude =    20.90
    case(22521)
        latitude =    21.33
    case(22536)
        latitude =    21.98
    case(23023)
        latitude =    31.95
    case(23034)
        latitude =    31.37
    case(23042)
        latitude =    33.65
    case(23044)
        latitude =    31.80
    case(23047)
        latitude =    35.23
    case(23048)
        latitude =    35.18
    case(23050)
        latitude =    35.05
    case(23061)
        latitude =    37.45
    case(23062)
        latitude =    39.77
    case(23063)
        latitude =    39.65
    case(23065)
        latitude =    39.37
    case(23066)
        latitude =    39.12
    case(23129)
        latitude =    33.82
    case(23153)
        latitude =    38.07
    case(23154)
        latitude =    39.28
    case(23155)
        latitude =    35.42
    case(23160)
        latitude =    32.13
    case(23161)
        latitude =    34.87
    case(23169)
        latitude =    36.08
    case(23174)
        latitude =    33.93
    case(23183)
        latitude =    33.43
    case(23184)
        latitude =    34.65
    case(23185)
        latitude =    39.50
    case(23188)
        latitude =    32.73
    case(23232)
        latitude =    38.52
    case(23234)
        latitude =    37.62
    case(23273)
        latitude =    34.90
    case(24011)
        latitude =    46.77
    case(24013)
        latitude =    48.27
    case(24018)
        latitude =    41.15
    case(24021)
        latitude =    42.82
    case(24023)
        latitude =    41.13
    case(24025)
        latitude =    44.38
    case(24028)
        latitude =    41.87
    case(24033)
        latitude =    45.77
    case(24036)
        latitude =    47.05
    case(24037)
        latitude =    46.43
    case(24089)
        latitude =    42.92
    case(24090)
        latitude =    44.05
    case(24121)
        latitude =    40.83
    case(24127)
        latitude =    40.78
    case(24128)
        latitude =    40.90
    case(24131)
        latitude =    43.57
    case(24137)
        latitude =    48.60
    case(24143)
        latitude =    47.48
    case(24144)
        latitude =    46.60
    case(24146)
        latitude =    48.30
    case(24153)
        latitude =    46.93
    case(24155)
        latitude =    45.68
    case(24156)
        latitude =    42.92
    case(24157)
        latitude =    47.63
    case(24221)
        latitude =    44.12
    case(24225)
        latitude =    42.38
    case(24227)
        latitude =    46.97
    case(24229)
        latitude =    45.60
    case(24230)
        latitude =    44.27
    case(24232)
        latitude =    44.92
    case(24233)
        latitude =    47.45
    case(24243)
        latitude =    46.57
    case(24283)
        latitude =    40.98
    case(24284)
        latitude =    43.42
    case(25308)
        latitude =    55.03
    case(25339)
        latitude =    59.52
    case(25501)
        latitude =    57.75
    case(25503)
        latitude =    58.68
    case(25624)
        latitude =    55.20
    case(25713)
        latitude =    57.15
    case(26411)
        latitude =    64.82
    case(26415)
        latitude =    64.00
    case(26425)
        latitude =    62.15
    case(26451)
        latitude =    61.17
    case(26510)
        latitude =    62.97
    case(26528)
        latitude =    62.30
    case(26533)
        latitude =    66.92
    case(26615)
        latitude =    60.78
    case(26616)
        latitude =    66.87
    case(26617)
        latitude =    64.50
    case(27502)
        latitude =    71.30
    case(41415)
        latitude =    13.55
    case(93037)
        latitude =    38.82
    case(93058)
        latitude =    38.28
    case(93129)
        latitude =    37.70
    case(93193)
        latitude =    36.78
    case(93721)
        latitude =    39.18
    case(93729)
        latitude =    35.27
    case(93730)
        latitude =    39.45
    case(93738)
        latitude =    38.95
    case(93805)
        latitude =    30.38
    case(93814)
        latitude =    39.05
    case(93815)
        latitude =    39.90
    case(93817)
        latitude =    38.05
    case(93819)
        latitude =    39.73
    case(93820)
        latitude =    38.03
    case(93821)
        latitude =    38.18
    case(93822)
        latitude =    39.85
    case(93842)
        latitude =    32.52
    case(93987)
        latitude =    31.23
    case(94008)
        latitude =    48.22
    case(94018)
        latitude =    40.02
    case(94185)
        latitude =    43.58
    case(94224)
        latitude =    46.15
    case(94240)
        latitude =    47.95
    case(94702)
        latitude =    41.17
    case(94725)
        latitude =    44.93
    case(94728)
        latitude =    40.78
    case(94746)
        latitude =    42.27
    case(94814)
        latitude =    44.37
    case(94822)
        latitude =    42.20
    case(94823)
        latitude =    40.50
    case(94830)
        latitude =    41.60
    case(94846)
        latitude =    42.00
    case(94847)
        latitude =    42.23
    case(94849)
        latitude =    45.07
    case(94860)
        latitude =    42.88
    case(94910)
        latitude =    42.55
    case(94918)
        latitude =    41.37
    case default
        latitude =  40.00
end select
end subroutine get_latitude



end module ProcessMetfiles