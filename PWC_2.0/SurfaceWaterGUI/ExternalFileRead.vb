Partial Public Class Form1

    Public Sub StartExternalBatchRun(ByVal waterbodytype As String, ByRef ThereIsProblem As Boolean)
        'Number of horizons is the total horizon including the aquiferhorizon

        Dim LineCount As Integer
        Dim sw_Header As String
        Dim gw_header As String
        Dim BatchFileName As String
        Dim errorfilename As String
        Dim sw_summaryfilename As String
        Dim gw_summaryfilename As String
        Dim EXTRAsummaryFileName As String
        Dim EXTRAsummaryFileNameDeg1 As String
        Dim EXTRAsummaryFileNameDeg2 As String
        Dim runflag As Boolean
        Dim run_id As String
        Dim msg As String
        Dim nchem As Integer

        Dim GWpeakConc(3) As Single
        Dim GWsimAvgConc(3) As Single
        Dim GWthroughputs(3) As Single
        Dim GWpostBTavg(3) As Single
        Dim GWBreakThroughdays(3) As Single

        Dim finalmsg As String
        Dim finalmsgDeg1 As String
        Dim finalmsgDeg2 As String

        Dim filename As String

        Dim ErrorMessage As String

        ErrorMessage = ""



        nchem = 1 'external batch can only do parent for now

        BatchFileName = BatchChemFile.Text

        sw_Header = String.Format("Line Batch Run ID             {0}Peak{0}1-day{0}Yr{0}overall{0}4-day{0}21-day{0}60-day{0}90-day{0}PW_pk{0}PW_21{1}", vbTab, vbNewLine)
        gw_header = String.Format("{1,-25}{0}{2,10}{0}{3,10}{0}{4,10}{0}{5,10}{0}{6,10}{7}", vbTab, "GW Run ID", "Peak", "Breakthru", "Thruput", "PostBT Avg", "Sim Avg", vbNewLine)

        LineCount = 0

        If Not System.IO.File.Exists(BatchFileName) Then
            MsgBox("Batch file does not exist")
            ThereIsProblem = True
            Exit Sub
        End If




        'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXx
        If FormatScenarios.Checked Then
            JustReformatTheScenarios(BatchFileName)
            Exit Sub
        End If






        workingDirectoryLabel.Text = System.IO.Path.GetDirectoryName(BatchFileName) & "\"
        System.IO.Directory.SetCurrentDirectory(workingDirectoryLabel.Text)

        'Delete error file if it exists and recreate with Header
        errorfilename = workingDirectoryLabel.Text & "BatchErrorSummary.txt"
        If IO.File.Exists(errorfilename) Then IO.File.Delete(errorfilename)
        msg = "List of Errors in batch run. Line numbers correspond to line numbers in input file."
        My.Computer.FileSystem.WriteAllText(errorfilename, msg, False, System.Text.Encoding.ASCII)



        If isMinimumBatchOutput.Checked = False Then

            'Delete summary file if it exists and recreate with Header
            sw_summaryfilename = workingDirectoryLabel.Text & "Summary_SW.txt"
            If IO.File.Exists(sw_summaryfilename) Then IO.File.Delete(sw_summaryfilename)
            My.Computer.FileSystem.WriteAllText(sw_summaryfilename, sw_Header, False, System.Text.Encoding.ASCII)

            'do the same for the gw file
            gw_summaryfilename = workingDirectoryLabel.Text & "Summary_GW.txt"

            If IO.File.Exists(gw_summaryfilename) Then IO.File.Delete(gw_summaryfilename)
            My.Computer.FileSystem.WriteAllText(gw_summaryfilename, gw_header, False, System.Text.Encoding.ASCII)

        End If


        FileNameClass.BatchOutputVVWM = workingDirectoryLabel.Text & "BatchOutputVVWM.txt"
        If IO.File.Exists(FileNameClass.BatchOutputVVWM) Then IO.File.Delete(FileNameClass.BatchOutputVVWM)


        'this file gets nemed in the vvwm file naming


        '********************* ADDITIONAL RETURN FREQUENCY ************************************************
        If IsAddFrequencyReturn.Checked Then
            'Delete summary file if it exists and recreate with Header

            EXTRAsummaryFileName = workingDirectoryLabel.Text & "Summary_sw_" & Trim(ReturnFrequency.Text) & ".txt"

            If IO.File.Exists(EXTRAsummaryFileName) Then IO.File.Delete(EXTRAsummaryFileName)

            My.Computer.FileSystem.WriteAllText(EXTRAsummaryFileName, sw_Header, False, System.Text.Encoding.ASCII)

            If Deg1CheckBox.Checked Then
                EXTRAsummaryFileNameDeg1 = workingDirectoryLabel.Text & "Summary_sw_Deg1_" & Trim(ReturnFrequency.Text) & ".txt"
                If IO.File.Exists(EXTRAsummaryFileNameDeg1) Then IO.File.Delete(EXTRAsummaryFileNameDeg1)

                My.Computer.FileSystem.WriteAllText(EXTRAsummaryFileNameDeg1, sw_Header, False, System.Text.Encoding.ASCII)
            End If

            If Deg2CheckBox.Checked Then
                EXTRAsummaryFileNameDeg2 = workingDirectoryLabel.Text & "Summary_sw_Deg2_" & Trim(ReturnFrequency.Text) & ".txt"
                If IO.File.Exists(EXTRAsummaryFileNameDeg2) Then IO.File.Delete(EXTRAsummaryFileNameDeg2)

                My.Computer.FileSystem.WriteAllText(EXTRAsummaryFileNameDeg2, sw_Header, False, System.Text.Encoding.ASCII)
            End If

        End If
        '**********************************************************************************************************************



        Dim spanDays As Integer
        Dim interval As Integer

        If ApplyWindow.Checked Then
            spanDays = SpanTB.Text
            interval = IntervalTB.Text
            'windowRuns = spanDays \ interval + 1  'Integer Operator \
        Else
            spanDays = 0
            interval = 1
            '  windowRuns = 1
        End If


        'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        'Start reading the External Batch Run File input file
        Using MyReader As New Microsoft.VisualBasic.FileIO.TextFieldParser(BatchFileName)
            MyReader.TextFieldType = FileIO.FieldType.Delimited
            MyReader.SetDelimiters(",")

            MyReader.ReadLine()  'Header Lines
            Dim fail As String
            fail = ""

            LineCount = 1
            While Not MyReader.EndOfData
                msg = ""
                LineCount = LineCount + 1

                ' ReadBatchInputs(MyReader, msg)  'Returns Waterbody type and error message, Loads inputs to GUI

                ReadExternalFileInputs(MyReader, fail)




                msg = Checkvalues()  'check all values loaded into interface after each external file line read

                If msg <> "" Then
                    msg = vbNewLine & LineCount & " " & msg
                    My.Computer.FileSystem.WriteAllText(errorfilename, msg, True, System.Text.Encoding.ASCII)
                    Continue While
                End If



                For i As Integer = 0 To spanDays Step interval

                    run_id = String.Format("{0} {1}_{2}_{3}", LineCount, ioFamilyNameBox.Text, scenarioID.Text, i)
                    FileNameClass.ScenarioRunID = run_id

                    RunPrograms(waterbodytype, i, runflag, ErrorMessage)


                    If runflag Then  ' additional check value routine for irtems that are difficult to check prior to przm file creation
                        ThereIsProblem = True
                        msg = vbNewLine & LineCount & " " & msg & " " & ErrorMessage
                        My.Computer.FileSystem.WriteAllText(errorfilename, msg, True, System.Text.Encoding.ASCII)
                        Continue While
                    End If

                    Select Case waterbodytype

                        Case StandardParameters.EPAGroundWater  'External Batch File GW
                            GroundWaterRoutines.CaptureGWOutput(nchem, FileNameClass.ZtsFile, numHoriz.Text, GWpeakConc, GWsimAvgConc, GWthroughputs, GWpostBTavg, GWBreakThroughdays)
                            DisplayGW_Results(nchem, GWpeakConc, GWsimAvgConc, GWthroughputs, GWpostBTavg, GWBreakThroughdays)
                            GW_IndividualScenarioOutput_unappended(run_id, finalmsg, finalmsgDeg1, finalmsgDeg2)

                            My.Computer.FileSystem.WriteAllText(gw_summaryfilename, finalmsg, True, System.Text.Encoding.ASCII)

                        Case Else 'External Batch File SW


                            If isMinimumBatchOutput.Checked = False Then
                                'Previous versions appended string, but might get too long for ESA batch, Here I append each line insted of accumulating

                                'filename = workingDirectory.Text & Trim(ioFamilyNameBox.Text) & "_" & Trim(scenarioID.Text) & "_" & StandardParameters.WaterBodyName(WaterBodyType) & "_Parent.txt"

                                filename = FileNameClass.VVWMoutputFileParent
                                GetResultsFromOutputFiles(filename, run_id, finalmsg)
                                My.Computer.FileSystem.WriteAllText(sw_summaryfilename, finalmsg, True, System.Text.Encoding.ASCII)


                                'why arent there degradates here?


                                'Special ESA frequency of return**************************************************
                                If IsAddFrequencyReturn.Checked Then
                                    filename = FileNameClass.VVWMoutputFileParentESA
                                    GetResultsFromOutputFiles(filename, run_id, finalmsg)
                                    My.Computer.FileSystem.WriteAllText(EXTRAsummaryFileName, finalmsg, True, System.Text.Encoding.ASCII)

                                    If Deg1CheckBox.Checked Then
                                        filename = FileNameClass.VVWMoutputFileDeg1ESA
                                        GetResultsFromOutputFiles(filename, run_id, finalmsg)
                                        My.Computer.FileSystem.WriteAllText(EXTRAsummaryFileNameDeg1, finalmsg, True, System.Text.Encoding.ASCII)
                                    End If

                                    If Deg2CheckBox.Checked Then
                                        filename = FileNameClass.VVWMoutputFileDeg2ESA
                                        GetResultsFromOutputFiles(filename, run_id, finalmsg)
                                        My.Computer.FileSystem.WriteAllText(EXTRAsummaryFileNameDeg2, finalmsg, True, System.Text.Encoding.ASCII)
                                    End If
                                End If
                            End If
                    End Select
                Next
            End While
        End Using








    End Sub


    Private Sub JustReformatTheScenarios(ByVal BatchFileName As String)
        Dim filename As String
        Dim i As Int16
        Dim fail As String
        Dim msg As String
        'Start reading the External Batch Run File input file
        Using MyReader As New Microsoft.VisualBasic.FileIO.TextFieldParser(BatchFileName)
            MyReader.TextFieldType = FileIO.FieldType.Delimited
            MyReader.SetDelimiters(",")

            i = 1


            MyReader.ReadLine()  'Header Lines
            While Not MyReader.EndOfData
                i = i + 1

                fail = ""


                ReadExternalFileInputs(MyReader, fail)

                If fail <> "" Then
                    msg = vbNewLine & i & " " & fail
                    My.Computer.FileSystem.WriteAllText("errors.txt", msg, True, System.Text.Encoding.ASCII)
                    Continue While
                End If


                filename = FileNameClass.PWCNewScenariosFilename & ".scn2"


                writeScenarioInfoToFile(filename)


            End While
        End Using



    End Sub





    Public Sub ReadExternalFileInputs(ByVal MyReader As Microsoft.VisualBasic.FileIO.TextFieldParser, ByRef fail As String)
        'newer scenario batch file read routine
        Dim currentRow As String()

        Dim emergence_date As Date  ' emergence
        Dim maturity_date As Date  ' maturity
        Dim harvest_date As Date  ' harvest

        Dim ii, jj, kk As Double
        Dim ss As Integer

        'Defaults
        pfac.Text = "1.0"
        snowmelt.Text = "0.274"
        NumberOfFactors.Text = "2"
        canopyHeight1.Text = "1.0"
        noIrrigation.Checked = True
        CropCyclesPerYear.Text = "1"


        Try
            currentRow = MyReader.ReadFields
            scenarioID.Text = currentRow(0)
            weatherBox.Text = EsaBatchscenarioDirectory.Text & currentRow(2) & "_grid.wea"
            ScenarioLatBox.Text = currentRow(9)
            evapDepth.Text = currentRow(11)
            ireg.Text = currentRow(12)

            ss = 1
            Select Case currentRow(13)
                Case 0
                    noIrrigation.Checked = True
                Case 1
                    overCanopy.Checked = True
                Case 2
                    underCanopy.Checked = True
                Case Else
                    noIrrigation.Checked = True

            End Select

            ss = 2
            rateIrrig.Text = currentRow(14)
            depletion.Text = currentRow(15)
            fleach.Text = currentRow(16)


            ss = 3
            ii = Convert.ToDouble(currentRow(18))
            jj = Convert.ToDouble(currentRow(19))
            kk = Convert.ToDouble(currentRow(20))

            emergence_date = DateAdd(DateInterval.Day, ii, #1/1/1900#)
            maturity_date = DateAdd(DateInterval.Day, jj, #1/1/1900#)
            harvest_date = DateAdd(DateInterval.Day, kk, #1/1/1900#)
            ss = 4

            canopyHoldup1.Text = currentRow(25)
            canopyCover1.Text = currentRow(26)
            rootDepth1.Text = currentRow(27)

            cn1.Text = currentRow(28)
            cn2.Text = currentRow(29)

            cFactor1.Text = currentRow(30)
            cFactor2.Text = currentRow(31)

            uslep.Text = currentRow(32)
            uslek.Text = currentRow(33)
            uslels.Text = currentRow(34)

            slope.Text = currentRow(35)

            ss = 5

            'pwc has a maximum of 8 horizons
            If Convert.ToInt16(currentRow(36)) > 8 Then
                numHoriz.Text = "8"
            Else
                numHoriz.Text = currentRow(36)
            End If

            thk1.Text = currentRow(37)
            thk2.Text = currentRow(38)
            thk3.Text = currentRow(39)
            thk4.Text = currentRow(40)
            thk5.Text = currentRow(41)
            thk6.Text = currentRow(42)
            thk7.Text = currentRow(43)
            thk8.Text = currentRow(44)


            ss = 6
            SoilProperty.thick(0).Text = currentRow(37)
            SoilProperty.thick(1).Text = currentRow(38)
            SoilProperty.thick(2).Text = currentRow(39)
            SoilProperty.thick(3).Text = currentRow(40)
            SoilProperty.thick(4).Text = currentRow(41)
            SoilProperty.thick(5).Text = currentRow(42)
            SoilProperty.thick(6).Text = currentRow(43)
            SoilProperty.thick(7).Text = currentRow(44)

            bd1.Text = currentRow(45)
            bd2.Text = currentRow(46)
            bd3.Text = currentRow(47)
            bd4.Text = currentRow(48)
            bd5.Text = currentRow(49)
            bd6.Text = currentRow(50)
            bd7.Text = currentRow(51)
            bd8.Text = currentRow(52)


            ss = 7
            maxCap1.Text = currentRow(53)
            maxCap2.Text = currentRow(54)
            maxCap3.Text = currentRow(55)
            maxCap4.Text = currentRow(56)
            maxCap5.Text = currentRow(57)
            maxCap6.Text = currentRow(58)
            maxCap7.Text = currentRow(59)
            maxCap8.Text = currentRow(60)

            minCap1.Text = currentRow(61)
            minCap2.Text = currentRow(62)
            minCap3.Text = currentRow(63)
            minCap4.Text = currentRow(64)
            minCap5.Text = currentRow(65)
            minCap6.Text = currentRow(66)
            minCap7.Text = currentRow(67)
            minCap8.Text = currentRow(68)

            oc1.Text = currentRow(69)
            oc2.Text = currentRow(70)
            oc3.Text = currentRow(71)
            oc4.Text = currentRow(72)
            oc5.Text = currentRow(73)
            oc6.Text = currentRow(74)
            oc7.Text = currentRow(75)
            oc8.Text = currentRow(76)
            ss = 8
            sand1.Text = currentRow(77)
            sand2.Text = currentRow(78)
            sand3.Text = currentRow(79)
            sand4.Text = currentRow(80)
            sand5.Text = currentRow(81)
            sand6.Text = currentRow(82)
            sand7.Text = currentRow(83)
            sand8.Text = currentRow(84)

            clay1.Text = currentRow(85)
            clay2.Text = currentRow(86)
            clay3.Text = currentRow(87)
            clay4.Text = currentRow(88)
            clay5.Text = currentRow(89)
            clay6.Text = currentRow(90)
            clay7.Text = currentRow(91)
            clay8.Text = currentRow(92)

            ss = 9
            FileNameClass.PWCNewScenariosFilename = currentRow(95)


            'Set Compartment discretizations
            Dim soildepth As Single
            soildepth = 0.0

            For i As Integer = 0 To numHoriz.Text - 1

                If soildepth <= 10.0 Then 'beginning depth of compartment

                    SoilProperty.compartment(i).Text = Math.Max(1, Convert.ToInt16(SoilProperty.thick(i).Text * 10))
                ElseIf soildepth > 10.0 And soildepth <= 100.0 Then

                    ''You must have the *1 here to fisrt convert the text to a single, otherwise the text decimal throws vb off
                    SoilProperty.compartment(i).Text = Math.Max(1, Convert.ToInt16(SoilProperty.thick(i).Text * 1))

                Else

                    SoilProperty.compartment(i).Text = Math.Max(1, Convert.ToInt16(SoilProperty.thick(i).Text * 0.1))
                End If
                soildepth = soildepth + SoilProperty.thick(i).Text

            Next
            ss = 10
            compart1.Text = SoilProperty.compartment(0).Text
            compart2.Text = SoilProperty.compartment(1).Text
            compart3.Text = SoilProperty.compartment(2).Text
            compart4.Text = SoilProperty.compartment(3).Text
            compart5.Text = SoilProperty.compartment(4).Text
            compart6.Text = SoilProperty.compartment(5).Text
            compart7.Text = SoilProperty.compartment(6).Text
            compart8.Text = SoilProperty.compartment(7).Text

            '########## CROP Dates ###################################

            dayEmerge1.Text = DatePart(DateInterval.Day, emergence_date)
            monthEmerge1.Text = DatePart(DateInterval.Month, emergence_date)

            dayMature1.Text = DatePart(DateInterval.Day, maturity_date)
            monthMature1.Text = DatePart(DateInterval.Month, maturity_date)

            dayHarvest1.Text = DatePart(DateInterval.Day, harvest_date)
            monthHarvest1.Text = DatePart(DateInterval.Month, harvest_date)

            dayFactor1.Text = dayEmerge1.Text
            monFactor1.Text = monthEmerge1.Text

            dayFactor2.Text = dayHarvest1.Text
            monFactor2.Text = monthHarvest1.Text
            ss = 11

        Catch ex As Exception

            fail = ss & " " & ex.Message





        End Try




    End Sub
End Class
