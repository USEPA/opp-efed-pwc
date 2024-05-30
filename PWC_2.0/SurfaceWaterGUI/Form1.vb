Imports System.Text.RegularExpressions
Imports System.Math
Imports System.Windows.Forms.DataVisualization.Charting
Imports System.Globalization




Public Class Form1
    Dim firstrun As Boolean = True

    Public Sub New()
        ' This call is required by the designer.
        InitializeComponent()


        System.Threading.Thread.CurrentThread.CurrentCulture = System.Globalization.CultureInfo.GetCultureInfo("en-US")


        Me.Text = StandardParameters.ProgramName & StandardParameters.VersionNumber

    End Sub

    Protected Overrides Sub Finalize()
        MyBase.Finalize()
    End Sub

    Private Sub RunButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RunButton.Click
        Dim AppPath As String
        Dim ThereIsproblem As Boolean

        Dim WaterBodyType As String
        Dim path As String

        AppPath = My.Application.Info.DirectoryPath()

        RunButton.Text = "WAIT"
        RunButton.ForeColor = Color.Red

        If BigCalibration.Checked = False Then
            Try
                System.IO.Directory.SetCurrentDirectory(workingDirectoryLabel.Text)
            Catch ex As Exception
                MsgBox(ex.Message & ":   Working Directory has problem with existence")
                Return
            End Try
        End If

        If BigCalibration.Checked Then
            WaterBodyType = StandardParameters.NoWaterBody
            RunLoops(WaterBodyType, ThereIsproblem)
            If ThereIsproblem Then
                RunButton.Text = "Try Again"
                Return
            End If
            RunButton.Text = "RUN"
            Exit Sub
        End If


        If CalibrateCN.Checked Then
            If firstrun Then
                'Initialize calibration level
                For i As Integer = 0 To NumberOfFactors.Text - 1
                    USLE.CNlevel(i) = -1  ' the -1 means that first iterations will occur on the 1/10^-1 or the tens place (e.g 60 to 70 to 80...)
                Next
            End If
            WaterBodyType = StandardParameters.NoWaterBody
            RunLoops(WaterBodyType, ThereIsproblem)
            If ThereIsproblem Then
                RunButton.Text = "Try Again"
                Return
            End If
            Exit Sub
        End If


        If ESA_Run.Checked Then
            RunLoops("Dummy", ThereIsproblem)
            If ThereIsproblem Then  'these were added to prevent multiple error messages on same run
                RunButton.Text = "Try Again"
                Return
            End If

            RunButton.Text = "RUN"
            RunButton.ForeColor = Color.Black
            StatusLabel.Text = "Run completed at " & Now
            StatusLabel.ForeColor = Color.Purple
            Exit Sub         'Batch ESA runs terminate and other runs types are not made
        End If


        '################################################################################################################
        'NEW EXTERNAL FILE RUN  : ABILITY TO USE ALL WATERBODIES------incomplete'

        If ExternalBatchFileRun.Checked Then
            If EPAreservoir.Checked Then
                WaterBodyType = StandardParameters.EpaReservoir
                RunLoops(WaterBodyType, ThereIsproblem)
                If ThereIsproblem Then  'these were added to prevent multiple error messages on same run
                    RunButton.Text = "Try Again"
                    Return
                End If
            End If

            If EPApond.Checked Then
                WaterBodyType = StandardParameters.EpaPond
                RunLoops(WaterBodyType, ThereIsproblem)
                If ThereIsproblem Then  'these were added to prevent multiple error messages on same run
                    RunButton.Text = "Try Again"
                    Return
                End If
            End If


            If GroundWater.Checked Then
                WaterBodyType = StandardParameters.EPAGroundWater
                RunLoops(WaterBodyType, ThereIsproblem)
                If ThereIsproblem Then  'these were added to prevent multiple error messages on same run
                    RunButton.Text = "Try Again"
                    Return
                End If
            End If


            If EPAreservoir.Checked Then
            ElseIf EPApond.Checked Then
            ElseIf VaryVolFlow.Checked Then
            ElseIf ConstVolFlow.Checked Then
            ElseIf ConstVolNoFlow.Checked Then
            ElseIf GroundWater.Checked Then
            ElseIf PRZMonly.Checked Then
            Else
                MsgBox("You need to select a water body. See the Water Body Tab.")
                RunButton.Text = "Try Again"
                Return
            End If


            path = System.IO.Directory.GetCurrentDirectory()

            RunButton.Text = "RUN"
            RunButton.ForeColor = Color.Black
            StatusLabel.Text = "Run completed at " & Now
            StatusLabel.ForeColor = Color.Purple



            Exit Sub



        End If



        '################################################################################################################



        '*****   The following can be run in series:

        If EPAreservoir.Checked Then
            WaterBodyType = StandardParameters.EpaReservoir
            RunLoops(WaterBodyType, ThereIsproblem)
            If ThereIsproblem Then  'these were added to prevent multiple error messages on same run
                RunButton.Text = "Try Again"
                Return
            End If
        End If

        If EPApond.Checked Then
            WaterBodyType = StandardParameters.EpaPond
            RunLoops(WaterBodyType, ThereIsproblem)
            If ThereIsproblem Then
                RunButton.Text = "Try Again"
                Return
            End If
        End If

        If VaryVolFlow.Checked Then
            WaterBodyType = StandardParameters.CustomVVWM
            RunLoops(WaterBodyType, ThereIsproblem)
            If ThereIsproblem Then
                RunButton.Text = "Try Again"
                Return
            End If
        End If

        If ConstVolFlow.Checked Then
            WaterBodyType = StandardParameters.ConstVolFlo
            RunLoops(WaterBodyType, ThereIsproblem)
            If ThereIsproblem Then
                RunButton.Text = "Try Again"
                Return
            End If
        End If

        If ConstVolNoFlow.Checked Then
            WaterBodyType = StandardParameters.ConstVolNoFlo
            RunLoops(WaterBodyType, ThereIsproblem)
            If ThereIsproblem Then
                RunButton.Text = "Try Again"
                Return
            End If
        End If

        If GroundWater.Checked Then
            WaterBodyType = StandardParameters.EPAGroundWater
            RunLoops(WaterBodyType, ThereIsproblem)
            If ThereIsproblem Then
                RunButton.Text = "Try Again"
                Return
            End If
        End If

        If PRZMonly.Checked Then
            WaterBodyType = StandardParameters.NoWaterBody
            RunLoops(WaterBodyType, ThereIsproblem)
            If ThereIsproblem Then
                RunButton.Text = "Try Again"
                Return
            End If
        End If


        If EPAreservoir.Checked Then
        ElseIf EPApond.Checked Then
        ElseIf VaryVolFlow.Checked Then
        ElseIf ConstVolFlow.Checked Then
        ElseIf ConstVolNoFlow.Checked Then
        ElseIf GroundWater.Checked Then
        ElseIf PRZMonly.Checked Then
        Else
            MsgBox("You need to select a water body. See the Water Body Tab.")
            RunButton.Text = "Try Again"
            Return
        End If


        path = System.IO.Directory.GetCurrentDirectory()

        RunButton.Text = "RUN"
        RunButton.ForeColor = Color.Black
        StatusLabel.Text = "Run completed at " & Now
        StatusLabel.ForeColor = Color.Purple
    End Sub


    Private Sub RunLoops(ByVal WaterBodyType As String, ByRef ThereIsProblem As Boolean)
        Dim finalmsg As String
        Dim finalmsgDeg1 As String
        Dim finalmsgDeg2 As String
        Dim runflag As Boolean
        Dim nchem As Integer
        Dim filename As String

        Dim scenarioName As String
        Dim run_id As String

        Dim EXTRAsummaryFileName As String = ""
        Dim EXTRAsummaryFileNameDeg1 As String = ""
        Dim EXTRAsummaryFileNameDeg2 As String = ""

        Dim summaryFileName As String
        Dim GW_summaryFileName As String

        Dim summaryFileNames(3) As String  'batch summary filename for parent daughter, granddaughter

        Dim errorFileName As String

        Dim msg As String
        Dim singleOutput As String = ""
        Dim checkvaluesMessage As String

        Dim GWpeakConc(3) As Single
        Dim GWsimAvgConc(3) As Single
        Dim GWthroughputs(3) As Single
        Dim GWpostBTavg(3) As Single
        Dim GWBreakThroughdays(3) As Single

        Dim LineCount As Integer

        Dim ErrorMessage As String
        ErrorMessage = ""


        ThereIsProblem = False
        finalmsg = ""
        finalmsgDeg1 = ""
        finalmsgDeg2 = ""

        nchem = 1
        If Deg1CheckBox.Checked Then nchem = 2
        If Deg2CheckBox.Checked Then nchem = 3


        '**********************  Big Calibration ***********************************************
        If BigCalibration.Checked Then
            If RunoffCalib.Checked Then
                DoBigCalibration(ThereIsProblem, rDepthBox, rBypassBox, rDeclineBox, True)
            Else
                DoBigCalibration(ThereIsProblem, eDepthBox, eBypassBox, eDeclineBox, False)
            End If
            Exit Sub
        End If


        '************************* CN Calibration ******************************************************
        If CalibrateCN.Checked Then
            For i As Integer = 1 To IterationsHydro.Text
                RunPrograms(WaterBodyType, 0, runflag, ErrorMessage)
                If runflag Then
                    MsgBox(ErrorMessage)
                    ThereIsProblem = True
                    Exit Sub
                End If
                MakeNewCurveNumbers()
                firstrun = False
            Next

            RunButton.Text = "Iterate Again or just run"
            Exit Sub
        End If


        '***************** New External Batch File SAM-compatible **************************
        If ExternalBatchFileRun.Checked Then




            StartExternalBatchRun(WaterBodyType, ThereIsProblem)

            Exit Sub
        End If
        '***********************************************************************************



        '*******************************  ESA Batch Runs *******************************************************


        'this section could probably be consolidated with the class ExternalBatchRuns some routines are common
        If ESA_Run.Checked Then

            Dim esaHeader As String
            Dim gw_header As String

            esaHeader = String.Format("Line Batch Run ID             {0}Peak{0}1-day{0}Yr{0}overall{0}4-day{0}21-day{0}60-day{0}90-day{0}PW_pk{0}PW_21{1}", vbTab, vbNewLine)
            gw_header = String.Format("{1,-25}{0}{2,10}{0}{3,10}{0}{4,10}{0}{5,10}{0}{6,10}{7}", vbTab, "GW Run ID", "Peak", "Breakthru", "Thruput", "PostBT Avg", "Sim Avg", vbNewLine)


            LineCount = 0

            If Not System.IO.File.Exists(BatchChemFile.Text) Then
                MsgBox("Batch file does not exist")
                ThereIsProblem = True
                Exit Sub
            End If

            workingDirectoryLabel.Text = System.IO.Path.GetDirectoryName(BatchChemFile.Text) & "\"
            System.IO.Directory.SetCurrentDirectory(workingDirectoryLabel.Text)

            'Delete error file if it exists and recreate with Header
            errorFileName = workingDirectoryLabel.Text & "ErrorSummary.txt"
            If IO.File.Exists(errorFileName) Then IO.File.Delete(errorFileName)
            msg = "List of Errors in batch run. Line numbers correspond to line numbers in input file."
            My.Computer.FileSystem.WriteAllText(errorFileName, msg, False, System.Text.Encoding.ASCII)

            'Delete summary file if it exists and recreate with Header
            summaryFileName = workingDirectoryLabel.Text & "Summary_SW.txt"
            If IO.File.Exists(summaryFileName) Then IO.File.Delete(summaryFileName)
            My.Computer.FileSystem.WriteAllText(summaryFileName, esaHeader, False, System.Text.Encoding.ASCII)

            'do the same for the gw file
            GW_summaryFileName = workingDirectoryLabel.Text & "Summary_GW.txt"
            If IO.File.Exists(GW_summaryFileName) Then IO.File.Delete(GW_summaryFileName)
            My.Computer.FileSystem.WriteAllText(GW_summaryFileName, gw_header, False, System.Text.Encoding.ASCII)



            '********************* ADDITIONAL RETURN FREQUENCY ************************************************
            If IsAddFrequencyReturn.Checked And WaterBodyType <> StandardParameters.EPAGroundWater Then
                'Delete summary file if it exists and recreate with Header

                EXTRAsummaryFileName = workingDirectoryLabel.Text & "Summary_SW_" & Trim(ReturnFrequency.Text) & ".txt"
                If IO.File.Exists(EXTRAsummaryFileName) Then IO.File.Delete(EXTRAsummaryFileName)

                My.Computer.FileSystem.WriteAllText(EXTRAsummaryFileName, esaHeader, False, System.Text.Encoding.ASCII)

                If Deg1CheckBox.Checked Then
                    EXTRAsummaryFileNameDeg1 = workingDirectoryLabel.Text & "Summary_SW_Deg1_" & Trim(ReturnFrequency.Text) & ".txt"
                    If IO.File.Exists(EXTRAsummaryFileNameDeg1) Then IO.File.Delete(EXTRAsummaryFileNameDeg1)

                    My.Computer.FileSystem.WriteAllText(EXTRAsummaryFileNameDeg1, esaHeader, False, System.Text.Encoding.ASCII)
                End If

                If Deg2CheckBox.Checked Then
                    EXTRAsummaryFileNameDeg2 = workingDirectoryLabel.Text & "Summary_SW_Deg2_" & Trim(ReturnFrequency.Text) & ".txt"
                    If IO.File.Exists(EXTRAsummaryFileNameDeg2) Then IO.File.Delete(EXTRAsummaryFileNameDeg2)

                    My.Computer.FileSystem.WriteAllText(EXTRAsummaryFileNameDeg2, esaHeader, False, System.Text.Encoding.ASCII)
                End If

            End If
            '**********************************************************************************************************************


            'Start reading the External Batch Run File input file
            Using MyReader As New Microsoft.VisualBasic.FileIO.TextFieldParser(BatchChemFile.Text)
                MyReader.TextFieldType = FileIO.FieldType.Delimited
                MyReader.SetDelimiters(",")
                MyReader.ReadLine()  'Header Line
                While Not MyReader.EndOfData

                    LineCount = LineCount + 1

                    ReadEsaInputs(MyReader, WaterBodyType, msg)  'Returns Waterbody type and error message, Loads inputs to GUI
                    If msg <> "" Then
                        msg = vbNewLine & LineCount & " " & msg
                        My.Computer.FileSystem.WriteAllText(errorFileName, msg, True, System.Text.Encoding.ASCII)
                        Continue While
                    End If

                    '************************************************
                    msg = checkvalues()
                    If msg <> "" Then
                        msg = vbNewLine & LineCount & " " & msg
                        My.Computer.FileSystem.WriteAllText(errorFileName, msg, True, System.Text.Encoding.ASCII)
                        Continue While
                    End If
                    msg = checkWaterBodyType(WaterBodyType)
                    If msg <> "" Then
                        msg = vbNewLine & LineCount & " " & msg
                        My.Computer.FileSystem.WriteAllText(errorFileName, msg, True, System.Text.Encoding.ASCII)
                        Continue While
                    End If
                    '************************************************


                    RunPrograms(WaterBodyType, 0, runflag, ErrorMessage)

                    If runflag Then
                        ThereIsProblem = True
                        msg = vbNewLine & LineCount & " " & msg & " " & ErrorMessage
                        My.Computer.FileSystem.WriteAllText(errorFileName, msg, True, System.Text.Encoding.ASCII)
                        Continue While
                    End If

                    run_id = String.Format("{0} {1}_{2}", LineCount, ioFamilyNameBox.Text, scenarioID.Text)



                    Select Case WaterBodyType


                        Case StandardParameters.EPAGroundWater  'External Batch File GW
                            GroundWaterRoutines.CaptureGWOutput(nchem, FileNameClass.ZtsFile, numHoriz.Text, GWpeakConc, GWsimAvgConc, GWthroughputs, GWpostBTavg, GWBreakThroughdays)
                            DisplayGW_Results(nchem, GWpeakConc, GWsimAvgConc, GWthroughputs, GWpostBTavg, GWBreakThroughdays)
                            GW_IndividualScenarioOutput_unappended(run_id, finalmsg, finalmsgDeg1, finalmsgDeg2)

                            My.Computer.FileSystem.WriteAllText(GW_summaryFileName, finalmsg, True, System.Text.Encoding.ASCII)

                        Case Else 'External Batch File SW

                            'Previous versions appended string, but might get too long for ESA batch, Here I append each line insted of accumulating

                            'filename = workingDirectory.Text & Trim(ioFamilyNameBox.Text) & "_" & Trim(scenarioID.Text) & "_" & StandardParameters.WaterBodyName(WaterBodyType) & "_Parent.txt"

                            filename = FileNameClass.VVWMoutputFileParent
                            GetResultsFromOutputFiles(filename, run_id, finalmsg)
                            My.Computer.FileSystem.WriteAllText(summaryFileName, finalmsg, True, System.Text.Encoding.ASCII)


                            'why arent there degradates here?
                            ' because esa does not do degaradates 

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
                    End Select





                End While
            End Using

            Exit Sub
        End If
        '*******************************  END ESA Batch Runs *******************************************************





        '***************************** Scenario & Date Batch Runs **************************************************
        Dim substring As String

        'Delete error file if it exists and recreate with Header
        errorFileName = workingDirectoryLabel.Text & "ErrorSummary.txt"
        If IO.File.Exists(errorFileName) Then IO.File.Delete(errorFileName)
        msg = "List of Errors in batch run. Line numbers correspond to line numbers in input file."
        My.Computer.FileSystem.WriteAllText(errorFileName, msg, False, System.Text.Encoding.ASCII)




        If BatchRun.Checked Or ApplyWindow.Checked Then
            substring = workingDirectoryLabel.Text & "Summary_" & StandardParameters.WaterBodyName(WaterBodyType) & "_" & Trim(ioFamilyNameBox.Text)
            summaryFileName = substring & ".txt"

            If IsAddFrequencyReturn.Checked Then
                'Delete summary file if it exists and recreate with Header
                EXTRAsummaryFileName = substring & "_" & Trim(ReturnFrequency.Text) & ".txt"


                If IO.File.Exists(EXTRAsummaryFileName) Then IO.File.Delete(EXTRAsummaryFileName)
                msg = String.Format("Batch Run ID                 {0}Peak{0}1-day{0}Yr{0}overall{0}4-day{0}21-day{0}60-day{0}90-day{0}PW_pk{0}PW_21{1}", vbTab, vbNewLine)
                My.Computer.FileSystem.WriteAllText(EXTRAsummaryFileName, msg, False, System.Text.Encoding.ASCII)

                If Deg1CheckBox.Checked Then
                    EXTRAsummaryFileNameDeg1 = substring & "_deg1_" & Trim(ReturnFrequency.Text) & ".txt"
                    If IO.File.Exists(EXTRAsummaryFileNameDeg1) Then IO.File.Delete(EXTRAsummaryFileNameDeg1)
                    msg = String.Format("Batch Run ID                 {0}Peak{0}1-day{0}Yr{0}overall{0}4-day{0}21-day{0}60-day{0}90-day{0}PW_pk{0}PW_21{1}", vbTab, vbNewLine)
                    My.Computer.FileSystem.WriteAllText(EXTRAsummaryFileNameDeg1, msg, False, System.Text.Encoding.ASCII)
                End If

                If Deg2CheckBox.Checked Then
                    EXTRAsummaryFileNameDeg2 = substring & "_deg2_" & Trim(ReturnFrequency.Text) & ".txt"
                    If IO.File.Exists(EXTRAsummaryFileNameDeg2) Then IO.File.Delete(EXTRAsummaryFileNameDeg2)
                    msg = String.Format("Batch Run ID                 {0}Peak{0}1-day{0}Yr{0}overall{0}4-day{0}21-day{0}60-day{0}90-day{0}PW_pk{0}PW_21{1}", vbTab, vbNewLine)
                    My.Computer.FileSystem.WriteAllText(EXTRAsummaryFileNameDeg2, msg, False, System.Text.Encoding.ASCII)
                End If

            End If



            Dim countOut As Integer
            Dim interval As Integer
            Dim spanDays As Integer
            Dim windowRuns As Integer

            If ApplyWindow.Checked Then
                spanDays = SpanTB.Text
                interval = IntervalTB.Text
                windowRuns = spanDays \ interval + 1  'Integer Operator \
            Else
                spanDays = 0
                interval = 1
                windowRuns = 1
            End If

            If BatchRun.Checked Then 'reading a scenario list
                countOut = ScenariosList.Items.Count * windowRuns - 1
            Else  'reading scenario from UI 
                countOut = windowRuns - 1
            End If


            'Dimension the output Arrays

            Dim EEC___1dayAvg(countOut) As Single
            Dim EEC___4dayAvg(countOut) As Single
            Dim EEC__21dayAvg(countOut) As Single
            Dim EEC__60dayAvg(countOut) As Single
            Dim EEC__90dayAvg(countOut) As Single
            Dim EEC_365dayAvg(countOut) As Single
            Dim EEC__TotalAvg(countOut) As Single
            Dim EEC__1Benthic(countOut) As Single
            Dim EEC_21Benthic(countOut) As Single

            Dim EEC___1dayAvgDeg1(countOut) As Single
            Dim EEC___4dayAvgDeg1(countOut) As Single
            Dim EEC__21dayAvgDeg1(countOut) As Single
            Dim EEC__60dayAvgDeg1(countOut) As Single
            Dim EEC__90dayAvgDeg1(countOut) As Single
            Dim EEC_365dayAvgDeg1(countOut) As Single
            Dim EEC__TotalAvgDeg1(countOut) As Single
            Dim EEC__1BenthicDeg1(countOut) As Single
            Dim EEC_21BenthicDeg1(countOut) As Single

            Dim EEC___1dayAvgDeg2(countOut) As Single
            Dim EEC___4dayAvgDeg2(countOut) As Single
            Dim EEC__21dayAvgDeg2(countOut) As Single
            Dim EEC__60dayAvgDeg2(countOut) As Single
            Dim EEC__90dayAvgDeg2(countOut) As Single
            Dim EEC_365dayAvgDeg2(countOut) As Single
            Dim EEC__TotalAvgDeg2(countOut) As Single
            Dim EEC__1BenthicDeg2(countOut) As Single
            Dim EEC_21BenthicDeg2(countOut) As Single

            Dim RunID(countOut), TempRunID(countOut) As String

            Dim avg1, avg4, avg21, avg60, avg90, avg365, avgTotal, benthic1, benthic21 As Single

            Dim worstscenario As String = ""
            Dim worstconcentration As Single = "0"




            Dim runcount As Integer
            runcount = 0


            'Create a 1 item collection just to get the outerloop to run one time when there is not a scenario batch



            'probably a better way to do this but I am just hacking through
            Dim lst As New List(Of String) From {scenarioID.Text}

            Dim ggggg As Object

            If BatchRun.Checked Then
                ggggg = ScenariosList.Items
            Else
                ggggg = lst
            End If


            LineCount = 0 ' linecount is only scenarios
            Dim ErrMessageScnBatchRun As String
            ErrMessageScnBatchRun = ""

            Try


                ' For Each selectedScenario As String In ScenariosList.Items
                For Each selectedScenario As String In ggggg
                    LineCount = LineCount + 1

                    If BatchRun.Checked Then
                        If LoadScenario(selectedScenario) = False Then Continue For 'cycle if a scenario does not load
                    End If

                    scenarioName = System.IO.Path.GetFileNameWithoutExtension(selectedScenario)


                    '************************************************
                    checkvaluesMessage = checkvalues()
                    If checkvaluesMessage <> "" Then 'Signifies problem with input.
                        ErrMessageScnBatchRun = vbNewLine & LineCount & "  " & scenarioName & " " & checkvaluesMessage
                        My.Computer.FileSystem.WriteAllText(errorFileName, ErrMessageScnBatchRun, True, System.Text.Encoding.ASCII)
                        Continue For
                    End If

                    checkvaluesMessage = checkWaterBodyType(WaterBodyType)
                    If checkvaluesMessage <> "" Then 'Signifies problem with input.
                        ErrMessageScnBatchRun = vbNewLine & LineCount & " " & checkvaluesMessage
                        My.Computer.FileSystem.WriteAllText(errorFileName, ErrMessageScnBatchRun, True, System.Text.Encoding.ASCII)
                        Continue For
                    End If





                    '*******Appwindow Loop *********************
                    For i As Integer = 0 To spanDays Step interval
                        RunPrograms(WaterBodyType, i, runflag, ErrorMessage)

                        If runflag Then
                            'MsgBox(ErrorMessage)
                            'ThereIsProblem = True
                            'Return

                            ErrMessageScnBatchRun = vbNewLine & LineCount & " " & ErrorMessage
                            My.Computer.FileSystem.WriteAllText(errorFileName, ErrMessageScnBatchRun, True, System.Text.Encoding.ASCII)
                            Continue For




                        End If
                        run_id = String.Format("{0}_+{1}", scenarioName, i)

                        RunID(runcount) = run_id


                        '********* Here is where we need to read different output files GW is different than the Water Bodies ****
                        If WaterBodyType = StandardParameters.EPAGroundWater Then

                            'Groundwater Needs Different File Read Summary

                            GroundWaterRoutines.CaptureGWOutput(nchem, FileNameClass.ZtsFile, numHoriz.Text, GWpeakConc, GWsimAvgConc, GWthroughputs, GWpostBTavg, GWBreakThroughdays)
                            DisplayGW_Results(nchem, GWpeakConc, GWsimAvgConc, GWthroughputs, GWpostBTavg, GWBreakThroughdays)

                            GW_IndividualScenarioOutput(run_id, finalmsg, finalmsgDeg1, finalmsgDeg2)

                        ElseIf WaterBodyType = StandardParameters.NoWaterBody Then
                            'do nothing with the water body summary

                        Else  ' Surface Water

                            IndividualScenarioOutput(run_id, finalmsg, finalmsgDeg1, finalmsgDeg2)



                            filename = FileNameClass.VVWMoutputFileParent
                            GetOutputFromVvwm(filename, avg1, avg4, avg21, avg60, avg90, avg365, avgTotal, benthic1, benthic21)
                            EEC___1dayAvg(runcount) = avg1
                            EEC___4dayAvg(runcount) = avg4
                            EEC__21dayAvg(runcount) = avg21
                            EEC__60dayAvg(runcount) = avg60
                            EEC__90dayAvg(runcount) = avg90
                            EEC_365dayAvg(runcount) = avg365
                            EEC__TotalAvg(runcount) = avgTotal
                            EEC__1Benthic(runcount) = benthic1
                            EEC_21Benthic(runcount) = benthic21

                            If Deg1CheckBox.Checked Then
                                filename = FileNameClass.VVWMoutputFileDeg1
                                GetOutputFromVvwm(filename, avg1, avg4, avg21, avg60, avg90, avg365, avgTotal, benthic1, benthic21)
                                EEC___1dayAvgDeg1(runcount) = avg1
                                EEC___4dayAvgDeg1(runcount) = avg4
                                EEC__21dayAvgDeg1(runcount) = avg21
                                EEC__60dayAvgDeg1(runcount) = avg60
                                EEC__90dayAvgDeg1(runcount) = avg90
                                EEC_365dayAvgDeg1(runcount) = avg365
                                EEC__TotalAvgDeg1(runcount) = avgTotal
                                EEC__1BenthicDeg1(runcount) = benthic1
                                EEC_21BenthicDeg1(runcount) = benthic21
                            End If

                            If Deg2CheckBox.Checked Then
                                filename = FileNameClass.VVWMoutputFileDeg2
                                GetOutputFromVvwm(filename, avg1, avg4, avg21, avg60, avg90, avg365, avgTotal, benthic1, benthic21)
                                EEC___1dayAvgDeg2(runcount) = avg1
                                EEC___4dayAvgDeg2(runcount) = avg4
                                EEC__21dayAvgDeg2(runcount) = avg21
                                EEC__60dayAvgDeg2(runcount) = avg60
                                EEC__90dayAvgDeg2(runcount) = avg90
                                EEC_365dayAvgDeg2(runcount) = avg365
                                EEC__TotalAvgDeg2(runcount) = avgTotal
                                EEC__1BenthicDeg2(runcount) = benthic1
                                EEC_21BenthicDeg2(runcount) = benthic21
                            End If





                            'Special frequency of return output   Appended**************************************************
                            If IsAddFrequencyReturn.Checked Then
                                filename = FileNameClass.VVWMoutputFileParentESA

                                GetResultsFromOutputFiles(filename, run_id, singleOutput)
                                My.Computer.FileSystem.WriteAllText(EXTRAsummaryFileName, singleOutput, True, System.Text.Encoding.ASCII)

                                If Deg1CheckBox.Checked Then
                                    filename = FileNameClass.VVWMoutputFileDeg1ESA
                                    GetResultsFromOutputFiles(filename, run_id, singleOutput)
                                    My.Computer.FileSystem.WriteAllText(EXTRAsummaryFileNameDeg1, singleOutput, True, System.Text.Encoding.ASCII)
                                End If

                                If Deg2CheckBox.Checked Then
                                    filename = FileNameClass.VVWMoutputFileDeg2ESA
                                    GetResultsFromOutputFiles(filename, run_id, singleOutput)
                                    My.Computer.FileSystem.WriteAllText(EXTRAsummaryFileNameDeg2, singleOutput, True, System.Text.Encoding.ASCII)
                                End If
                            End If
                        End If

                        If keepAllBatchFiles.Checked Then
                            My.Computer.FileSystem.CopyFile(FileNameClass.ZtsFile, FileNameClass.WorkingDirectory & run_id & "_" & System.IO.Path.GetFileName(FileNameClass.ZtsFile), True)

                            If IO.File.Exists(FileNameClass.VVWMoutputFileParent) Then
                                My.Computer.FileSystem.CopyFile(FileNameClass.VVWMoutputFileParent, FileNameClass.WorkingDirectory & run_id & "_" & System.IO.Path.GetFileName(FileNameClass.VVWMoutputFileParent), True)
                            End If

                            If IO.File.Exists(FileNameClass.VVWMoutputFileDeg1) Then
                                My.Computer.FileSystem.CopyFile(FileNameClass.VVWMoutputFileDeg1, FileNameClass.WorkingDirectory & run_id & "_" & System.IO.Path.GetFileName(FileNameClass.VVWMoutputFileDeg1), True)
                            End If

                            If IO.File.Exists(FileNameClass.VVWMoutputFileDeg2) Then
                                My.Computer.FileSystem.CopyFile(FileNameClass.VVWMoutputFileDeg2, FileNameClass.WorkingDirectory & run_id & "_" & System.IO.Path.GetFileName(FileNameClass.VVWMoutputFileDeg2), True)
                            End If

                            If IO.File.Exists(FileNameClass.VVWMoutputFileParentTS) Then
                                My.Computer.FileSystem.CopyFile(FileNameClass.VVWMoutputFileParentTS, FileNameClass.WorkingDirectory & run_id & "_" & System.IO.Path.GetFileName(FileNameClass.VVWMoutputFileParentTS), True)
                            End If

                            If IO.File.Exists(FileNameClass.VVWMoutputFileDeg1TS) Then
                                My.Computer.FileSystem.CopyFile(FileNameClass.VVWMoutputFileDeg1TS, FileNameClass.WorkingDirectory & run_id & "_" & System.IO.Path.GetFileName(FileNameClass.VVWMoutputFileDeg1TS), True)
                            End If

                            If IO.File.Exists(FileNameClass.VVWMoutputFileDeg2TS) Then
                                My.Computer.FileSystem.CopyFile(FileNameClass.VVWMoutputFileDeg2TS, FileNameClass.WorkingDirectory & run_id & "_" & System.IO.Path.GetFileName(FileNameClass.VVWMoutputFileDeg2TS), True)
                            End If
                        End If

                        runcount = runcount + 1

                    Next  ' end of the app window loop
                Next  'end batch scenario & Ap loops loop

            Catch ex As Exception

                MsgBox(ex.Message)



            End Try




            '************ WRITE OUTPUT FOR BATCH RUN

            'this uses  appended string: i.e one big string in a single write, should change to apppend on the fly
            If WaterBodyType = StandardParameters.EPAGroundWater Then
                WriteBatchOutputSummaryForGroundwater(summaryFileName, finalmsg, finalmsgDeg1, finalmsgDeg2)

            ElseIf WaterBodyType = StandardParameters.NoWaterBody Then

                'do nothing

            Else
                ' WriteBatchOutputSummary(summaryFileName & "_old", finalmsg, finalmsgDeg1, finalmsgDeg2)


                Dim testmsg As String
                testmsg = String.Format("{2,-30},{1}{3}{1}{4}{1}{5}{1}{6}{1}{7}{1}{8}{1}{9}{1}{10}{1}{11}", vbNewLine, vbTab, "Scenario Name", "1-d", "4-d", "21-d", "60-d", "90-d", "365-d", "Full", "1-db", "21-db")
                My.Computer.FileSystem.WriteAllText(summaryFileName, testmsg, False, System.Text.Encoding.ASCII)

                For i As Integer = 0 To countOut
                    testmsg = String.Format(" {0}{2,-30},{1}{3}{1}{4}{1}{5}{1}{6}{1}{7}{1}{8}{1}{9}{1}{10}{1}{11}", vbNewLine, vbTab, RunID(i),
                                            EEC___1dayAvg(i), EEC___4dayAvg(i), EEC__21dayAvg(i), EEC__60dayAvg(i), EEC__90dayAvg(i), EEC_365dayAvg(i), EEC__TotalAvg(i), EEC__1Benthic(i), EEC_21Benthic(i))
                    My.Computer.FileSystem.WriteAllText(summaryFileName, testmsg, True, System.Text.Encoding.ASCII)
                Next

                Dim eec1, eec4, eec21, eec60, eec90, eec365, eecFull, eecBenthic1, eecbenthic21 As Single
                Dim scenario1, scenario4, scenario21, scenario60, scenario90, scenario365, scenarioFull, scenarioBenthic1, scenariobenthic21 As String




                WriteBatchOutputFile(summaryFileName, "1-d", countOut, RunID, EEC___1dayAvg, scenario1, eec1)
                WriteBatchOutputFile(summaryFileName, "4-d", countOut, RunID, EEC___4dayAvg, scenario4, eec4)
                WriteBatchOutputFile(summaryFileName, "21-d", countOut, RunID, EEC__21dayAvg, scenario21, eec21)
                WriteBatchOutputFile(summaryFileName, "60-d", countOut, RunID, EEC__60dayAvg, scenario60, eec60)
                WriteBatchOutputFile(summaryFileName, "90-d", countOut, RunID, EEC__90dayAvg, scenario90, eec90)
                WriteBatchOutputFile(summaryFileName, "365-d", countOut, RunID, EEC_365dayAvg, scenario365, eec365)
                WriteBatchOutputFile(summaryFileName, "Full", countOut, RunID, EEC__TotalAvg, scenarioFull, eecFull)
                WriteBatchOutputFile(summaryFileName, "1-db", countOut, RunID, EEC__1Benthic, scenarioBenthic1, eecBenthic1)
                WriteBatchOutputFile(summaryFileName, "21-db", countOut, RunID, EEC_21Benthic, scenariobenthic21, eecbenthic21)





                Select Case WaterBodyType

                    Case StandardParameters.EpaPond

                        scenario1Pond = scenario1
                        scenario4Pond = scenario4
                        scenario21Pond = scenario21
                        scenario60Pond = scenario60
                        scenario90Pond = scenario90
                        scenario365Pond = scenario365
                        scenarioFullPond = scenarioFull
                        scenarioBenthic1Pond = scenarioBenthic1
                        scenariobenthic21Pond = scenariobenthic21

                        eec1Pond = eec1
                        eec4Pond = eec4
                        eec21Pond = eec21
                        eec60Pond = eec60
                        eec90Pond = eec90
                        eec365Pond = eec365
                        eecFullPond = eecFull
                        eecBenthic1Pond = eecBenthic1
                        eecbenthic21Pond = eecbenthic21

                        PondRB.Checked = True

                        '   AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA()
                    Case StandardParameters.EpaReservoir

                        scenario1Res = scenario1
                        scenario4Res = scenario4
                        scenario21Res = scenario21
                        scenario60Res = scenario60
                        scenario90Res = scenario90
                        scenario365Res = scenario365
                        scenarioFullRes = scenarioFull
                        scenarioBenthic1Res = scenarioBenthic1
                        scenariobenthic21Res = scenariobenthic21

                        eec1Res = eec1
                        eec4Res = eec4
                        eec21Res = eec21
                        eec60Res = eec60
                        eec90Res = eec90
                        eec365Res = eec365
                        eecFullRes = eecFull
                        eecBenthic1Res = eecBenthic1
                        eecbenthic21Res = eecbenthic21

                        ReservoirRB.Checked = True

                    Case StandardParameters.CustomVVWM, StandardParameters.ConstVolFlo, StandardParameters.ConstVolNoFlo

                        scenario1Custom = scenario1
                        scenario4Custom = scenario4
                        scenario21Custom = scenario21
                        scenario60Custom = scenario60
                        scenario90Custom = scenario90
                        scenario365Custom = scenario365
                        scenarioFullCustom = scenarioFull
                        scenarioBenthic1Custom = scenarioBenthic1
                        scenariobenthic21Custom = scenariobenthic21

                        eec1Custom = eec1
                        eec4Custom = eec4
                        eec21Custom = eec21
                        eec60Custom = eec60
                        eec90Custom = eec90
                        eec365Custom = eec365
                        eecFullCustom = eecFull
                        eecBenthic1Custom = eecBenthic1
                        eecbenthic21Custom = eecbenthic21

                        CustomRB.Checked = True

                End Select




            End If

        Else  'Do a single scenario run  '**************************************************************************************
            '**************************************************************************************



            checkvaluesMessage = checkvalues()
            If checkvaluesMessage <> "" Then 'Signifies problem with input.
                MsgBox(checkvaluesMessage)
                ThereIsProblem = True
                Return
            End If

            checkvaluesMessage = checkWaterBodyType(WaterBodyType)
            If checkvaluesMessage <> "" Then 'Signifies problem with input.
                MsgBox(checkvaluesMessage)
                ThereIsProblem = True
                Return
            End If

            '************************************************

            RunPrograms(WaterBodyType, 0, runflag, ErrorMessage)  'HERE IS WHERE THE PRZM & VVWM ARE STARTED


            If runflag Then
                '    MsgBox(ErrorMessage)
                ThereIsProblem = True
                Return
            End If

            If WaterBodyType = StandardParameters.EPAGroundWater Then

                GroundWaterRoutines.CaptureGWOutput(nchem, FileNameClass.ZtsFile, numHoriz.Text, GWpeakConc, GWsimAvgConc, GWthroughputs, GWpostBTavg, GWBreakThroughdays)

                DisplayGW_Results(nchem, GWpeakConc, GWsimAvgConc, GWthroughputs, GWpostBTavg, GWBreakThroughdays)
                GroundWaterRoutines.MakeGwGraph(nchem, FileNameClass.ZtsFile, GWChart)


            ElseIf WaterBodyType = StandardParameters.NoWaterBody Then
                'dont do anything

            Else
                'Need a way to postprocess and distiguish between the 3 water bodies, previously it was done through the tags on the charts
                ' but that was awkward and not general enough to use with the file naming consolidation routines implemented with this revision
                ' Hence these definitions:
                Select Case WaterBodyType
                    Case StandardParameters.EpaPond
                        FileNameClass.pondParentFile = FileNameClass.VVWMoutputFileParent
                        FileNameClass.pondDeg1File = FileNameClass.VVWMoutputFileDeg1
                        FileNameClass.pondDeg2File = FileNameClass.VVWMoutputFileDeg2

                    Case StandardParameters.EpaReservoir
                        FileNameClass.reservoirParentFile = FileNameClass.VVWMoutputFileParent
                        FileNameClass.reservoirDeg1File = FileNameClass.VVWMoutputFileDeg1
                        FileNameClass.reservoirDeg2File = FileNameClass.VVWMoutputFileDeg2

                    Case StandardParameters.CustomVVWM, StandardParameters.ConstVolFlo, StandardParameters.ConstVolNoFlo

                        FileNameClass.customParentFile = FileNameClass.VVWMoutputFileParent
                        FileNameClass.customDeg1File = FileNameClass.VVWMoutputFileDeg1
                        FileNameClass.customDeg2File = FileNameClass.VVWMoutputFileDeg2
                End Select

                DisplayGraphs(WaterBodyType)

            End If

        End If


    End Sub


    Public Sub DisplayGW_Results(ByVal nchem As Integer, ByVal GWpeakConc As Single(), ByVal GWsimAvgConc() As Single, ByVal GWThroughputs() As Single,
                            ByVal GWpostBTavg() As Single, ByVal GWBreakThroughdays() As Single)

        If Deg1CheckBox.Checked Then
            DaughterPanel.Visible = True
        Else
            DaughterPanel.Visible = False
        End If

        If Deg2CheckBox.Checked Then
            GranddaughterPanel.Visible = True
        Else
            GranddaughterPanel.Visible = False
        End If

        PeakGW_parent.Text = GWpeakConc(1)
        PeakGW_deg1.Text = GWpeakConc(2)
        PeakGW_deg2.Text = GWpeakConc(3)

        BreakthroughDays_parent.Text = GWBreakThroughdays(1)
        BreakthroughDays_deg1.Text = GWBreakThroughdays(2)
        BreakthroughDays_deg2.Text = GWBreakThroughdays(3)

        Throughputs_parent.Text = GWThroughputs(1)
        Throughputs_deg1.Text = GWThroughputs(2)
        Throughputs_deg2.Text = GWThroughputs(3)

        PostBTAvg_parent.Text = GWpostBTavg(1)
        PostBTAvg_deg1.Text = GWpostBTavg(2)
        PostBTAvg_deg2.Text = GWpostBTavg(3)

        SimAvg_parent.Text = GWsimAvgConc(1)
        SimAvg_deg1.Text = GWsimAvgConc(2)
        SimAvg_deg2.Text = GWsimAvgConc(3)
    End Sub



    Sub GetResultsFromOutputFiles(ByVal filename As String, ByVal LineID As String, ByRef OutputString As String)
        'Modified version of the previous batch run grabbing routine

        'Creates the output strings and the output file names for parent and degradates for each scenario
        'finalmsg, finalmsg1, finalmsg2 are returned and have the summary scenario results in them
        ' Concatenates each batch run results
        Dim msg As String = ""


        GetSummaryResultsFromVvwmOutput(filename, msg)  'EECs are summarized in msg
        OutputString = String.Format("{0, -25}", LineID) & msg & vbNewLine

    End Sub



    Private Sub GW_IndividualScenarioOutput(ByVal selectedScenario As String, ByRef finalmsg As String, ByRef finalmsgDeg1 As String, ByRef finalmsgDeg2 As String)
        'Creates the output strings and the output file names for parent and degradates for each scenario
        'finalmsg, finalmsg1, finalmsg2 are returned and have the summary scenario results in them
        ' Concatenates each batch run results

        Dim msg As String = ""

        finalmsg = finalmsg & String.Format("{2,-25}{1}{3,10}{1}{4,10}{1}{5,10}{1}{6,10}{1}{7,10}{0}", vbNewLine, vbTab, selectedScenario, PeakGW_parent.Text, BreakthroughDays_parent.Text, Throughputs_parent.Text, PostBTAvg_parent.Text, SimAvg_parent.Text)

        If Deg1CheckBox.Checked Then
            finalmsgDeg1 = finalmsgDeg1 & String.Format("{2,-25}{1}{3,10}{1}{4,10}{1}{5,10}{1}{6,10}{1}{7,10}{0}", vbNewLine, vbTab, selectedScenario, PeakGW_deg1.Text, BreakthroughDays_deg1.Text, Throughputs_deg1.Text, PostBTAvg_deg1.Text, SimAvg_deg1.Text)
        End If

        If Deg2CheckBox.Checked Then
            finalmsgDeg2 = finalmsgDeg2 & String.Format("{2,-25}{1}{3,10}{1}{4,10}{1}{5,10}{1}{6,10}{1}{7,10}{0}", vbNewLine, vbTab, selectedScenario, PeakGW_deg2.Text, BreakthroughDays_deg2.Text, Throughputs_deg2.Text, PostBTAvg_deg2.Text, SimAvg_deg2.Text)

        End If


    End Sub

    Public Sub GW_IndividualScenarioOutput_unappended(ByVal selectedScenario As String, ByRef finalmsg As String, ByRef finalmsgDeg1 As String, ByRef finalmsgDeg2 As String)
        'Creates the output strings and the output file names for parent and degradates for each scenario
        'finalmsg, finalmsg1, finalmsg2 are returned and have the summary scenario results in them
        'Results are NOT concatenated

        Dim msg As String = ""


        finalmsg = String.Format("{2,-25}{1}{3,10}{1}{4,10}{1}{5,10}{1}{6,10}{1}{7,10}{0}", vbNewLine, vbTab, selectedScenario, PeakGW_parent.Text, BreakthroughDays_parent.Text, Throughputs_parent.Text, PostBTAvg_parent.Text, SimAvg_parent.Text)

        If Deg1CheckBox.Checked Then
            finalmsgDeg1 = String.Format("{2,-25}{1}{3,10}{1}{4,10}{1}{5,10}{1}{6,10}{1}{7,10}{0}", vbNewLine, vbTab, selectedScenario, PeakGW_deg1.Text, BreakthroughDays_deg1.Text, Throughputs_deg1.Text, PostBTAvg_deg1.Text, SimAvg_deg1.Text)
        End If

        If Deg2CheckBox.Checked Then
            finalmsgDeg2 = String.Format("{2,-25}{1}{3,10}{1}{4,10}{1}{5,10}{1}{6,10}{1}{7,10}{0}", vbNewLine, vbTab, selectedScenario, PeakGW_deg2.Text, BreakthroughDays_deg2.Text, Throughputs_deg2.Text, PostBTAvg_deg2.Text, SimAvg_deg2.Text)
        End If


    End Sub




    Private Sub IndividualScenarioOutput(ByVal selectedScenario As String, ByRef finalmsg As String, ByRef finalmsgDeg1 As String, ByRef finalmsgDeg2 As String)
        'Creates the output strings and the output file names for parent and degradates for each scenario
        'finalmsg, finalmsg1, finalmsg2 are returned and have the summary scenario results in them
        ' Concatenates each batch run results
        Dim filename As String
        Dim msg As String = ""

        filename = FileNameClass.VVWMoutputFileParent

        'filename = workingDirectory.Text & Trim(ioFamilyNameBox.Text) & "_" & Trim(scenarioID.Text) & "_" & StandardParameters.WaterBodyName(WaterBodyType) & "_Parent.txt"

        GetSummaryResultsFromVvwmOutput(filename, msg)  'EECs are summarized in msg
        finalmsg = finalmsg & String.Format("{0, -25}", selectedScenario) & msg & vbNewLine

        If Deg1CheckBox.Checked Then
            filename = FileNameClass.VVWMoutputFileDeg1
            'filename = workingDirectory.Text & "/" & Trim(ioFamilyNameBox.Text) & "_" & Trim(scenarioID.Text) & "_" & StandardParameters.WaterBodyName(WaterBodyType) & "_Degradate1.txt"
            GetSummaryResultsFromVvwmOutput(filename, msg)  'EECs are summarized in msg
            finalmsgDeg1 = finalmsgDeg1 & String.Format("{0, -25}", selectedScenario) & msg & vbNewLine
        End If

        If Deg2CheckBox.Checked Then
            filename = FileNameClass.VVWMoutputFileDeg2
            'filename = workingDirectory.Text & "/" & Trim(ioFamilyNameBox.Text) & "_" & Trim(scenarioID.Text) & "_" & StandardParameters.WaterBodyName(WaterBodyType) & "_Degradate2.txt"
            GetSummaryResultsFromVvwmOutput(filename, msg)  'EECs are summarized in msg
            finalmsgDeg2 = finalmsgDeg2 & String.Format("{0, -25}", selectedScenario) & msg & vbNewLine
        End If


    End Sub

    Private Sub GetOutputFromVvwm(ByVal filename As String, ByRef avg1 As Single, ByRef avg4 As Single, ByRef avg21 As Single, ByRef avg60 As Single, ByRef avg90 As Single, ByRef avg365 As Single,
                                  ByRef avgTotal As Single, ByRef benthic1 As Single, ByRef benthic21 As Single)

        'YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY

        'this opens up the file (filename) and puts the critical concentrations 




        Dim currentrow As String()


        Using reader As New Microsoft.VisualBasic.FileIO.TextFieldParser(filename)
            reader.TextFieldType = Microsoft.VisualBasic.FileIO.FieldType.FixedWidth

            'skip Header Lines
            For i As Integer = 1 To 5
                reader.ReadLine()
            Next

            reader.SetFieldWidths(21, 16, -1)
            currentrow = reader.ReadFields()
            'peak = currentrow(1)

            currentrow = reader.ReadFields()
            avg365 = currentrow(1)  'i in 10 year yearly average

            currentrow = reader.ReadFields()
            avgTotal = currentrow(1)   'overall average

            currentrow = reader.ReadFields()
            avg4 = currentrow(1)

            currentrow = reader.ReadFields()
            avg21 = currentrow(1)


            currentrow = reader.ReadFields()
            avg60 = currentrow(1)

            currentrow = reader.ReadFields()
            avg90 = currentrow(1)

            currentrow = reader.ReadFields()
            avg1 = currentrow(1)

            reader.SetFieldWidths(40, 16, -1)
            currentrow = reader.ReadFields()
            benthic1 = currentrow(1)

            currentrow = reader.ReadFields()
            benthic21 = currentrow(1)

            'msg = String.Format("{0}{1}{0}{2}{0}{3}{0}{4}{0}{5}{0}{6}{0}{7}{0}{8}{0}{9}{0}{10}", vbTab, peak, chronic1, chronic, simAvg, chronic4, chronic21, chronic60, chronic90, poreWaterPeak, poreWater21)
        End Using
    End Sub








    Private Sub GetSummaryResultsFromVvwmOutput(ByVal filename As String, ByRef msg As String)
        'ONLY USED FOR ESA BATCH RETRIEVAL

        'this opens up the file (filename) and puts the critical concentrations into the string msg
        Dim peak As Single
        Dim chronic As Single

        Dim chronic1 As Single
        Dim chronic21 As Single
        Dim chronic60 As Single
        Dim chronic90 As Single
        Dim simAvg As Single
        Dim chronic4 As Single
        Dim poreWaterPeak As Single
        Dim poreWater21 As Single


        Dim currentrow As String()


        Using reader As New Microsoft.VisualBasic.FileIO.TextFieldParser(filename)
            reader.TextFieldType = Microsoft.VisualBasic.FileIO.FieldType.FixedWidth

            'skip Header Lines
            For i As Integer = 1 To 5
                reader.ReadLine()
            Next

            reader.SetFieldWidths(21, 16, -1)
            currentrow = reader.ReadFields()
            peak = currentrow(1)

            currentrow = reader.ReadFields()
            chronic = currentrow(1)  'i in 10 year yearly average

            currentrow = reader.ReadFields()
            simAvg = currentrow(1)   'overall average

            currentrow = reader.ReadFields()
            chronic4 = currentrow(1)

            currentrow = reader.ReadFields()
            chronic21 = currentrow(1)


            currentrow = reader.ReadFields()
            chronic60 = currentrow(1)

            currentrow = reader.ReadFields()
            chronic90 = currentrow(1)

            currentrow = reader.ReadFields()
            chronic1 = currentrow(1)

            reader.SetFieldWidths(40, 16, -1)
            currentrow = reader.ReadFields()
            poreWaterPeak = currentrow(1)

            currentrow = reader.ReadFields()
            poreWater21 = currentrow(1)

            msg = String.Format("{0}{1}{0}{2}{0}{3}{0}{4}{0}{5}{0}{6}{0}{7}{0}{8}{0}{9}{0}{10}", vbTab, peak, chronic1, chronic, simAvg, chronic4, chronic21, chronic60, chronic90, poreWaterPeak, poreWater21)
        End Using
    End Sub

    Private Sub WriteBatchOutputSummary(ByVal filename As String, ByVal finalmsg As String, ByVal finalmsgDeg1 As String, ByVal finalmsgDeg2 As String)
        'Parent and degradate info sent in for entire Batch-Ran simulations and a summary file is created

        Dim Header As String

        Dim headerTitle As String = String.Format("Batch Run ID             {0}Peak{0}1-day{0}Yr{0}overall{0}4-day{0}21-day{0}60-day{0}90-day{0}PW_pk{0}PW_21", vbTab)

        Header = "**** Parent *****************************************************************************************"
        Header = Header & vbNewLine & headerTitle
        finalmsg = Header & vbNewLine & finalmsg

        If Deg1CheckBox.Checked Then
            Header = "**** Degradate 1 ************************************************************************************"
            Header = Header & vbNewLine & headerTitle
            finalmsg = finalmsg & vbNewLine & Header
            finalmsg = finalmsg & vbNewLine & finalmsgDeg1
        End If
        If Deg2CheckBox.Checked Then
            Header = "**** Degradate 2 ************************************************************************************"
            Header = Header & vbNewLine & headerTitle
            finalmsg = finalmsg & vbNewLine & Header
            finalmsg = finalmsg & vbNewLine & finalmsgDeg2
        End If



        Try
            My.Computer.FileSystem.WriteAllText(filename, finalmsg, False, System.Text.Encoding.ASCII)
        Catch ex As Exception
            MsgBox(ex.Message)
        End Try

    End Sub


    Private Sub WriteBatchOutputSummaryForGroundwater(ByVal filename As String, ByVal finalmsg As String, ByVal finalmsgDeg1 As String, ByVal finalmsgDeg2 As String)
        'Parent and degradate info sent in for entire Batch-Ran simulations and a summary file is created

        Dim Header As String

        Dim headerTitle As String = String.Format("{1,-25}{0}{2,10}{0}{3,10}{0}{4,10}{0}{5,10}{0}{6,10}", vbTab, "GW Run ID", "Peak", "Breakthru", "Thruput", "PostBT Avg", "Sim Avg")

        Header = "**** Parent *****************************************************************************************"
        Header = Header & vbNewLine & headerTitle
        finalmsg = Header & vbNewLine & finalmsg

        If Deg1CheckBox.Checked Then
            Header = "**** Degradate 1 ************************************************************************************"
            Header = Header & vbNewLine & headerTitle
            finalmsg = finalmsg & vbNewLine & Header
            finalmsg = finalmsg & vbNewLine & finalmsgDeg1
        End If
        If Deg2CheckBox.Checked Then
            Header = "**** Degradate 2 ************************************************************************************"
            Header = Header & vbNewLine & headerTitle
            finalmsg = finalmsg & vbNewLine & Header
            finalmsg = finalmsg & vbNewLine & finalmsgDeg2
        End If



        Try
            My.Computer.FileSystem.WriteAllText(filename, finalmsg, False, System.Text.Encoding.ASCII)
        Catch ex As Exception
            MsgBox(ex.Message)
        End Try

    End Sub

    Private Sub CreatePrzm5String(ByVal dayAdd As Integer, ByVal WaterBodyType As String, ByRef isError As Boolean, ByRef ErrorMessage As String)
        Dim msg As String
        Dim firstday As Integer
        Dim firstmon As Integer
        Dim firstyear As Integer
        Dim lastyear As Integer

        Dim nchem As Integer
        isError = False

        FileNameClass.ZtsFile = workingDirectoryLabel.Text & ioFamilyNameBox.Text & ".zts"

        nchem = 1
        If Deg1CheckBox.Checked Then nchem = 2
        If Deg2CheckBox.Checked Then nchem = 3
        msg = String.Format("***  {0}", Date.Now)
        msg = msg & String.Format("{0}***  PRZM5 Input File Generator", vbNewLine)
        msg = msg & String.Format("{0}***Record A1: Weather File", vbNewLine)

        Dim fullweatherfile As String
        If UseWeatherDirecory.Checked Then
            fullweatherfile = WeatherDirectoryBox.Text & weatherBox.Text
        Else
            fullweatherfile = weatherBox.Text

        End If



        msg = msg & String.Format("{0}{1}", vbNewLine, fullweatherfile)

        msg = msg & String.Format("{0}***Record A2: PRZM5 Time Series Output File", vbNewLine)
        msg = msg & String.Format("{0}{1}", vbNewLine, FileNameClass.ZtsFile)


        msg = msg & String.Format("{0}***Record A3: PRZM5 Advanced Options", vbNewLine)
        msg = msg & String.Format("{0}{1},{2},{3},{4},{5},{6},{7}", vbNewLine, UseFreundlich.Checked, AdustCN.Checked, Test3new.Checked, UseNonequilibrium.Checked, ReadCalibrationData.Checked, SubTimeStepBox.Text, TrueRainfallDistribution.Checked)

        If ReadCalibrationData.Checked Then

            FileNameClass.PrZMCalibrationInputFile = calibrationFileBox.Text

            Dim filecount As Integer

            filecount = FileNameClass.PrZMCalibrationInputFile.Length
            FileNameClass.PrZMCalibrationOutputFile = FileNameClass.PrZMCalibrationInputFile.Remove(filecount - 4) & "_CalibrateOut.txt"

            msg = msg & String.Format("{0}{1}", vbNewLine, FileNameClass.PrZMCalibrationInputFile)
            msg = msg & String.Format("{0}{1}", vbNewLine, FileNameClass.PrZMCalibrationOutputFile)

        End If

        msg = msg & String.Format("{0}***Record 1: pfac, sfac, anetd ", vbCrLf)
        msg = msg & String.Format("{0}{1}, {2}, {3}", vbCrLf, pfac.Text, snowmelt.Text, evapDepth.Text)

        msg = msg & String.Format("{0}***Record 2: Erosion Flag 4 = MUSS, 3= MUST, 1= MUSLE ", vbNewLine)
        msg = msg & String.Format("{0}{1}", vbNewLine, 4)


        '**********Not sure why this section is placed here, Maybe should be moved down to where it is used********************
        Dim FieldSize As Single
        Dim hydlength As Single
        Dim appsArraySize As Integer
        Dim FractionCroppedArea As Single


        appsArraySize = Convert.ToInt16(appNumber.Text) - 1




        Dim localSpray(appsArraySize) As Single
        Dim localEff(appsArraySize) As Single

        Select Case WaterBodyType

            Case StandardParameters.EpaReservoir
                FieldSize = StandardParameters.reservoirField / 10000.0
                hydlength = StandardParameters.reservoirHL
                FractionCroppedArea = ReservoirCroppedAreaBox.Text

                For i As Integer = 0 To appsArraySize
                    localEff(i) = ApplicationInfo.Effr(i).Text
                    localSpray(i) = ApplicationInfo.sprayr(i).Text()
                Next

            Case StandardParameters.EpaPond
                FieldSize = StandardParameters.pondField / 10000.0
                hydlength = StandardParameters.pondHL
                FractionCroppedArea = StandardParameters.PondCroppedAreaFraction

                For i As Integer = 0 To appsArraySize
                    localEff(i) = ApplicationInfo.Effp(i).Text
                    localSpray(i) = ApplicationInfo.sprayp(i).Text()
                Next
            Case StandardParameters.EPAGroundWater
                FieldSize = 1.0
                hydlength = 1.0
                FractionCroppedArea = 1.0

                For i As Integer = 0 To appsArraySize
                    localEff(i) = 1.0
                    localSpray(i) = 0.0
                Next

            Case StandardParameters.NoWaterBody
                FieldSize = fieldAreaBox.Text / 10000.0
                hydlength = hl.Text
                FractionCroppedArea = 1.0
                For i As Integer = 0 To appsArraySize
                    localEff(i) = 1.0
                    localSpray(i) = 0.0
                Next

            Case Else  ' custom water bodies
                FieldSize = fieldAreaBox.Text / 10000.0
                hydlength = hl.Text
                FractionCroppedArea = CustomCroppedAreaBox.Text
                For i As Integer = 0 To appsArraySize
                    localEff(i) = ApplicationInfo.Eff(i).Text
                    localSpray(i) = ApplicationInfo.spray(i).Text()
                Next

        End Select
        '****************************************************************************************************************************************************


        msg = msg & String.Format("{0}***Record 3: uslek, uslels, uslep, FieldSize, ireg, slope, hydraul length", vbNewLine)
        msg = msg & String.Format("{0}{1}, {2}, {3}, {4}, {5}, {6}, {7}", vbNewLine, uslek.Text, uslels.Text, uslep.Text, FieldSize, ireg.Text, slope.Text, hydlength)

        ''changed canbopy coverage in prazm to fraction
        'msg = msg & String.Format("{0}***Record 5: Crop ID, canopyHoldup, rootDepth, canopyCover, WFMAX (optional), canopyHeight", vbNewLine)
        'msg = msg & String.Format("{0}{1}, {2}, {3}, {4}, {5}, {6}", vbNewLine, 1, Convert.ToSingle(canopyHoldup.Text) / 100, rootDepth.Text, canopyCover.Text, 0, canopyHeight.Text)

        Dim useYears As Integer
        If useUSLEYear.Checked Then
            useYears = 1
        Else
            useYears = 0
        End If

        msg = msg & String.Format("{0}***Record 4 Number of hydro-event changes", vbNewLine)
        msg = msg & String.Format("{0}{1}, {2}", vbNewLine, NumberOfFactors.Text, useYears)

        msg = msg & String.Format("{0}***Record 5 Day, Month, Year, C, n, CN", vbNewLine)
        For i As Integer = 0 To NumberOfFactors.Text - 1
            msg = msg & String.Format("{0}{1,2},{2,2},{3,4},{4}, {5}, {6}", vbNewLine, USLE.day(i).Text, USLE.mon(i).Text, USLE.year(i).Text, USLE.C(i).Text, USLE.n(i).Text, USLE.cn(i).Text)
        Next







        Dim weatherFileExtension As String
        weatherFileExtension = System.IO.Path.GetExtension(fullweatherfile)

        weatherFileExtension = weatherFileExtension.ToUpper()


        Select Case weatherFileExtension
            Case ".WEA"

                '****** Scan the weather file and determine the first and last years *********
                Using reader As New Microsoft.VisualBasic.FileIO.TextFieldParser(fullweatherfile)
                    reader.TextFieldType = Microsoft.VisualBasic.FileIO.FieldType.Delimited
                    reader.SetDelimiters(",")
                    Dim allfields As String()

                    allfields = reader.ReadFields
                    firstmon = allfields(0)
                    firstday = allfields(1)
                    firstyear = allfields(2)

                    ApplicationInfo.startdate = DateAndTime.DateSerial(firstyear, firstmon, firstday)

                    While Not reader.EndOfData
                        Try
                            lastyear = reader.ReadFields(2)
                        Catch ex As Exception
                            ErrorMessage = ("There is a problem reading the weather file. " & ex.Message)
                            isError = True
                            Return
                        End Try
                    End While
                End Using

            Case ".DVF"


                '**************************OLD DVF FILE with 2 digit years*************************
                '****** Scan the weather file and determine the first and last years *********
                Using reader As New Microsoft.VisualBasic.FileIO.TextFieldParser(fullweatherfile)
                    reader.TextFieldType = Microsoft.VisualBasic.FileIO.FieldType.FixedWidth
                    reader.SetFieldWidths(1, 2, 2, 2)
                    Dim allfields As String()


                    allfields = reader.ReadFields
                    firstmon = allfields(1)
                    firstday = allfields(2)
                    firstyear = allfields(3) + 1900

                    ApplicationInfo.startdate = DateAndTime.DateSerial(firstyear, firstmon, firstday)

                    While Not reader.EndOfData
                        Try
                            lastyear = reader.ReadFields(3) + 1900
                        Catch ex As Exception
                            ErrorMessage = ("There is a problem reading the weather file. " & ex.Message)
                            isError = True
                            Return
                        End Try
                    End While
                End Using
                '****************************************************************************************************


            Case ".MET"
                '**************************OLD DVF FILE with 2 digit years*************************
                '****** Scan the weather file and determine the first and last years *********
                Using reader As New Microsoft.VisualBasic.FileIO.TextFieldParser(fullweatherfile)
                    reader.TextFieldType = Microsoft.VisualBasic.FileIO.FieldType.FixedWidth
                    reader.SetFieldWidths(1, 2, 2, 2)
                    Dim allfields As String()

                    allfields = reader.ReadFields
                    firstmon = allfields(1)
                    firstday = allfields(2)
                    firstyear = allfields(3) + 1900

                    ApplicationInfo.startdate = DateAndTime.DateSerial(firstyear, firstmon, firstday)

                    While Not reader.EndOfData
                        Try
                            lastyear = reader.ReadFields(3)
                        Catch ex As Exception
                            ErrorMessage = ("There is a problem reading the weather file. " & ex.Message)
                            isError = True
                            Return
                        End Try
                    End While


                    'per Ian Kennedy
                    lastyear = lastyear + 1900
                    If lastyear <= firstyear Then
                        lastyear = lastyear + 100  ' this is needed fore Canadian weather that go ubtil 2003
                    End If

                End Using
                '****************************************************************************************************

            Case Else
                ErrorMessage = ("Unknown weather file extension type")
                isError = True
                Return
        End Select




        'NEW ROUTINEs FOR CROPS
        '*******************************************************************************************
        Dim lagyear As Integer
        Dim skipyear As Integer
        Dim addYearM As Integer

        Dim totalyears As Integer
        Dim numCropCyclesPerYear As Integer
        Dim numTotalCropCycles As Integer

        Dim postHarvestDisposition As Integer

        totalyears = lastyear - firstyear + 1


        If evergreen.Checked Then   '******* EVERGREEN **************************************************
            '********************************************************************************************
            msg = msg & String.Format("{0}***Record 6: Number of crop periods that follow{0}{1}", vbNewLine, 1)
            msg = msg & String.Format("{0}***Record 7: Emergence (d/m/y), Maturity (d/m/y), Harvest (d/m/y), depth (cm), cover, height(cm), holdup (cm), post-harvest disposition", vbNewLine)
            '*************************************************************************

            msg = msg & String.Format("{0}{1,2},{2,2},{3,2},{4,4},{5,2},{6,2},{7,4},{8,2},{9,2},{10,8}, {11,8}, {12,8}, {13,8}, {14,8}",
                                    vbNewLine, 0, 0, 0, 0, 0, 0, 9, 9, 9999, altRootDepth.Text, Convert.ToSingle(altCanopyCover.Text) / 100.0, altCanopyHeight.Text, altCanopyHoldup.Text, 1)

            '**********************************************************************************************
        ElseIf lessThanAnnualGrowth.Checked Then  '*****  Annual Cycle of crops****************************
            '**********************************************************************************************
            numCropCyclesPerYear = Convert.ToInt32(CropCyclesPerYear.Text)

            numTotalCropCycles = 0
            For i = 0 To numCropCyclesPerYear - 1
                skipyear = CropProperties.plantingFrequency(i).Text
                lagyear = CropProperties.plantingLag(i).Text
                numTotalCropCycles = numTotalCropCycles + Int((totalyears - lagyear - 1) / skipyear) + 1
            Next

            msg = msg & String.Format("{0}***Record 6: Number of crop periods that follow{0}{1}", vbNewLine, numTotalCropCycles)
            msg = msg & String.Format("{0}***Record 7: Emergence (d/m/y), Maturity (d/m/y), Harvest (d/m/y), depth (cm), cover, height(cm), holdup (cm), post-harvest disposition", vbNewLine)
            '*************************************************************************

            Dim numberOfCropCycles As Integer
            numberOfCropCycles = CropCyclesPerYear.Text

            For j As Integer = 0 To numberOfCropCycles - 1

                addYearM = 0
                'Check the crop dates to see if they cross over a calendar year.
                If Convert.ToInt16(CropProperties.emergenceMonth(j).Text) < Convert.ToInt16(CropProperties.maturityMonth(j).Text) Then
                    addYearM = 0
                ElseIf Convert.ToInt16(CropProperties.emergenceMonth(j).Text) > Convert.ToInt16(CropProperties.maturityMonth(j).Text) Then
                    addYearM = 1

                    'Make sure Harvest  date does not exceed 1 year
                    If Convert.ToInt16(CropProperties.harvestMonth(j).Text) > Convert.ToInt16(CropProperties.emergenceMonth(j).Text) Then
                        ErrorMessage = ("Crop cycle exceeds 365 days. This is not workable in PRZM. Cycle = " & j + 1)
                        isError = True
                        Return
                        'ElseIf Convert.ToInt16(monthHarvest1.Text) = Convert.ToInt16(monthEmerge1.Text) Then
                    ElseIf Convert.ToInt16(CropProperties.harvestMonth(j).Text) = Convert.ToInt16(CropProperties.emergenceMonth(j).Text) Then
                        If Convert.ToInt16(CropProperties.harvestDay(j).Text) >= Convert.ToInt16(CropProperties.emergenceDay(j).Text) Then
                            ErrorMessage = ("Crop cycle exceeds 365 days. This is not workable in PRZM. Cycle = " & j + 1)
                            isError = True
                            Return
                        End If
                    End If

                ElseIf Convert.ToInt16(CropProperties.emergenceMonth(j).Text) = Convert.ToInt16(CropProperties.maturityMonth(j).Text) Then
                    If Convert.ToInt16(CropProperties.emergenceDay(j).Text) < Convert.ToInt16(CropProperties.maturityDay(j).Text) Then
                        addYearM = 0
                    ElseIf Convert.ToInt16(CropProperties.emergenceDay(j).Text) > Convert.ToInt16(CropProperties.maturityDay(j).Text) Then
                        addYearM = 1
                        'Make sure Harvest  date does not exceed 1 year
                        If Convert.ToInt16(CropProperties.harvestMonth(j).Text) > Convert.ToInt16(CropProperties.emergenceMonth(j).Text) Then
                            ErrorMessage = ("Crop cycle exceeds 365 days. This is not workable in PRZM. Cycle = " & j + 1)
                            isError = True
                            Return
                        ElseIf Convert.ToInt16(CropProperties.harvestMonth(j).Text) = Convert.ToInt16(CropProperties.emergenceMonth(j).Text) Then
                            If Convert.ToInt16(CropProperties.harvestDay(j).Text) >= Convert.ToInt16(CropProperties.emergenceDay(j).Text) Then
                                ErrorMessage = ("Crop cycle exceeds 365 days. This is not workable in PRZM. Cycle = " & j + 1)
                                isError = True
                                Return
                            End If
                        End If
                    Else
                        ErrorMessage = ("Maturity and Emergence dates can not occur on the same day. Cycle = " & j + 1)
                        isError = True
                        Return
                    End If
                End If


                Dim addYearH As Integer
                addYearH = 0
                'Check the crop dates to see if they cross over a calendar year.
                If Convert.ToInt16(CropProperties.maturityMonth(j).Text) < Convert.ToInt16(CropProperties.harvestMonth(j).Text) Then
                    addYearH = 0
                ElseIf Convert.ToInt16(CropProperties.maturityMonth(j).Text) > Convert.ToInt16(CropProperties.harvestMonth(j).Text) Then

                    addYearH = 1
                    If Convert.ToInt16(CropProperties.harvestMonth(j).Text) > Convert.ToInt16(CropProperties.emergenceMonth(j).Text) Then
                        ErrorMessage = ("Crop cycle exceeds 365 days. This is not workable in PRZM. Cycle = " & j + 1)
                        isError = True
                        Return
                    ElseIf Convert.ToInt16(CropProperties.harvestMonth(j).Text) = Convert.ToInt16(CropProperties.emergenceMonth(j).Text) Then
                        If Convert.ToInt16(CropProperties.harvestDay(j).Text) >= Convert.ToInt16(CropProperties.emergenceDay(j).Text) Then
                            ErrorMessage = ("Crop cycle exceeds 365 days. This is not workable in PRZM. Cycle = " & j + 1)
                            isError = True
                            Return
                        End If
                    End If

                ElseIf Convert.ToInt16(CropProperties.maturityMonth(j).Text) = Convert.ToInt16(CropProperties.harvestMonth(j).Text) Then
                    If Convert.ToInt16(CropProperties.maturityDay(j).Text) < Convert.ToInt16(CropProperties.harvestDay(j).Text) Then
                        addYearH = 0
                    ElseIf Convert.ToInt16(CropProperties.maturityDay(j).Text) > Convert.ToInt16(CropProperties.harvestDay(j).Text) Then
                        addYearH = 1
                        If Convert.ToInt16(CropProperties.harvestMonth(j).Text) > Convert.ToInt16(CropProperties.emergenceMonth(j).Text) Then

                            ErrorMessage = ("Crop cycle exceeds 365 days. This is not workable in PRZM. Cycle = " & j + 1)
                            isError = True
                            Return
                        ElseIf Convert.ToInt16(CropProperties.harvestMonth(j).Text) = Convert.ToInt16(CropProperties.emergenceMonth(j).Text) Then
                            If Convert.ToInt16(CropProperties.harvestDay(j).Text) >= Convert.ToInt16(CropProperties.emergenceDay(j).Text) Then

                                ErrorMessage = ("Crop cycle exceeds 365 days. This is not workable in PRZM. Cycle = " & j + 1)
                                isError = True
                                Return
                            End If
                        End If
                    Else
                        ErrorMessage = ("Maturity and Harvest dates can not occur on the same day. Cycle = " & j + 1)
                        isError = True
                        Return
                    End If
                End If



                If addYearH + addYearM > 1 Then
                    ErrorMessage = ("Crop growth period exceeds 365 days. Cycle = " & j + 1)
                    isError = True
                    Return
                End If
                '*************************************************************************
                'RECORD 7
                skipyear = CropProperties.plantingFrequency(j).Text
                lagyear = CropProperties.plantingLag(j).Text



                If CropProperties.foliarDisposition(j * 3).Checked Then
                    postHarvestDisposition = 1
                ElseIf CropProperties.foliarDisposition(j * 3 + 1).Checked Then
                    postHarvestDisposition = 2
                Else
                    postHarvestDisposition = 3
                End If



                For i As Integer = firstyear + lagyear To lastyear Step skipyear
                    ' msg = msg & String.Format("{0}{1,2},{2,2},{3,2},{4,4},{5,2},{6,2},{7,4},{8,2},{9,2},{10,8}, {11,8}, {12,8}, {13,8}, {14,8}", vbNewLine, dayEmerge1.Text, monthEmerge1.Text, i, dayMature1.Text, monthMature1.Text, i + addYearM, dayHarvest1.Text, monthHarvest1.Text, i + addYearM + addYearH, rootDepth1.Text, Convert.ToSingle(canopyCover1.Text) / 100, canopyHeight1.Text, canopyHoldup1.Text, 1)
                    msg = msg & String.Format("{0}{1,2},{2,2},{3,2},{4,4},{5,2},{6,2},{7,4},{8,2},{9,2},{10,8}, {11,8}, {12,8}, {13,8}, {14,8}",
                                              vbNewLine, CropProperties.emergenceDay(j).Text, CropProperties.emergenceMonth(j).Text, i,
                                              CropProperties.maturityDay(j).Text, CropProperties.maturityMonth(j).Text,
                                              i + addYearM, CropProperties.harvestDay(j).Text, CropProperties.harvestMonth(j).Text,
                                              i + addYearM + addYearH, CropProperties.rootDepth(j).Text,
                                              Convert.ToSingle(CropProperties.canopyCover(j).Text) / 100.0, CropProperties.canopyHeight(j).Text,
                                              CropProperties.canopyHoldup(j).Text, postHarvestDisposition)
                Next
            Next


            '******************************************************************************************************************
        ElseIf greaterThanAnualGrowth.Checked Then  ' Cycle is Greater than 1 year
            '******************************************************************************************************************


            Dim emergenceDate As Date
            Dim maturityDate As Date
            Dim harvestDate As Date

            'Check the crop dates to see if they cross over a calendar year.

            'dummy calculation to determine how many years between harvest and emergence
            emergenceDate = DateSerial(1900, altEmergeMon.Text, altEmergeDay.Text)
            harvestDate = emergenceDate.AddDays(altDaysToHarvest.Text)
            skipyear = harvestDate.Year - emergenceDate.Year  'FOR CaSEs WHERE NEW EMERGENCE WILL OCCUR IN SAME CALENDAR YEAR AS PREVIOUS HARVEST



            emergenceDate = DateSerial(harvestDate.Year, altEmergeMon.Text, altEmergeDay.Text)

            If emergenceDate < harvestDate Then
                skipyear = skipyear + 1
            End If


            msg = msg & String.Format("{0}***Record 6: Number of crop periods that follow{0}{1}", vbNewLine, Int((totalyears - 1) / skipyear) + 1)
            msg = msg & String.Format("{0}***Record 7: Emergence (d/m/y), Maturity (d/m/y), Harvest (d/m/y), depth (cm), cover, height(cm), holdup (cm), post-harvest disposition", vbNewLine)



            For i As Integer = firstyear To lastyear Step skipyear

                emergenceDate = DateSerial(i, altEmergeMon.Text, altEmergeDay.Text)
                maturityDate = emergenceDate.AddDays(altDaysToMaturity.Text)
                harvestDate = emergenceDate.AddDays(altDaysToHarvest.Text)

                msg = msg & String.Format("{0}{1,2},{2,2},{3,2},{4,4},{5,2},{6,2},{7,4},{8,2},{9,2},{10,8}, {11,8}, {12,8}, {13,8}, {14,8}", vbNewLine,
                                             emergenceDate.Day, emergenceDate.Month, emergenceDate.Year,
                                             maturityDate.Day, maturityDate.Month, maturityDate.Year,
                                             harvestDate.Day, harvestDate.Month, harvestDate.Year,
                                             altRootDepth.Text, Convert.ToSingle(altCanopyCover.Text) / 100.0, altCanopyHeight.Text, altCanopyHoldup.Text, 1)
            Next




        End If  '***********************END of Crop Inputs  ******************************************




        msg = msg & String.Format("{0}***Record 8: irrflag, tempflag", vbNewLine)

        Dim irflag As Integer
        If noIrrigation.Checked Then
            irflag = 0
        Else
            irflag = 2
        End If

        'Dim tempflag As Integer
        'If simTemperature.Checked Then
        '    tempflag = 1
        'Else
        '    tempflag = 0
        'End If


        '                              ir,  T,  
        msg = msg & String.Format("{0} {1}, {2}", vbNewLine, irflag, simTemperature.Checked)   ' if temp flag is on thermcon is on also

        Dim irtype As Integer

        If irflag = 2 Then
            If overCanopy.Checked Then
                irtype = 3
            Else
                irtype = 4
            End If
            msg = msg & String.Format("{0}***Record 9", vbNewLine)
            'The last two items are NEW and for user defined irrigation depths in lieu of root depth "TRUE" indicates no user defined depth, value is the depth in cm.
            Dim irrigationDepth As Single
            If UserSpecifiesIrrigDepth.Checked Then
                irrigationDepth = IrrigationDepthUserSpec.Text
            Else
                irrigationDepth = 0.0
            End If


            msg = msg & String.Format("{0}{1}, {2}, {3}, {4}, {5}, {6}",
                                      vbNewLine, irtype, fleach.Text, depletion.Text,
                                      rateIrrig.Text, UserSpecifiesIrrigDepth.Checked, irrigationDepth)
        End If

        Dim albedo As Single
        Dim volatilizationThickness As Single

        If simTemperature.Checked Then
            albedo = albedoBox.Text
            volatilizationThickness = volatilizationBoundaryBox.Text
            msg = msg & String.Format("{0}***Record 10 ", vbNewLine)
            msg = msg & String.Format("{0}{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}, {2}", vbNewLine, albedo, 0.97) '0.97 is emmisivity

            msg = msg & String.Format("{0}***Record 11 ", vbNewLine)
            msg = msg & String.Format("{0}{1},{2}", vbNewLine, 10.0, volatilizationThickness)  '10 is the wind measurement height

            msg = msg & String.Format("{0}***Record 12 Lower Boundary Temperature for 12 months", vbNewLine)
            msg = msg & String.Format("{0}{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1}", vbNewLine, bcTemp.Text)

            msg = msg & String.Format("{0}***Record 13  Q10, Soil Ref Temperature", vbNewLine)
            msg = msg & String.Format("{0} 2.00,{2}", vbNewLine, Q10Box.Text, soilTempBox1.Text)
        End If


        'CREATE SOIL PROFILE RECORDS

        Dim soilHalfLifeBox(3) As TextBox
        Dim soilRate(3) As Single

        soilHalfLifeBox(1) = soilDegradation1
        soilHalfLifeBox(2) = soilDegradation2
        soilHalfLifeBox(3) = soilDegradation3

        For i As Integer = 1 To nchem
            If soilHalfLifeBox(i).Text = "" Then
                soilRate(i) = 0.0   'Blank means stable
            ElseIf soilHalfLifeBox(i).Text = 0 Then
                soilRate(i) = 0.0   'half life of 0 means stable
            Else
                soilRate(i) = 0.69314 / soilHalfLifeBox(i).Text
            End If
        Next

        msg = msg & String.Format("{0}***Record 14: Number of horizons", vbNewLine)
        msg = msg & String.Format("{0}{1}", vbNewLine, numHoriz.Text)



        Dim kd1 As Single
        Dim kd2 As Single
        Dim kd3 As Single

        msg = msg & String.Format("{0}*** Record 15", vbNewLine)
        msg = msg & vbNewLine & "*** #,thk, Del, Dsp,   bd,  W0,    FC,    WP,    oc, snd, cly,  tmp "
        For i As Integer = 0 To Convert.ToInt16(numHoriz.Text) - 1
            msg = msg & String.Format("{0}{1}, {2}, {3}, {4}, {5}, {6}, {6}, {7}, {8}, {9}, {10}, {11},", vbNewLine, i + 1, SoilProperty.thick(i).Text,
                 SoilProperty.compartment(i).Text, "0.0", SoilProperty.bulkden(i).Text, SoilProperty.maxcap(i).Text, SoilProperty.mincap(i).Text,
                 SoilProperty.oc(i).Text, SoilProperty.sand(i).Text, SoilProperty.clay(i).Text, bcTemp.Text)
        Next

        msg = msg & String.Format("{0}*** Record 16, New Runoff Extraction Parameters: rDepth, rDecline, Bypass", vbNewLine)
        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, rDepthBox.Text, rDeclineBox.Text, rBypassBox.Text)

        msg = msg & String.Format("{0}*** Record 17: New Erosion Extraction Parameters: eDepth, eDecline, eEfficiency", vbNewLine)
        msg = msg & String.Format("{0}{1},{2},{3}", vbNewLine, eDepthBox.Text, eDeclineBox.Text, eBypassBox.Text)

        msg = msg & String.Format("{0}************ START OF CHEMICAL INPUTS ***************************", vbNewLine)


        Dim startApp As Integer
        Dim endApp As Integer
        Dim k As Integer

        Dim appNumberConsideringMultiCrop As Integer

        If AppsAbsolute.Checked Then
            appNumberConsideringMultiCrop = Convert.ToInt16(appNumber.Text) - 1
        Else
            appNumberConsideringMultiCrop = Convert.ToInt16(appNumber.Text) * (numCropCyclesPerYear) - 1
        End If

        Dim appdate(appNumberConsideringMultiCrop) As Date


        'this creates a template of application dates for a generic year
        If AppsAbsolute.Checked Then
            For i As Integer = 0 To appNumber.Text - 1
                appdate(i) = DateSerial(1900, ApplicationInfo.mon(i).Text, ApplicationInfo.day(i).Text).AddDays(dayAdd)
            Next
        Else 'Relative applications
            k = 0
            For j As Integer = 0 To numCropCyclesPerYear - 1
                For i As Integer = 0 To appNumber.Text - 1
                    appdate(k) = RelativeDateConverter(ApplicationInfo.RelApp(i).Text, j).AddDays(dayAdd)
                    k = k + 1
                Next
            Next
        End If

        'Load pesticide application days


        For i As Integer = 0 To appNumberConsideringMultiCrop  'this is the array size of  the application date template for a single year
            ApplicationInfo.PestAppyDay(i) = appdate(i).Day  'store the app dates
            ApplicationInfo.PestAppyMon(i) = appdate(i).Month
        Next


        Dim TotalMassInput As Single
        Dim cam As Integer
        Dim depi As Single

        Dim tempmsg As String = " "

        ApplicationInfo.totalApplications = 0

        Dim tband_local As Single


        'Adjust Application rate by cropped fraction; this is the effective application rate used for direct ap as well as spraydrift

        Dim effectiveAppRate(appsArraySize) As Single

        For j As Integer = 0 To appsArraySize
            effectiveAppRate(j) = Convert.ToSingle(ApplicationInfo.rate(j).Text) * FractionCroppedArea
        Next


        'this is for use with multicrop schedules below
        Dim loopendvalue As Integer
        If AppsRelative.Checked Then
            loopendvalue = numCropCyclesPerYear - 1
        Else
            loopendvalue = 0
        End If


        If SpecifyYears.Checked Then

            k = 0  'counts the total applications 
            For m As Integer = 0 To loopendvalue
                For j As Integer = 0 To appsArraySize '(appNumber.text -1)
                    ApplicationInfo.GetApplicationMethodProperties(j, cam, depi, tband_local)

                    '                              d  m   yr  cam dep Rat eff spr cam=1  0    0   0     0   1     0   0    0    0
                    tempmsg = tempmsg & String.Format("{0}{1},{2},{3},{4},{5},{6},{7},{8}, 1, 4.0, 0, 0, 0, 1, 4.0, 0, 0, 0",
                          vbNewLine, ApplicationInfo.PestAppyDay(k), ApplicationInfo.PestAppyMon(k), ApplicationInfo.year(j).Text, cam, depi, effectiveAppRate(j), localEff(j), tband_local)
                    TotalMassInput = TotalMassInput + effectiveAppRate(j)

                    ApplicationInfo.applicationDates(k) = DateSerial(ApplicationInfo.year(j).Text, ApplicationInfo.PestAppyMon(j), ApplicationInfo.PestAppyDay(j))
                    ApplicationInfo.sprayTotalArray(k) = localSpray(j)
                    ApplicationInfo.rateTotalArray(k) = effectiveAppRate(j)

                    k = k + 1
                Next

            Next
            ApplicationInfo.totalApplications = Convert.ToInt16(appNumber.Text) * (loopendvalue + 1)


        Else  'Use normal repeating year applications
            Dim mm As Integer  'overall tracker for pest applications

            startApp = firstyear + firstAppYear.Text - 1
            If lastAppyear.Text = "last" Then
                endApp = lastyear
            Else
                endApp = Math.Min(lastyear, (firstyear + lastAppyear.Text - 1))
            End If

            mm = 0
            'Include Multi Crop Cycles
            For i As Integer = startApp To endApp Step appFrequency.Text   'i holds the year

                k = 0  'counts the total applications 
                'crop cycle loop (m)

                'for relative apps, we put on the appication series for each crop,
                'but for absolute apps, we only put on by the specified dates


                For m As Integer = 0 To loopendvalue           'm holds the crop cycle
                    ' loop for the specified dates

                    For j As Integer = 0 To appsArraySize '(appNumber.text -1)    appNumberConsideringMultiCrop

                        'returns the cam, depi and tband
                        ApplicationInfo.GetApplicationMethodProperties(j, cam, depi, tband_local)

                        '                              d  m   yr  cam dep Rat eff spr cam=1  0    0   0     0   1     0   0    0    0
                        tempmsg = tempmsg & String.Format("{0}{1},{2},{3},{4},{5},{6},{7},{8}, 1, 4.0, 0, 0, 0, 1, 4.0, 0, 0, 0",
                              vbNewLine, ApplicationInfo.PestAppyDay(k), ApplicationInfo.PestAppyMon(k), i, cam, depi, effectiveAppRate(j), localEff(j), tband_local)
                        'the dates here are k because the dates change for a new crop but the other parameters do not

                        TotalMassInput = TotalMassInput + effectiveAppRate(j)

                        'Store this info for vvwm
                        ApplicationInfo.applicationDates(mm) = DateSerial(i, ApplicationInfo.PestAppyMon(k), ApplicationInfo.PestAppyDay(k))
                        ApplicationInfo.sprayTotalArray(mm) = localSpray(j)
                        ApplicationInfo.rateTotalArray(mm) = effectiveAppRate(j)

                        mm = mm + 1  'total accounting
                        k = k + 1 ' reference for application single year, (while j is is reference for just applications)

                        ApplicationInfo.totalApplications = ApplicationInfo.totalApplications + 1
                    Next
                Next
            Next
        End If




        msg = msg & String.Format("{0}***Record C1  Number of Applications, Number Of Chemicals", vbNewLine)

        msg = msg & String.Format("{0}{1}, {2}, {3}, {4}, {5}, {6}, {7}", vbNewLine, ApplicationInfo.totalApplications, nchem, AdjustForRain.Checked, rain_limit.Text, intolerable_rain_window.Text, optimum_application_window.Text, MinDaysBetweenApps.Text)

        msg = msg & String.Format("{0}***Record C2  dd, mm, yy, cam, dep, Rate, eff, tband, 1cam, 1Dep, 1.0Rate, 1eff, 0, 2cam, 2Dep, 2.0Rate, 2eff, 0 ", vbNewLine)
        msg = msg & tempmsg


        ApplicationInfo.TotalMassApplied = TotalMassInput * FieldSize  'area box is in m2, appinfo is kg/ha, final in kg
        msg = msg & String.Format("{0}***Record C3: UPTKF (uptake factors)", vbNewLine)

        msg = msg & vbNewLine & "0,0,0" 'set all uptake tp zero


        Dim parentTo3 As Integer = 0
        Dim parentTo2 As Integer = 0
        Dim deg1To2 As Integer = 0

        If Deg2CheckBox.Checked Then
            deg1To2 = convertFoliar2.Text
        Else
            deg1To2 = 0
        End If

        Dim foliarHalfLifeBox(3) As TextBox
        Dim foliarRate(3) As Single

        foliarHalfLifeBox(1) = foliarDeg1
        foliarHalfLifeBox(2) = foliarDeg2
        foliarHalfLifeBox(3) = foliarDeg3


        Dim foliarWashoffBox(3) As TextBox
        Dim foliarWashoff(3) As Single

        foliarWashoffBox(1) = FoliarWashoff_1
        foliarWashoffBox(2) = FoliarWashoff_2
        foliarWashoffBox(3) = FoliarWashoff_3
        For i As Integer = 1 To nchem
            foliarWashoff(i) = foliarWashoffBox(i).Text
        Next




        For i As Integer = 1 To nchem
            If foliarHalfLifeBox(i).Text = "" Then
                foliarRate(i) = 0.0   'blank means stable
            ElseIf foliarHalfLifeBox(i).Text = 0 Then
                foliarRate(i) = 0.0   'half life of 0 means stable
            Else
                foliarRate(i) = 0.69314 / foliarHalfLifeBox(i).Text
            End If
        Next



        'Are there any foliar applications?
        Dim thereAreFoliarApps As Boolean = False

        'Foliar apps are in the array at 1, 5, 9 etc
        For i As Integer = 1 To appNumber.Text * ApplicationInfo.NumberofApplicationTypes Step ApplicationInfo.NumberofApplicationTypes
            If ApplicationInfo.methodbutton(i).Checked Then
                thereAreFoliarApps = True
                Exit For
            End If
        Next

        If thereAreFoliarApps Then
            For i As Integer = 1 To nchem
                msg = msg & String.Format("{0}***Record C4 (Chem #{1})", vbNewLine, i)
                msg = msg & String.Format("{0}{1}, {2}, {3}", vbNewLine, 0, foliarRate(i), foliarWashoff(i))
            Next

            If nchem > 1 Then
                msg = msg & String.Format("{0}***Record C5", vbNewLine)
                msg = msg & String.Format("{0}{1}, {2}, {3}", vbNewLine, convertFoliar1.Text, parentTo3, deg1To2)
            End If
        End If

        msg = msg & String.Format("{0}***Record C6: volatilization", vbNewLine)
        'msg = msg & String.Format("{0}{1},{1},{1},{1},{1},{1},{1},{1},{1}", vbNewLine, 0.0)

        Dim airDiff(3) As Single
        Dim hen(3) As Single
        Dim enth(3) As Single

        Dim henryBox(3) As TextBox
        Dim airDiffBox(3) As TextBox
        Dim enthalBox(3) As TextBox

        henryBox(1) = henry1
        henryBox(2) = henry2
        henryBox(3) = henry3

        airDiffBox(1) = airDiffusion1
        airDiffBox(2) = airDiffusion2
        airDiffBox(3) = airDiffusion3

        enthalBox(1) = enthalpyGas1
        enthalBox(2) = enthalpyGas2
        enthalBox(3) = enthalpyGas3

        For i As Integer = 1 To nchem
            If henryBox(i).Text = "" Then
                hen(i) = 0.0   'Blank means stable
            Else
                hen(i) = Convert.ToSingle(henryBox(i).Text)
            End If
        Next

        For i As Integer = 1 To nchem
            If airDiffBox(i).Text = "" Then
                airDiff(i) = 0.0   'Blank means stable
            Else
                airDiff(i) = Convert.ToSingle(airDiffBox(i).Text)
            End If
        Next

        For i As Integer = 1 To nchem
            If enthalBox(i).Text = "" Then
                enth(i) = 0.0   'Blank means stable
            Else
                ' convert enthalpy from Joules to Kcal for PRZM  4184 J/kcal
                enth(i) = Convert.ToSingle(enthalBox(i).Text) / 4184.0
            End If
        Next

        For i As Integer = 1 To nchem
            msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, airDiff(i), hen(i), enth(i))
        Next


        'Select Case nchem
        '    Case 1   ' convert enthalpy from Joules to Kcal for PRZM  4184 J/kcal
        '        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, airDiffusion1.Text, henry1.Text, Convert.ToSingle(enthalpyGas1.Text) / 4184.0)
        '    Case 2
        '        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, airDiffusion1.Text, henry1.Text, Convert.ToSingle(enthalpyGas1.Text) / 4184.0)
        '        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, airDiffusion2.Text, henry2.Text, Convert.ToSingle(enthalpyGas2.Text) / 4184.0)
        '    Case 3
        '        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, airDiffusion1.Text, henry1.Text, Convert.ToSingle(enthalpyGas1.Text) / 4184.0)
        '        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, airDiffusion2.Text, henry2.Text, Convert.ToSingle(enthalpyGas2.Text) / 4184.0)
        '        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, airDiffusion3.Text, henry3.Text, Convert.ToSingle(enthalpyGas3.Text) / 4184.0)
        'End Select


        Dim kdfactor As Single
        msg = msg & String.Format("{0}*** Record C7: Kf1, Kf2, Kf3 for each horizon", vbNewLine)

        For i As Integer = 0 To Convert.ToInt16(numHoriz.Text) - 1

            If IsKocRadioButton.Checked Then
                kdfactor = SoilProperty.oc(i).Text * 0.01
            Else
                kdfactor = 1
            End If

            Select Case nchem
                Case 1
                    kd1 = KocBox1.Text * kdfactor
                    SoilProperty.KdLayers(1, i) = kd1  'store for later throughput calcs
                    msg = msg & String.Format("{0}{1},", vbNewLine, kd1)
                Case 2
                    kd1 = KocBox1.Text * kdfactor
                    kd2 = KocBox2.Text * kdfactor
                    SoilProperty.KdLayers(1, i) = kd1
                    SoilProperty.KdLayers(2, i) = kd2
                    msg = msg & String.Format("{0}{1},{2},", vbNewLine, kd1, kd2)

                Case 3
                    kd1 = KocBox1.Text * kdfactor
                    kd2 = KocBox2.Text * kdfactor
                    kd3 = KocBox3.Text * kdfactor
                    SoilProperty.KdLayers(1, i) = kd1
                    SoilProperty.KdLayers(2, i) = kd2
                    SoilProperty.KdLayers(3, i) = kd3
                    msg = msg & String.Format("{0}{1},{2},{3}", vbNewLine, kd1, kd2, kd3)
            End Select

        Next

        msg = msg & String.Format("{0}*** Record C7A: N1, N2, N3 Freundlich Exponents for each horizon", vbNewLine)

        For i As Integer = 0 To Convert.ToInt16(numHoriz.Text) - 1
            Select Case nchem
                Case 1
                    msg = msg & String.Format("{0}{1},", vbNewLine, FreundlichExp1.Text)
                Case 2
                    'kd1 = KocBox1.Text * kdfactor
                    'kd2 = KocBox2.Text * kdfactor
                    msg = msg & String.Format("{0}{1},{2},", vbNewLine, FreundlichExp1.Text, FreundlichExp2.Text)

                Case 3
                    'kd1 = KocBox1.Text * kdfactor
                    'kd2 = KocBox2.Text * kdfactor
                    'kd3 = KocBox3.Text * kdfactor
                    msg = msg & String.Format("{0}{1},{2},{3}", vbNewLine, FreundlichExp1.Text, FreundlichExp2.Text, FreundlichExp3.Text)
            End Select
        Next

        msg = msg & String.Format("{0}*** Record C7B: Region 2 Freundlich Coefficient for each horizon", vbNewLine)

        For i As Integer = 0 To Convert.ToInt16(numHoriz.Text) - 1
            Select Case nchem
                Case 1
                    msg = msg & String.Format("{0}{1},", vbNewLine, FreundlichCoeffRegion2.Text)
                Case 2
                    msg = msg & String.Format("{0}{1},{2},", vbNewLine, FreundlichCoeffRegion2.Text, FreundlichCoeffRegion2Daughter.Text)
                Case 3
                    msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, FreundlichCoeffRegion2.Text, FreundlichCoeffRegion2Daughter.Text, FreundlichCoeffRegion2Granddaughter.Text)
            End Select
        Next

        msg = msg & String.Format("{0}*** Record C7C: Region 2 Freundlich Exponents for each horizon", vbNewLine)
        For i As Integer = 0 To Convert.ToInt16(numHoriz.Text) - 1
            Select Case nchem
                Case 1
                    msg = msg & String.Format("{0}{1},", vbNewLine, FreundlichExpRegion2.Text)
                Case 2
                    msg = msg & String.Format("{0}{1},{2},", vbNewLine, FreundlichExpRegion2.Text, FreundlichExpRegion2Daughter.Text)
                Case 3
                    msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, FreundlichExpRegion2.Text, FreundlichExpRegion2Daughter.Text, FreundlichExpRegion2GrandDaughter.Text)
            End Select
        Next



        msg = msg & String.Format("{0}*** Record C7D: Lowest Concentration (mg/L) for Freundlich Exponent", vbNewLine)
        msg = msg & String.Format("{0}{1},", vbNewLine, FreundlichMinimumConc.Text)


        msg = msg & String.Format("{0}*** Record C7E: Sorbed-Phase-Referenced Mass-Transfer Coefficient", vbNewLine)


        Select Case nchem
            Case 1
                msg = msg & String.Format("{0}{1},", vbNewLine, MassTransferRegion2.Text)
            Case 2
                msg = msg & String.Format("{0}{1},{2},", vbNewLine, MassTransferRegion2.Text, MassTransferRegion2Daughter.Text)
            Case 3
                msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, MassTransferRegion2.Text, MassTransferRegion2Daughter.Text, MassTransferRegion2GrandDaughter.Text)
        End Select


        msg = msg & String.Format("{0}*** Record C8: Degradation Rates Aqueous, Sorbed, Gas", vbNewLine)


        Dim hydroLocal1, hydroLocal2, hydroLocal3 As String
        Dim HydroRate1, HydroRate2, HydroRate3 As Single

        'this is to take care of blank entries without altering text boxes themselves
        hydroLocal1 = hydroBox1.Text
        hydroLocal2 = hydroBox2.Text
        hydroLocal3 = hydroBox3.Text

        If hydroLocal1 = "0" Or hydroLocal1 = "" Then
            HydroRate1 = 0.0
        Else
            HydroRate1 = 0.69314 / hydroBox1.Text
        End If

        If nchem > 1 Then
            If hydroLocal2 = "0" Or hydroLocal2 = "" Then
                HydroRate2 = 0.0
            Else
                HydroRate2 = 0.69314 / hydroBox2.Text
            End If
        End If

        If nchem > 2 Then
            If hydroLocal3 = "0" Or hydroLocal3 = "" Then
                HydroRate3 = 0.0
            Else
                HydroRate3 = 0.69314 / hydroBox3.Text
            End If
        End If

        Dim ReducedSoilRate1, ReducedSoilRate2, ReducedSoilRate3 As Single
        Dim SorbedDegradationRate1, SorbedDegradationRate2, SorbedDegradationRate3 As Single
        Dim AqueousDegradationRate1, AqueousDegradationRate2, AqueousDegradationRate3 As Single

        Dim startdepth, enddepth As Single
        Dim degradationReductionLocal(SoilProperty.maxSoilLayers) As Single
        Dim molarConvertGW_aq12(SoilProperty.maxSoilLayers) As Single  'special molar conversion for hydrolysis plus biodeg
        Dim molarConvertGW_aq23(SoilProperty.maxSoilLayers) As Single

        Dim convert1to3_aq As Single
        Dim convert2to3_aq As Single
        Dim convert1to2_aq As Single
        Dim convert1to3_s As Single
        Dim convert2to3_s As Single
        Dim convert1to2_s As Single

        Dim degradation_zone_depth As Single


        Select Case WaterBodyType
            Case StandardParameters.EPAGroundWater
                enddepth = 0.0
                startdepth = 0.0



                'Degradation Reductuction based on NAFTA guidance: first 10 cm no reduction, then linearly decreasing to 0 at 1 meter.
                If use_adjustable_profile.Checked Then

                    degradation_zone_depth = Degradation_Depth.Text

                    For i As Integer = 0 To numHoriz.Text - 1
                        enddepth = enddepth + SoilProperty.thick(i).Text

                        'case where horizon is less than 10 cm
                        If enddepth <= 10.0 Then
                            degradationReductionLocal(i) = 1.0

                            'case where horizon starts before 10 cm and ends in deg zone--it spans the first constant section
                        ElseIf enddepth > 10.0 And enddepth <= degradation_zone_depth And startdepth <= 10.0 Then
                            degradationReductionLocal(i) = ((10 - startdepth) + (enddepth - 10.0) * (1.0 - ((enddepth + 10.0) / 2.0 - 10.0) / (degradation_zone_depth - 10.0))) / (enddepth - startdepth)

                            'typical start and finish in the declining zone
                        ElseIf enddepth > 10.0 And enddepth <= degradation_zone_depth And startdepth > 10.0 Then
                            degradationReductionLocal(i) = (1.0 - ((enddepth + startdepth) / 2.0 - 10.0) / (degradation_zone_depth - 10.0))

                            'horizon ends after the deg zone but and starts after the 10 cm zone
                        ElseIf enddepth > degradation_zone_depth And startdepth > 10.0 And startdepth < degradation_zone_depth Then
                            degradationReductionLocal(i) = (degradation_zone_depth - startdepth) * (1.0 - ((startdepth + degradation_zone_depth) / 2.0 - 10.0) / (degradation_zone_depth - 10.0)) / (enddepth - startdepth)

                            'case where horizon starts inside the 10 cm zone and ends outside the deg zone
                        ElseIf enddepth > degradation_zone_depth And startdepth < 10.0 Then
                            degradationReductionLocal(i) = ((10.0 - startdepth) + (degradation_zone_depth - 10.0) / 2.0) / (enddepth - startdepth)

                        ElseIf startdepth > degradation_zone_depth Then
                            degradationReductionLocal(i) = 0.0
                        End If

                        startdepth = startdepth + SoilProperty.thick(i).Text 'get startdepth fo next horizon

                    Next

                Else


                    'OLD SCHEME
                    For i As Integer = 0 To numHoriz.Text - 1
                        enddepth = enddepth + SoilProperty.thick(i).Text

                        If enddepth <= 10.0 Then
                            degradationReductionLocal(i) = 1.0

                        ElseIf enddepth > 10.0 And enddepth <= 100.0 And startdepth <= 10.0 Then

                            degradationReductionLocal(i) = ((10 - startdepth) + (enddepth - 10.0) * (1.0 - ((enddepth + 10.0) / 2.0 - 10.0) / 90.0)) / (enddepth - startdepth)

                        ElseIf enddepth > 10.0 And enddepth <= 100.0 And startdepth > 10.0 Then
                            degradationReductionLocal(i) = (1.0 - ((enddepth + startdepth) / 2.0 - 10.0) / 90.0)

                        ElseIf enddepth > 100.0 And startdepth > 10.0 And startdepth < 100.0 Then
                            degradationReductionLocal(i) = (100.0 - startdepth) * (1.0 - ((startdepth + 100.0) / 2.0 - 10.0) / 90.0) / (enddepth - startdepth)

                        ElseIf enddepth > 100.0 And startdepth < 10.0 Then
                            degradationReductionLocal(i) = ((10.0 - startdepth) + 45.0) / (enddepth - startdepth)
                        ElseIf enddepth > 100.0 And startdepth > 100.0 Then
                            degradationReductionLocal(i) = 0.0
                        End If

                        startdepth = startdepth + SoilProperty.thick(i).Text 'get startdepth fo next horizon

                    Next


                End If


                For i As Integer = 0 To Convert.ToInt16(numHoriz.Text) - 1
                    ReducedSoilRate1 = soilRate(1) * degradationReductionLocal(i)
                    ReducedSoilRate2 = soilRate(2) * degradationReductionLocal(i)
                    ReducedSoilRate3 = soilRate(3) * degradationReductionLocal(i)

                    AqueousDegradationRate1 = System.Math.Max(ReducedSoilRate1, HydroRate1)
                    AqueousDegradationRate2 = System.Math.Max(ReducedSoilRate2, HydroRate2)
                    AqueousDegradationRate3 = System.Math.Max(ReducedSoilRate3, HydroRate3)

                    If (ReducedSoilRate1 + HydroRate1) > 0 Then
                        molarConvertGW_aq12(i) = (ReducedSoilRate1 * convertSoil1.Text + HydroRate1 * convertHydro1.Text) / (ReducedSoilRate1 + HydroRate1)
                    Else
                        molarConvertGW_aq12(i) = 0.0
                    End If

                    If (ReducedSoilRate2 + HydroRate2) > 0 Then
                        molarConvertGW_aq23(i) = (ReducedSoilRate2 * convertSoil2.Text + HydroRate2 * convertHydro2.Text) / (ReducedSoilRate2 + HydroRate2)
                    Else
                        molarConvertGW_aq23(i) = 0.0
                    End If


                    'If ReducedSoilRate1 >= HydroRate1 Then
                    '    molarConvertGW_aq12(i) = convertSoil1.Text
                    'Else
                    '    molarConvertGW_aq12(i) = convertHydro1.Text
                    'End If

                    'If ReducedSoilRate2 >= HydroRate2 Then
                    '    molarConvertGW_aq23(i) = convertSoil2.Text
                    'Else
                    '    molarConvertGW_aq23(i) = convertHydro2.Text
                    'End If

                    SorbedDegradationRate1 = ReducedSoilRate1
                    SorbedDegradationRate2 = ReducedSoilRate2
                    SorbedDegradationRate3 = ReducedSoilRate3


                    If NoSorbedDegradation.Checked Then  'No sorbed phase degradation
                        SorbedDegradationRate1 = 0.0
                        SorbedDegradationRate2 = 0.0
                        SorbedDegradationRate3 = 0.0
                    End If


                    Select Case nchem
                        Case 1
                            msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, AqueousDegradationRate1, SorbedDegradationRate1, 0)
                        Case 2
                            msg = msg & String.Format("{0}{1},{2},{5},{3},{4},{5},", vbNewLine, AqueousDegradationRate1, SorbedDegradationRate1, AqueousDegradationRate2, SorbedDegradationRate2, 0)
                        Case 3
                            msg = msg & String.Format("{0}{1},{2},{7},{3},{4},{7},{5},{6},{7},", vbNewLine, AqueousDegradationRate1, SorbedDegradationRate1, AqueousDegradationRate2, SorbedDegradationRate2, AqueousDegradationRate3, SorbedDegradationRate3, 0)
                    End Select
                Next

                'Hydrolysis and Soil degradation are combined


                msg = msg & String.Format("{0}*** Record C9: Molar Conversions 1 to 2, 1 to 3, 2 to 3", vbNewLine)
                For i As Integer = 0 To Convert.ToInt16(numHoriz.Text) - 1
                    msg = msg & String.Format("{0}{1},{2},{3},{4},{5},{6}", vbNewLine, molarConvertGW_aq12(i), 0.0, molarConvertGW_aq23(i), convertSoil1.Text, 0.0, convertSoil2.Text)
                Next

            Case Else                     'All surface Wter bodies use this degradation configuration


                For i As Integer = 0 To Convert.ToInt16(numHoriz.Text) - 1
                    Select Case nchem
                        Case 1
                            msg = msg & String.Format("{0}{1},{1},{2},", vbNewLine, soilRate(1), 0)
                        Case 2
                            msg = msg & String.Format("{0}{1},{1},{3},{2},{2},{3},", vbNewLine, soilRate(1), soilRate(2), 0)
                        Case 3
                            msg = msg & String.Format("{0}{1},{1},{4},{2},{2},{4},{3},{3},{4},", vbNewLine, soilRate(1), soilRate(2), soilRate(3), 0)
                    End Select
                Next
                Select Case nchem
                    Case 1
                        convert1to2_aq = 0.0
                        convert1to3_aq = 0.0
                        convert2to3_aq = 0.0

                        convert1to2_s = 0.0
                        convert1to3_s = 0.0
                        convert2to3_s = 0.0

                    Case 2
                        convert1to2_aq = convertSoil1.Text
                        convert1to3_aq = 0.0
                        convert2to3_aq = 0.0

                        convert1to2_s = convertSoil1.Text
                        convert1to3_s = 0.0
                        convert2to3_s = 0.0

                    Case 3
                        convert1to2_aq = convertSoil1.Text
                        convert1to3_aq = 0.0
                        convert2to3_aq = convertSoil2.Text

                        convert1to2_s = convertSoil1.Text
                        convert1to3_s = 0.0
                        convert2to3_s = convertSoil2.Text
                End Select

                msg = msg & String.Format("{0}*** Record C9: Molar Conversions 1 to 2, 1 to 3, 2 to 3", vbNewLine)
                For i As Integer = 0 To Convert.ToInt16(numHoriz.Text) - 1
                    msg = msg & String.Format("{0}{1},{2},{3},{4},{5},{6}", vbNewLine, convert1to2_aq, convert1to3_aq, convert2to3_aq, convert1to2_s, convert1to3_s, convert2to3_s)
                Next
        End Select








        msg = msg & String.Format("{0}********** OUTPUT SPECIFICATIONS ***********************", vbNewLine)
        msg = msg & String.Format("{0}*** Record U1", vbNewLine)


        '****************Calculate Total Number of Time Series to Print **************************************
        Dim NumberOfSeries As Integer
        NumberOfSeries = 3 + 3 * nchem

        If OutputMassInSoilProfile.Checked Then
            NumberOfSeries = NumberOfSeries + nchem
        End If

        If OutputVolatilization.Checked Then
            NumberOfSeries = NumberOfSeries + nchem
        End If

        If CumulativeVolatilization.Checked Then
            NumberOfSeries = NumberOfSeries + nchem
        End If

        If OutputLeached.Checked Then
            NumberOfSeries = NumberOfSeries + nchem
        End If

        If CumulativeLeached.Checked Then
            NumberOfSeries = NumberOfSeries + nchem
        End If



        If OutputDecayed.Checked Then
            NumberOfSeries = NumberOfSeries + nchem
        End If

        If OutputMassSoilSpecific.Checked Then
            NumberOfSeries = NumberOfSeries + nchem
        End If

        If OutputMassOnFoliage.Checked Then
            NumberOfSeries = NumberOfSeries + nchem
        End If





        If OutputPrecipitation.Checked Then
            NumberOfSeries = NumberOfSeries + 1
        End If


        If OutputActualEvap.Checked Then
            NumberOfSeries = NumberOfSeries + 1
        End If

        If OutputTotalSoilWater.Checked Then
            NumberOfSeries = NumberOfSeries + 1
        End If

        If OutputIrrigation.Checked Then
            NumberOfSeries = NumberOfSeries + 1
        End If

        If OutputInfiltrationAtDepth.Checked Then
            NumberOfSeries = NumberOfSeries + 1
        End If

        'THIS IS FOF THE NEW MASS CALCULATION TEST OUTPUT

        If OutputEquilibriumMass.Checked Then
            NumberOfSeries = NumberOfSeries + nchem  'for equilibrium mass
        End If


        If OutputNonEquilibriumMass.Checked Then
            NumberOfSeries = NumberOfSeries + nchem  'for nonequilibrium mass
        End If

        '*****************************************************

        '****************************************************************************************


        msg = msg & String.Format("{0}{1}", vbNewLine, NumberOfSeries)

        msg = msg & String.Format("{0}*** Record U2", vbNewLine)
        msg = msg & String.Format("{0}RUNF,0,TSER,   0,   0,    1.0", vbNewLine)
        msg = msg & String.Format("{0}ESLS,0,TSER,   0,   0,    1.0", vbNewLine)


        '************ Molecular Weight Conversions **************************
        Dim MWTs As String() = {mwtBox1.Text, mwtBox2.Text, mwtBox3.Text}
        Dim MwtRatio As Single() = {1.0, 1.0, 1.0, 1.0}
        For i As Integer = 1 To nchem
            MwtRatio(i) = MWTs(i - 1) / MWTs(0)
        Next
        '*********************************************************************

        For i As Integer = 1 To nchem
            msg = msg & String.Format("{0}RFLX,{1},TSER,   0,   0,    {2}", vbNewLine, i, MwtRatio(i))
            msg = msg & String.Format("{0}EFLX,{1},TSER,   0,   0,    {2}", vbNewLine, i, MwtRatio(i))
        Next


        'Now is set up to take average concentration of last horizon regardless of number of horizons
        Dim node1, node2 As Integer
        Dim BottomNode As Integer

        node1 = 1
        node2 = SoilProperty.compartment(0).Text

        For i As Integer = 1 To numHoriz.Text - 1
            node1 = node1 + SoilProperty.compartment(i - 1).Text
            node2 = node2 + SoilProperty.compartment(i).Text
        Next
        BottomNode = node2

        'node1 is the first compartment of last horizon
        'node2(Bottom Node) is the last compartment

        'For i As Integer = 1 To nchem
        '    msg = msg & String.Format("{0}DCON,{3},TAVE,  {1},  {2},   1000.", vbNewLine, node1, BottomNode, i)
        'Next


        'Find node that is 1 meter above the botton. This is the standard 1 meter screen length:
        Dim NodeOfScreenTop As Integer
        Dim DepthOfScreenBottom, DepthOfScreenTop As Single

        DepthOfScreenBottom = SoilProperty.FindDepth(BottomNode, numHoriz.Text)
        DepthOfScreenTop = DepthOfScreenBottom - 100.0  'Screen Length is set to 100 cm here




        'Must add 1 node in the following because, the depths are indicative of the depth of the bottom of the compaprtmnent, so therefore we need the next compartment (the top of the scee
        NodeOfScreenTop = SoilProperty.FindNode(DepthOfScreenTop, numHoriz.Text) + 1




        For i As Integer = 1 To nchem
            msg = msg & String.Format("{0}DCON,{3},TAVE,   {1},  {2},    {4}", vbNewLine, NodeOfScreenTop, BottomNode, i, 1000 * MwtRatio(i))
        Next

        msg = msg & String.Format("{0}INFL,0,TCUM,   {1},   {1},    1.0", vbNewLine, BottomNode)

        If OutputMassInSoilProfile.Checked Then   'Request to print out Total Soil pesticide in entire profile
            For i As Integer = 1 To nchem
                msg = msg & String.Format("{0}TPST,{2},TSUM,   1,  {1},  {3}", vbNewLine, BottomNode, i, 100000 * MwtRatio(i))
            Next
        End If




        If OutputVolatilization.Checked Then   'Request to volatilized pestide
            For i As Integer = 1 To nchem
                msg = msg & String.Format("{0}VFLX,{1},TSER,   1,  1,  {2}", vbNewLine, i, 100000 * MwtRatio(i))
            Next
        End If



        If CumulativeVolatilization.Checked Then   'Request to print out Total Soil pesticide in entire profile
            For i As Integer = 1 To nchem
                msg = msg & String.Format("{0}VFLX,{1},TCUM,   1,  1,  {2}", vbNewLine, i, 100000 * MwtRatio(i))
            Next
        End If




        If OutputLeached.Checked Then
            For i As Integer = 1 To nchem
                msg = msg & String.Format("{0}COFX,{1},TSER,   {2},  {2},  {3}", vbNewLine, i, 0, 100000 * MwtRatio(i))
            Next
        End If


        If CumulativeLeached.Checked Then
            For i As Integer = 1 To nchem
                msg = msg & String.Format("{0}COFX,{1},TCUM,   {2},  {2},  {3}", vbNewLine, i, 0, 100000 * MwtRatio(i))
            Next
        End If




        If OutputDecayed.Checked Then
            node1 = SoilProperty.FindNode(OutputDecayDepth1.Text, numHoriz.Text)
            node2 = SoilProperty.FindNode(OutputDecayDepth2.Text, numHoriz.Text)
            For i As Integer = 1 To nchem
                msg = msg & String.Format("{0}DKFX,{1},TSUM,   {2},  {3},  {4}", vbNewLine, i, node1, node2, 100000 * MwtRatio(i))
            Next
        End If


        If OutputMassSoilSpecific.Checked Then
            node1 = SoilProperty.FindNode(OutputMassDepth1.Text, numHoriz.Text)
            node2 = SoilProperty.FindNode(OutputMassDepth2.Text, numHoriz.Text)
            For i As Integer = 1 To nchem
                msg = msg & String.Format("{0}TPST,{1},TSUM,   {2},  {3},  {4}", vbNewLine, i, node1, node2, 100000 * MwtRatio(i))
            Next
        End If


        If OutputMassOnFoliage.Checked Then
            For i As Integer = 1 To nchem
                msg = msg & String.Format("{0}FPST,{1},TSER,   1,  1,  {2}", vbNewLine, i, 100000 * MwtRatio(i))
            Next
        End If

        If OutputPrecipitation.Checked Then
            msg = msg & String.Format("{0}PRCP,0,TSER,   0,   0,    1.0", vbNewLine)
        End If


        If OutputActualEvap.Checked Then
            msg = msg & String.Format("{0}TETD,0,TSER,   0,   0,    1.0", vbNewLine)
        End If


        If OutputTotalSoilWater.Checked Then
            msg = msg & String.Format("{0}SWTR,0,TSUM,   1,   {1},    1.0", vbNewLine, BottomNode)
        End If



        If OutputIrrigation.Checked Then
            msg = msg & String.Format("{0}IRRG,0,TSER,   0,   0,    1.0", vbNewLine)
        End If


        If OutputInfiltrationAtDepth.Checked Then
            node1 = SoilProperty.FindNode(OutputInfiltrationDepth.Text, numHoriz.Text)
            msg = msg & String.Format("{0}INFL,0,TSER,   {1},  {1},    1.0", vbNewLine, node1)
        End If


        If OutputEquilibriumMass.Checked Then
            For i As Integer = 1 To nchem
                msg = msg & String.Format("{0}MASS,{2},TSUM,   1,  {1},  {3}", vbNewLine, BottomNode, i, 100000 * MwtRatio(i))
            Next
        End If


        If OutputNonEquilibriumMass.Checked Then
            For i As Integer = 1 To nchem
                msg = msg & String.Format("{0}MAS2,{2},TSUM,   1,  {1},  {3}", vbNewLine, BottomNode, i, 100000 * MwtRatio(i))
            Next
        End If


        msg = msg & vbCrLf


        Try
            My.Computer.FileSystem.WriteAllText(workingDirectoryLabel.Text & "PRZM5.inp", msg, False, System.Text.Encoding.ASCII)
        Catch ex As Exception
            MsgBox(ex.Message & " Creating PRZM5 input file.")
        End Try

    End Sub

    Private Sub createTransferFile(ByVal filename As String, ByVal WaterBodyType As String)
        'Transfer file to the VVWM
        Dim msg As String

        msg = workingDirectoryLabel.Text & ioFamilyNameBox.Text
        msg = msg & vbNewLine & ChemInfo()
        '     msg = msg & vbNewLine & ApplicationInfo.AppInfoString()
        msg = msg & vbNewLine & WaterBodyInfo()
        msg = msg & vbNewLine & TransferFileSpecificInfo(WaterBodyType)
        msg = msg & vbNewLine & isMinimumBatchOutput.Checked  'end with a return to avoid that bizarre rare file size error in intel fortran
        msg = msg & String.Format("{0}""{1}""", vbNewLine, FileNameClass.ScenarioRunID) & vbNewLine

        Try
            System.IO.Directory.SetCurrentDirectory(workingDirectoryLabel.Text)
            My.Computer.FileSystem.WriteAllText(filename, msg, False, System.Text.Encoding.ASCII)
        Catch ex As Exception
            MsgBox(ex.Message)
        End Try
    End Sub
    Private Function TransferFileSpecificInfo(ByVal WaterBodyType As String) As String
        'these are application days either relative or avbsolute that are to be used by the PRZM & VVWM
        Dim msg As String = ""
        msg = msg & ApplicationInfo.totalApplications  'Line 56
        msg = msg & vbNewLine


        'this gives the app date as the number of days into the simulation  'Line 57
        For i As Integer = 0 To ApplicationInfo.totalApplications - 1
            msg = msg & String.Format("{0},", DateDiff("d", ApplicationInfo.startdate, ApplicationInfo.applicationDates(i)) + 1)

        Next
        msg = msg & vbNewLine


        Dim localWaterArea As Single  ' in m2 of water body area
        Dim croppedfraction As Single

        'This converts the reservoir and pond into thier correct SimTypes in VVWM

        Select Case WaterBodyType
            Case StandardParameters.EpaReservoir
                msg = msg & "3" ' this will override simulation type in vvwm     Line 58
                msg = msg & String.Format("{0}{1}", vbNewLine, StandardParameters.reservoirField)
                msg = msg & String.Format("{0}{1}", vbNewLine, StandardParameters.reservoirArea)
                msg = msg & String.Format("{0}{1}", vbNewLine, StandardParameters.reservoirDepth)
                msg = msg & String.Format("{0}{1}", vbNewLine, StandardParameters.reservoirDepthMax)

                msg = msg & vbNewLine
                localWaterArea = StandardParameters.reservoirArea / 10000.0

                croppedfraction = ReservoirCroppedAreaBox.Text

            Case StandardParameters.EpaPond
                msg = msg & "2" ' this will override simulation type in vvwm
                msg = msg & String.Format("{0}{1}", vbNewLine, StandardParameters.pondField)
                msg = msg & String.Format("{0}{1}", vbNewLine, StandardParameters.pondArea)
                msg = msg & String.Format("{0}{1}", vbNewLine, StandardParameters.pondDepth)
                msg = msg & String.Format("{0}{1}", vbNewLine, StandardParameters.pondDepthMax)

                msg = msg & vbNewLine
                localWaterArea = StandardParameters.pondArea / 10000.0
                croppedfraction = PondCropAreaLabel.Text

                'For i As Integer = 0 To ApplicationInfo.totalApplications - 1
                '    msg = msg & ApplicationInfo.sprayp(i).Text & ","
                'Next


            Case StandardParameters.CustomVVWM
                msg = msg & "1"  'Varying Volume and Flowthrough
                msg = msg & String.Format("{0}{1}", vbNewLine, fieldAreaBox.Text)
                msg = msg & String.Format("{0}{1}", vbNewLine, waterAreaBox.Text)
                msg = msg & String.Format("{0}{1}", vbNewLine, initialDepthBox.Text)
                msg = msg & String.Format("{0}{1}", vbNewLine, maxDepthBox.Text)
                msg = msg & vbNewLine
                localWaterArea = waterAreaBox.Text / 10000.0
                croppedfraction = CustomCroppedAreaBox.Text

            Case StandardParameters.ConstVolFlo
                msg = msg & "5"  'Constant Volume flowthrough
                msg = msg & String.Format("{0}{1}", vbNewLine, fieldAreaBox.Text)
                msg = msg & String.Format("{0}{1}", vbNewLine, waterAreaBox.Text)
                msg = msg & String.Format("{0}{1}", vbNewLine, initialDepthBox.Text)
                msg = msg & String.Format("{0}{1}", vbNewLine, maxDepthBox.Text)
                msg = msg & vbNewLine
                localWaterArea = waterAreaBox.Text / 10000.0
                croppedfraction = CustomCroppedAreaBox.Text

            Case StandardParameters.ConstVolNoFlo
                msg = msg & "4"  'Constant Volume No flowthrough
                msg = msg & String.Format("{0}{1}", vbNewLine, fieldAreaBox.Text)
                msg = msg & String.Format("{0}{1}", vbNewLine, waterAreaBox.Text)
                msg = msg & String.Format("{0}{1}", vbNewLine, initialDepthBox.Text)
                msg = msg & String.Format("{0}{1}", vbNewLine, maxDepthBox.Text)
                msg = msg & vbNewLine
                localWaterArea = waterAreaBox.Text / 10000.0
                croppedfraction = CustomCroppedAreaBox.Text
        End Select

        '   ApplicationInfo.rateTotalArray  this holds the effective rate which includes cropped fraction

        Dim ff As Single
        For i As Integer = 0 To ApplicationInfo.totalApplications - 1
            ff = ApplicationInfo.sprayTotalArray(i) * ApplicationInfo.rateTotalArray(i) * localWaterArea
            msg = msg & String.Format("{0},", ff)              'Line 63
        Next


        'Line 64
        Select Case WaterBodyType
            Case StandardParameters.EpaReservoir
                If UserSpecifiedFlowAvg.Checked Then
                    msg = msg & String.Format("{0}{1}", vbNewLine, ReservoirFlowAvgDays.Text)
                Else
                    msg = msg & String.Format("{0}{1}", vbNewLine, 0)
                End If


            Case StandardParameters.ConstVolFlo
                msg = msg & String.Format("{0}{1}", vbNewLine, CustomFlowAvgDays.Text)


                'If CustomUserSpecifiedFlowAvg.Checked Then
                '    msg = msg & String.Format("{0}{1}", vbNewLine, CustomFlowAvgDays.Text)
                'Else
                '    msg = msg & String.Format("{0}{1}", vbNewLine, 0)
                'End If


            Case Else
                msg = msg & String.Format("{0}{1}", vbNewLine, 0)
        End Select


        Select Case WaterBodyType

            Case StandardParameters.EpaReservoir
                msg = msg & String.Format("{0}{1}", vbNewLine, 0)  'Line 65
            Case StandardParameters.EpaPond
                msg = msg & String.Format("{0}{1}", vbNewLine, 0)  'Line 65
            Case Else
                msg = msg & String.Format("{0}{1}", vbNewLine, baseFlow.Text)  'Line 65
        End Select



        msg = msg & String.Format("{0}{1}", vbNewLine, croppedfraction)

        msg = msg & String.Format("{0}{1}", vbNewLine, IsHEDFilesMade.Checked)

        msg = msg & String.Format("{0}{1},{2}", vbNewLine, IsAddFrequencyReturn.Checked, ReturnFrequency.Text)

        msg = msg & String.Format("{0}""{1}""", vbNewLine, FileNameClass.VVWMoutputFileParentTS)
        msg = msg & String.Format("{0}""{1}""", vbNewLine, FileNameClass.VVWMoutputFileDeg1TS)
        msg = msg & String.Format("{0}""{1}""", vbNewLine, FileNameClass.VVWMoutputFileDeg2TS)

        msg = msg & String.Format("{0}""{1}""", vbNewLine, FileNameClass.VVWMoutputFileParent)
        msg = msg & String.Format("{0}""{1}""", vbNewLine, FileNameClass.VVWMoutputFileDeg1)
        msg = msg & String.Format("{0}""{1}""", vbNewLine, FileNameClass.VVWMoutputFileDeg2)

        msg = msg & String.Format("{0}""{1}""", vbNewLine, FileNameClass.VVWMoutputFileParentDEEM)
        msg = msg & String.Format("{0}""{1}""", vbNewLine, FileNameClass.VVWMoutputFileDeg1DEEM)
        msg = msg & String.Format("{0}""{1}""", vbNewLine, FileNameClass.VVWMoutputFileDeg2DEEM)

        msg = msg & String.Format("{0}""{1}""", vbNewLine, FileNameClass.VVWMoutputFileParentCalendex)
        msg = msg & String.Format("{0}""{1}""", vbNewLine, FileNameClass.VVWMoutputFileDeg1Calendex)
        msg = msg & String.Format("{0}""{1}""", vbNewLine, FileNameClass.VVWMoutputFileDeg2Calendex)

        msg = msg & String.Format("{0}""{1}""", vbNewLine, FileNameClass.VVWMoutputFileParentESA)
        msg = msg & String.Format("{0}""{1}""", vbNewLine, FileNameClass.VVWMoutputFileDeg1ESA)
        msg = msg & String.Format("{0}""{1}""", vbNewLine, FileNameClass.VVWMoutputFileDeg2ESA)


        msg = msg & String.Format("{0}""{1}""", vbNewLine, FileNameClass.BatchOutputVVWM)


        Return msg
    End Function
    Private Sub NameTheVVWMoutputFiles(ByVal WaterBodyType As String)
        Dim pathstring As String

        pathstring = workingDirectoryLabel.Text & Trim(ioFamilyNameBox.Text) & "_" & Trim(scenarioID.Text) & "_" & StandardParameters.WaterBodyName(WaterBodyType)

        FileNameClass.VVWMoutputFileParent = pathstring & "_Parent.txt"
        FileNameClass.VVWMoutputFileDeg1 = pathstring & "_Degradate1.txt"
        FileNameClass.VVWMoutputFileDeg2 = pathstring & "_Degradate2.txt"

        FileNameClass.VVWMoutputFileParentTS = pathstring & "_Parent_daily.csv"
        FileNameClass.VVWMoutputFileDeg1TS = pathstring & "_Degradate1_daily.csv"
        FileNameClass.VVWMoutputFileDeg2TS = pathstring & "_Degradate2_daily.csv"

        FileNameClass.VVWMoutputFileParentESA = pathstring & "_" & Trim(ReturnFrequency.Text) & "_Parent.txt"
        FileNameClass.VVWMoutputFileDeg1ESA = pathstring & "_" & Trim(ReturnFrequency.Text) & "_Degradate1.txt"
        FileNameClass.VVWMoutputFileDeg2ESA = pathstring & "_" & Trim(ReturnFrequency.Text) & "_Degradate2.txt"

        FileNameClass.VVWMoutputFileParentCalendex = pathstring & "_Parent_Calendex.rdf"
        FileNameClass.VVWMoutputFileDeg1Calendex = pathstring & "_Degradate1_Calendex.rdf"
        FileNameClass.VVWMoutputFileDeg2Calendex = pathstring & "_Degradate2_Calendex.rdf"

        FileNameClass.VVWMoutputFileParentDEEM = pathstring & "_Parent_DEEM.rdf"
        FileNameClass.VVWMoutputFileDeg1DEEM = pathstring & "_Degradate1_DEEM.rdf"
        FileNameClass.VVWMoutputFileDeg2DEEM = pathstring & "_Degradate2_DEEM.rdf"

        ' FileNameClass.BatchOutputVVWM = workingDirectory.Text & "BatchOutputVVWM.txt"
        ' already named



    End Sub


    Public Sub RunPrograms(ByVal WaterBodyType As String, ByVal DayAdd As Integer, ByRef runflag As Boolean, ByRef ErrorMessage As String)
        'Runflag = true means an error occured
        Dim XFERfile As String
        Dim AppPath As String
        AppPath = My.Application.Info.DirectoryPath()
        Dim ErrorFlag As Boolean
        'Dim ErrorMessage As String



        Try
            CreatePrzm5String(DayAdd, WaterBodyType, ErrorFlag, ErrorMessage)

            If ErrorFlag = True Then
                DiagnosticMessage.Text = ErrorMessage
                runflag = True
                Return
            End If



            'Modify PRZM Input File Manually
            If PauseBeforePRZM.Checked Then
                MsgBox("You can now open and modify the PRZM file. Push OK to continue run.")
            End If


        Catch ex As Exception
            MsgBox("Problem in Creating PRZM Files, " & ex.Message)
            runflag = True
            Return
        End Try

        AppPath = My.Application.Info.DirectoryPath()




        Try

            Dim startInfo2 As New System.Diagnostics.ProcessStartInfo(AppPath & "\PRZM5.exe")
            If ShowPRZMcommand.Checked = False Then
                startInfo2.WindowStyle = ProcessWindowStyle.Hidden
            End If
            System.Diagnostics.Process.Start(startInfo2).WaitForExit()
        Catch ex As Exception
            MsgBox("Problem in executing PRZM. (After writing input file.) Is the executable missing? " & ex.Message)
            Return
        End Try




        'Check to see if PRZM ran correctly





        If BadPRZMrun() Then
            runflag = True

            Return
        End If





        If WaterBodyType = StandardParameters.EPAGroundWater Then
            '***** GROUND WATER STUFF HERE if any is needed in future
        ElseIf WaterBodyType = StandardParameters.NoWaterBody Then
            '***** PRZM ONLY STUFF HERE  'dont run vvwm
        Else




            '**** Manipulate the Zts File for Multiple Daysheds *****************************************
            If ESA_Run.Checked And DaySheds.NumDaySheds > 1 Then
                DaySheds.Calculate()
            End If
            '******************************************************************************************


            NameTheVVWMoutputFiles(WaterBodyType)

            XFERfile = workingDirectoryLabel.Text & "vvwmTransfer.txt"


            Try
                createTransferFile(XFERfile, WaterBodyType)
            Catch ex As Exception
                MsgBox("Problem in ceating transfer file " & ex.Message)
            End Try




            Try
                Dim startInfo As New System.Diagnostics.ProcessStartInfo(AppPath & "\VVWM.exe", """" & XFERfile & """")
                If ShowVVWMcommand.Checked = False Then
                    startInfo.WindowStyle = ProcessWindowStyle.Hidden
                End If
                System.Diagnostics.Process.Start(startInfo).WaitForExit()
            Catch ex As Exception
                MsgBox(ex.Message & ": VVWM.exe")
            End Try
        End If




    End Sub

    Private Function BadPRZMrun() As Boolean
        'This routine checks the kecho.prn file for the statement that PRZM ran OK.  If not then a message box is produced.
        'And an error flag is thrown



        BadPRZMrun = False
        If Not IO.File.Exists(workingDirectoryLabel.Text & "\" & "kecho.prn") Then
            DiagnosticMessage.Text = "Kecho.prn file does not exist.  PRZM did not run properly."
            Return True
        End If




        Dim line As String = "Fail"
        Dim testString As String = " PRZM5 program normal completion."

        Using reader As New System.IO.StreamReader(workingDirectoryLabel.Text & "/" & "kecho.prn")
            While Not reader.EndOfStream
                line = reader.ReadLine
            End While
        End Using

        If line <> testString Then


            DiagnosticMessage.Text = "PRZM did not run correctly.  See kecho.prn file"
            BadPRZMrun = True
        End If



    End Function

    Private Sub simTemperature_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs)

        If simTemperature.Checked = True Then
            For i As Integer = 0 To 5
                SoilProperty.sand(i).Visible = True
                SoilProperty.clay(i).Visible = True
            Next
            Label69.Visible = True
            Label70.Visible = True
            Label83.Visible = True
            Label84.Visible = True
            albedoBox.Visible = True
            bcTemp.Visible = True
        Else
            For i As Integer = 0 To 5
                SoilProperty.sand(i).Visible = False
                SoilProperty.clay(i).Visible = False
            Next
            Label69.Visible = False
            Label70.Visible = False
            Label83.Visible = False
            Label84.Visible = False
            albedoBox.Visible = False
            bcTemp.Visible = False
        End If

        updatehorizonsub()

    End Sub
    Private Sub UpdateApps_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles UpdateApps.Click

        updateApplications()

    End Sub

    Sub updateApplications()

        Dim ApplicationNumber As Integer

        Try
            ApplicationNumber = appNumber.Text
        Catch ex As Exception
            MsgBox("Application Number must be an integer")
            ApplicationNumber = 1
        End Try
        'Reset TextBox: convert real numbers to integers:

        appNumber.Text = ApplicationNumber

        If ApplicationNumber > ApplicationInfo.MaximumApplications Then
            ApplicationNumber = ApplicationInfo.MaximumApplications
            MsgBox(String.Format("Application Number maximum is {0}", ApplicationInfo.MaximumApplications))
            appNumber.Text = ApplicationInfo.MaximumApplications
        End If

        If ApplicationNumber < 1 Then
            ApplicationNumber = 1
            MsgBox("Application Number doesn't make sence.  Need at least 1 application")
            appNumber.Text = 1
        End If

        For i As Integer = 0 To ApplicationNumber - 1
            ApplicationInfo.day(i).Visible = True
            ApplicationInfo.mon(i).Visible = True
            ApplicationInfo.rate(i).Visible = True
            ApplicationInfo.MethodPanels(i).Visible = True
            '   ApplicationInfo.spray(i).Visible = True
            ApplicationInfo.RelApp(i).Visible = True
            ApplicationInfo.DepthIncorp(i).Visible = True
            '    ApplicationInfo.Eff(i).Visible = True
            ApplicationInfo.tBand(i).Visible = True
        Next

        For i As Integer = ApplicationNumber To ApplicationInfo.MaximumApplications - 1
            ApplicationInfo.day(i).Visible = False
            ApplicationInfo.mon(i).Visible = False
            ApplicationInfo.rate(i).Visible = False
            ApplicationInfo.MethodPanels(i).Visible = False
            '    ApplicationInfo.spray(i).Visible = False
            ApplicationInfo.RelApp(i).Visible = False
            ApplicationInfo.DepthIncorp(i).Visible = False
            '  ApplicationInfo.Eff(i).Visible = False
            ApplicationInfo.tBand(i).Visible = False
        Next
        UpdateApplicationChecks()
        AdjustApplicationDates()
        AdjustSprayAndEfficiency()

    End Sub
    Private Sub updateHoriz_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        updatehorizonsub()
    End Sub
    Sub updatehorizonsub()

        Dim horizonNumber As Integer

        Try
            horizonNumber = numHoriz.Text
        Catch ex As Exception
            MsgBox("Horizon Number must be an integer")
            horizonNumber = 1
        End Try

        'Reset TextBox: convert real numbers to integers:
        numHoriz.Text = horizonNumber

        If horizonNumber > SoilProperty.maxSoilLayers Then
            horizonNumber = SoilProperty.maxSoilLayers
            MsgBox("Horizon Number maximum is " & SoilProperty.maxSoilLayers)
            numHoriz.Text = SoilProperty.maxSoilLayers
        End If

        If horizonNumber < 1 Then
            horizonNumber = 1
            MsgBox("Horizon Number doesnt make sense.  Need at least 1 horizon")
            appNumber.Text = 1
        End If

        For i As Integer = 0 To horizonNumber - 1
            SoilProperty.thick(i).Visible = True
            SoilProperty.bulkden(i).Visible = True
            SoilProperty.oc(i).Visible = True
            SoilProperty.maxcap(i).Visible = True
            SoilProperty.mincap(i).Visible = True
            SoilProperty.compartment(i).Visible = True

            If simTemperature.Checked = True Then
                SoilProperty.sand(i).Visible = True
                SoilProperty.clay(i).Visible = True
            End If
        Next

        For i As Integer = horizonNumber To SoilProperty.maxSoilLayers - 1
            SoilProperty.thick(i).Visible = False
            SoilProperty.bulkden(i).Visible = False
            SoilProperty.oc(i).Visible = False
            SoilProperty.maxcap(i).Visible = False
            SoilProperty.mincap(i).Visible = False
            SoilProperty.sand(i).Visible = False
            SoilProperty.clay(i).Visible = False
            SoilProperty.compartment(i).Visible = False
        Next
    End Sub

    Private Function WaterBodyInfo() As String
        Dim msg As String


        Dim fullweatherfile As String
        If UseWeatherDirecory.Checked Then
            fullweatherfile = WeatherDirectoryBox.Text & weatherBox.Text
        Else
            fullweatherfile = weatherBox.Text

        End If





        msg = String.Format("{0}", scenarioID.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, fullweatherfile)

        msg = msg & vbNewLine & ScenarioLatBox.Text
        'If ReservoirPond.Checked Then
        '    msg = msg & String.Format("{0}0", vbNewLine)
        'ElseIf ReservoirOnly.Checked Then
        '    msg = msg & String.Format("{0}4", vbNewLine)
        'ElseIf PondOnly.Checked Then
        '    msg = msg & String.Format("{0}5", vbNewLine)
        'ElseIf RervoirWithUserAvg.Checked Then
        '    msg = msg & String.Format("{0}6,{1}", vbNewLine, ReservoirFlowAvgDays.Text)
        'ElseIf VVWM.Checked Then
        '    msg = msg & String.Format("{0}1", vbNewLine)
        'ElseIf VVWMnoFlow.Checked Then
        '    msg = msg & String.Format("{0}2", vbNewLine)
        'ElseIf VVWMflow.Checked Then
        '    msg = msg & String.Format("{0}3", vbNewLine)
        'End If


        'line 32 of transfer file
        msg = msg & String.Format("{0}{1},{2},{3},{4},{5},{6},{7}", vbNewLine, EPAreservoir.Checked, EPApond.Checked, VaryVolFlow.Checked, ConstVolNoFlow.Checked, ConstVolFlow.Checked, GroundWater.Checked, PRZMonly.Checked)

        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, UserSpecifiedFlowAvg.Checked, ReservoirFlowAvgDays.Text, CustomFlowAvgDays.Text)


        msg = msg & vbNewLine & BurialButton.Checked  'line 34 of transfer file


        msg = msg & String.Format("{0}{1}", vbNewLine, fieldAreaBox.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, waterAreaBox.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, initialDepthBox.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, maxDepthBox.Text)

        msg = msg & String.Format("{0}{1}", vbNewLine, massXferBox.Text)
        msg = msg & String.Format("{0}{1},{2}", vbNewLine, calculate_prben.Checked, prbenBox.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, benthicdepthBox.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, porosityBox.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, bdBox.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, foc2Box.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, DOC2Box.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, biomass2Box.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, dfacBox.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, ssBox.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, ChlorophyllBox.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, foc1Box.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, DOC1Box.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, Biomass1Box.Text)
        msg = msg & vbNewLine & EpaDefaultsCheck.Checked                                                            'line 53
        msg = msg & String.Format("{0}{1},{2}", vbNewLine, ReservoirCroppedAreaBox.Text, CustomCroppedAreaBox.Text) 'Line 54
        msg = msg & vbNewLine

        Return msg
    End Function

    Private Function PRZMinfo()

        'Routine for pre-verdsion 1.55
        'returns msg
        Dim msg As String

        msg = String.Format("{0}", dayEmerge1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, monthEmerge1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, dayMature1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, monthMature1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, dayHarvest1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, monthHarvest1.Text)

        msg = msg & String.Format("{0}{1},{2}", vbNewLine, rootDepth1.Text, canopyHeight1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, canopyCover1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, canopyHoldup1.Text)

        If mb11.Checked Then 'post harvest foliage
            msg = msg & vbNewLine & "1"
        End If
        If mb12.Checked Then
            msg = msg & vbNewLine & "2"
        End If
        If mb13.Checked Then
            msg = msg & vbNewLine & "3"
        End If

        msg = msg & String.Format("{0}{1}", vbNewLine, pfac.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, snowmelt.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, evapDepth.Text)

        If noIrrigation.Checked Then
            msg = msg & String.Format("{0}0", vbNewLine)
        End If
        If overCanopy.Checked Then
            msg = msg & String.Format("{0}1", vbNewLine)
        End If
        If underCanopy.Checked Then
            msg = msg & String.Format("{0}2", vbNewLine)
        End If

        msg = msg & String.Format("{0}{1}", vbNewLine, fleach.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, depletion.Text)
        msg = msg & String.Format("{0}{1},{2},{3}", vbNewLine, rateIrrig.Text, UserSpecifiesIrrigDepth.Checked, IrrigationDepthUserSpec.Text)

        msg = msg & String.Format("{0}{1}", vbNewLine, uslek.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, uslels.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, uslep.Text)

        msg = msg & String.Format("{0}{1}", vbNewLine, ireg.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, slope.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, hl.Text)

        msg = msg & String.Format("{0}{1}{0}", vbNewLine, numHoriz.Text)


        updatehorizonsub()
        For i As Integer = 0 To numHoriz.Text - 1
            msg = msg & String.Format("{0},", SoilProperty.thick(i).Text)
        Next
        msg = msg & vbNewLine
        For i As Integer = 0 To numHoriz.Text - 1
            msg = msg & String.Format("{0},", SoilProperty.bulkden(i).Text)
        Next
        msg = msg & vbNewLine
        For i As Integer = 0 To numHoriz.Text - 1
            msg = msg & String.Format("{0},", SoilProperty.maxcap(i).Text)
        Next
        msg = msg & vbNewLine
        For i As Integer = 0 To numHoriz.Text - 1
            msg = msg & String.Format("{0},", SoilProperty.mincap(i).Text)
        Next
        msg = msg & vbNewLine
        For i As Integer = 0 To numHoriz.Text - 1
            msg = msg & String.Format("{0},", SoilProperty.oc(i).Text)
        Next
        msg = msg & vbNewLine
        For i As Integer = 0 To numHoriz.Text - 1
            msg = msg & String.Format("{0},", SoilProperty.compartment(i).Text)
        Next
        msg = msg & vbNewLine
        For i As Integer = 0 To numHoriz.Text - 1
            msg = msg & String.Format("{0},", SoilProperty.sand(i).Text)
        Next
        msg = msg & vbNewLine
        For i As Integer = 0 To numHoriz.Text - 1
            msg = msg & String.Format("{0},", SoilProperty.clay(i).Text)
        Next




        msg = msg & String.Format("{0}{1}", vbNewLine, albedoBox.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, bcTemp.Text)

        msg = msg & vbNewLine & simTemperature.Checked

        msg = msg & vbNewLine & NumberOfFactors.Text


        msg = msg & vbNewLine
        For i As Integer = 0 To USLE.MaxHydroErosionFactors - 1
            msg = msg & USLE.day(i).Text & ","
        Next

        msg = msg & vbNewLine

        For i As Integer = 0 To USLE.MaxHydroErosionFactors - 1
            msg = msg & USLE.mon(i).Text & ","
        Next

        msg = msg & vbNewLine
        For i As Integer = 0 To USLE.MaxHydroErosionFactors - 1
            msg = msg & USLE.cn(i).Text & ","
        Next

        msg = msg & vbNewLine
        For i As Integer = 0 To USLE.MaxHydroErosionFactors - 1
            msg = msg & USLE.C(i).Text & ","
        Next

        msg = msg & vbNewLine
        For i As Integer = 0 To USLE.MaxHydroErosionFactors - 1
            msg = msg & USLE.n(i).Text & ","
        Next

        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, rDepthBox.Text, rDeclineBox.Text, rBypassBox.Text)
        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, eDepthBox.Text, eDeclineBox.Text, eBypassBox.Text)


        msg = msg & vbNewLine & useUSLEYear.Checked & vbNewLine
        For i As Integer = 0 To USLE.MaxHydroErosionFactors - 1
            msg = msg & USLE.year(i).Text & ","
        Next

        msg = msg & vbNewLine & volatilizationBoundaryBox.Text

        Return msg
    End Function
    Private Function ChemInfo() As String
        Dim msg As String
        Dim nchem As Integer

        If Deg2CheckBox.Checked Then
            nchem = 3
        ElseIf Deg1CheckBox.Checked Then
            nchem = 2
        Else
            nchem = 1
        End If
        msg = chemID.Text
        msg = msg & String.Format("{0}{1}", vbNewLine, nchem)                             'line 3 tranfer file
        msg = msg & String.Format("{0}{1}", vbNewLine, IsKocRadioButton.Checked)
        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, KocBox1.Text, KocBox2.Text, KocBox3.Text)
        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, waterMetabBox1.Text, waterMetabBox2.Text, waterMetabBox3.Text)
        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, waterTempBox1.Text, waterTempBox2.Text, waterTempBox3.Text)
        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, benthicMetabBox1.Text, benthicMetabBox2.Text, benthicMetabBox3.Text)
        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, benthicTempBox1.Text, benthicTempBox2.Text, benthicTempBox3.Text)
        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, photoBox1.Text, photoBox2.Text, photoBox3.Text)
        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, rlatBox1.Text, rlatBox2.Text, rlatBox3.Text)
        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, hydroBox1.Text, hydroBox2.Text, hydroBox3.Text)
        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, soilDegradation1.Text, soilDegradation2.Text, soilDegradation3.Text)
        msg = msg & String.Format("{0}{1},", vbNewLine, soilTempBox1.Text)
        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, foliarDeg1.Text, foliarDeg2.Text, foliarDeg3.Text)
        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, mwtBox1.Text, mwtBox2.Text, mwtBox3.Text)  'Line 16
        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, vpBox1.Text, vpBox2.Text, vpBox3.Text)
        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, solBox1.Text, solBox2.Text, solBox3.Text)  'Line 18

        msg = msg & String.Format("{0}{1},{2},", vbNewLine, convertWater1.Text, convertWater2.Text)
        msg = msg & String.Format("{0}{1},{2},", vbNewLine, convertBenthic1.Text, convertBenthic2.Text)
        msg = msg & String.Format("{0}{1},{2},", vbNewLine, convertPhoto1.Text, convertPhoto2.Text)
        msg = msg & String.Format("{0}{1},{2},", vbNewLine, convertHydro1.Text, convertHydro2.Text)
        msg = msg & String.Format("{0}{1},{2},", vbNewLine, convertSoil1.Text, convertSoil2.Text)
        msg = msg & String.Format("{0}{1},{2},", vbNewLine, convertFoliar1.Text, convertFoliar2.Text) ' Line 24

        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, airDiffusion1.Text, airDiffusion2.Text, airDiffusion3.Text)  'Line 25
        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, henry1.Text, henry2.Text, henry3.Text)
        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, enthalpyGas1.Text, enthalpyGas2.Text, enthalpyGas3.Text)  'Line 27
        msg = msg & String.Format("{0}{1}", vbNewLine, Q10Box.Text)

        Return msg
    End Function

    Private Sub getChemInfo(ByVal ofile As Microsoft.VisualBasic.FileIO.TextFieldParser, ByVal inputfileversion As Single)
        Dim currentRow As String()
        Dim nchem As Integer


        chemID.Text = ofile.ReadLine()




        nchem = ofile.ReadLine()
        'nchem = ofile.ReadLine()  'This contains the nchem used for VVWM info

        Select Case nchem
            Case 2
                Deg1CheckBox.Checked = True
                Deg2CheckBox.Checked = False
            Case 3
                Deg1CheckBox.Checked = True
                Deg2CheckBox.Checked = True
            Case Else
                Deg1CheckBox.Checked = False
                Deg2CheckBox.Checked = False
        End Select


        If ofile.ReadLine() Then
            IsKocRadioButton.Checked = True
        Else
            IsKdRadioButton.Checked = True
        End If
        currentRow = ofile.ReadFields
        KocBox1.Text = currentRow(0)
        KocBox2.Text = currentRow(1)
        KocBox3.Text = currentRow(2)



        currentRow = ofile.ReadFields
        waterMetabBox1.Text = currentRow(0)
        waterMetabBox2.Text = currentRow(1)
        waterMetabBox3.Text = currentRow(2)

        currentRow = ofile.ReadFields
        waterTempBox1.Text = currentRow(0)
        waterTempBox2.Text = currentRow(1)
        waterTempBox3.Text = currentRow(2)

        currentRow = ofile.ReadFields
        benthicMetabBox1.Text = currentRow(0)
        benthicMetabBox2.Text = currentRow(1)
        benthicMetabBox3.Text = currentRow(2)

        currentRow = ofile.ReadFields
        benthicTempBox1.Text = currentRow(0)
        benthicTempBox2.Text = currentRow(1)
        benthicTempBox3.Text = currentRow(2)

        currentRow = ofile.ReadFields
        photoBox1.Text = currentRow(0)
        photoBox2.Text = currentRow(1)
        photoBox3.Text = currentRow(2)

        currentRow = ofile.ReadFields
        rlatBox1.Text = currentRow(0)
        rlatBox2.Text = currentRow(1)
        rlatBox3.Text = currentRow(2)

        currentRow = ofile.ReadFields
        hydroBox1.Text = currentRow(0)
        hydroBox2.Text = currentRow(1)
        hydroBox3.Text = currentRow(2)

        currentRow = ofile.ReadFields
        soilDegradation1.Text = currentRow(0)
        soilDegradation2.Text = currentRow(1)
        soilDegradation3.Text = currentRow(2)

        currentRow = ofile.ReadFields
        soilTempBox1.Text = currentRow(0)
        'soilTempBox2.Text = currentRow(1)
        'soilTempBox3.Text = currentRow(2)

        currentRow = ofile.ReadFields
        foliarDeg1.Text = currentRow(0)
        foliarDeg2.Text = currentRow(1)
        foliarDeg3.Text = currentRow(2)

        currentRow = ofile.ReadFields
        mwtBox1.Text = currentRow(0)
        mwtBox2.Text = currentRow(1)
        mwtBox3.Text = currentRow(2)

        currentRow = ofile.ReadFields
        vpBox1.Text = currentRow(0)
        vpBox2.Text = currentRow(1)
        vpBox3.Text = currentRow(2)

        currentRow = ofile.ReadFields
        solBox1.Text = currentRow(0)
        solBox2.Text = currentRow(1)
        solBox3.Text = currentRow(2)

        currentRow = ofile.ReadFields
        convertWater1.Text = currentRow(0)
        convertWater2.Text = currentRow(1)

        currentRow = ofile.ReadFields
        convertBenthic1.Text = currentRow(0)
        convertBenthic2.Text = currentRow(1)

        currentRow = ofile.ReadFields
        convertPhoto1.Text = currentRow(0)
        convertPhoto2.Text = currentRow(1)

        currentRow = ofile.ReadFields
        convertHydro1.Text = currentRow(0)
        convertHydro2.Text = currentRow(1)

        currentRow = ofile.ReadFields
        convertSoil1.Text = currentRow(0)
        convertSoil2.Text = currentRow(1)

        currentRow = ofile.ReadFields
        convertFoliar1.Text = currentRow(0)
        convertFoliar2.Text = currentRow(1)


        If inputfileversion > 1.1 Then

            currentRow = ofile.ReadFields
            airDiffusion1.Text = currentRow(0)
            airDiffusion2.Text = currentRow(1)
            airDiffusion3.Text = currentRow(2)

            currentRow = ofile.ReadFields
            henry1.Text = currentRow(0)
            henry2.Text = currentRow(1)
            henry3.Text = currentRow(2)

            currentRow = ofile.ReadFields
            enthalpyGas1.Text = currentRow(0)
            enthalpyGas2.Text = currentRow(1)
            enthalpyGas3.Text = currentRow(2)
        Else
            ofile.ReadLine()
            ofile.ReadLine()
            ofile.ReadLine()
        End If


        Q10Box.Text = ofile.ReadLine()


    End Sub
    Private Sub getAppInfo(ByVal ofile As Microsoft.VisualBasic.FileIO.TextFieldParser)
        Dim currentRow As String()

        appNumber.Text = ofile.ReadLine()

        currentRow = ofile.ReadFields
        For i As Integer = 0 To appNumber.Text - 1
            ApplicationInfo.day(i).Text = currentRow(i)
        Next

        currentRow = ofile.ReadFields
        For i As Integer = 0 To appNumber.Text - 1
            ApplicationInfo.mon(i).Text = currentRow(i)
        Next

        currentRow = ofile.ReadFields
        For i As Integer = 0 To appNumber.Text - 1
            ApplicationInfo.rate(i).Text = currentRow(i)
        Next

        SpecifyYears.Checked = ofile.ReadLine


        currentRow = ofile.ReadFields
        For i As Integer = 0 To appNumber.Text - 1
            ApplicationInfo.year(i).Text = currentRow(i)
        Next



        currentRow = ofile.ReadFields


        For i As Integer = 0 To appNumber.Text - 1

            If Not IsNumeric(currentRow(i)) Then Exit For 'Check to see if an application is specified, otherwise an error is thrown



            If currentRow(i) = 1 Then
                ApplicationInfo.methodbutton(i * ApplicationInfo.NumberofApplicationTypes + 0).Checked = True
            ElseIf currentRow(i) = 2 Then
                ApplicationInfo.methodbutton(i * ApplicationInfo.NumberofApplicationTypes + 1).Checked = True
            ElseIf currentRow(i) = 3 Then
                ApplicationInfo.methodbutton(i * ApplicationInfo.NumberofApplicationTypes + 2).Checked = True
            ElseIf currentRow(i) = 4 Then
                ApplicationInfo.methodbutton(i * ApplicationInfo.NumberofApplicationTypes + 3).Checked = True
            ElseIf currentRow(i) = 5 Then
                ApplicationInfo.methodbutton(i * ApplicationInfo.NumberofApplicationTypes + 4).Checked = True
            ElseIf currentRow(i) = 6 Then
                ApplicationInfo.methodbutton(i * ApplicationInfo.NumberofApplicationTypes + 5).Checked = True
            Else
                ApplicationInfo.methodbutton(i * ApplicationInfo.NumberofApplicationTypes + 6).Checked = True
            End If
        Next


        '***********************************************************


        '****** Read in Custom Values *******
        currentRow = ofile.ReadFields
        For i As Integer = 0 To appNumber.Text - 1
            ApplicationInfo.Eff(i).Text = currentRow(i)
        Next

        currentRow = ofile.ReadFields
        For i As Integer = 0 To appNumber.Text - 1
            ApplicationInfo.spray(i).Text = currentRow(i)
        Next

        '******* Read in Pond Values
        currentRow = ofile.ReadFields
        For i As Integer = 0 To appNumber.Text - 1
            ApplicationInfo.Effp(i).Text = currentRow(i)
        Next

        currentRow = ofile.ReadFields
        For i As Integer = 0 To appNumber.Text - 1
            ApplicationInfo.sprayp(i).Text = currentRow(i)
        Next

        '******* Read in Reservoir Values
        currentRow = ofile.ReadFields
        For i As Integer = 0 To appNumber.Text - 1
            ApplicationInfo.Effr(i).Text = currentRow(i)
        Next

        currentRow = ofile.ReadFields
        For i As Integer = 0 To appNumber.Text - 1
            ApplicationInfo.sprayr(i).Text = currentRow(i)
        Next
        '***********************************************************

        currentRow = ofile.ReadFields
        For i As Integer = 0 To appNumber.Text - 1
            ApplicationInfo.DepthIncorp(i).Text = currentRow(i)
        Next

        currentRow = ofile.ReadFields
        For i As Integer = 0 To appNumber.Text - 1
            ApplicationInfo.RelApp(i).Text = currentRow(i)
        Next


        currentRow = ofile.ReadFields
        If currentRow(0) Then
            AppsAbsolute.Checked = True
            AppsRelative.Checked = False
        Else
            AppsAbsolute.Checked = False
            AppsRelative.Checked = True
        End If

        Try
            Select Case currentRow(1)
                Case 1
                    RelToEmerge.Checked = True
                Case 2
                    RelToMat.Checked = True
                Case Else
                    RelToHarv.Checked = True
            End Select






        Catch ex As Exception

        End Try




        currentRow = ofile.ReadFields
        For i As Integer = 0 To appNumber.Text - 1
            ApplicationInfo.tBand(i).Text = currentRow(i)
        Next


        ofile.ReadLine()
        ofile.ReadLine()
        ofile.ReadLine()

    End Sub

    Private Sub getWaterBodyInfo2(ByVal ofile As Microsoft.VisualBasic.FileIO.TextFieldParser)
        Dim currentrow As String()

        Try

            scenarioID.Text = ofile.ReadLine()
            weatherBox.Text = ofile.ReadLine()
            ScenarioLatBox.Text = ofile.ReadLine()

            currentrow = ofile.ReadFields


            If BatchRun.Checked = False Then
                '**************************************************
                'During a batch scenario run these will be bypassed 
                'whatever state the interface is in  will be kept for the run
                '**************************************************
                EPAreservoir.Checked = currentrow(0)
                EPApond.Checked = currentrow(1)

                VaryVolFlow.Checked = currentrow(2)
                ConstVolNoFlow.Checked = currentrow(3)
                ConstVolFlow.Checked = currentrow(4)

                Try  'Older version will not have this checked dsate

                    GroundWater.Checked = currentrow(5)
                    PRZMonly.Checked = currentrow(6)
                Catch ex As Exception

                End Try

            End If



            currentrow = ofile.ReadFields 'place for averaging time
            If BatchRun.Checked = False Then
                '**************************************************
                'During a batch scenario run these will be bypassed 
                'whatever state the interface is in  will be kept for the run
                '**************************************************
                UserSpecifiedFlowAvg.Checked = currentrow(0)
                ReservoirFlowAvgDays.Text = currentrow(1)
            End If


            CustomFlowAvgDays.Text = currentrow(2)


            If (ofile.ReadLine()) Then
                BurialButton.Checked = True
            Else
                noBurialButton.Checked = True
            End If

            fieldAreaBox.Text = ofile.ReadLine()
            waterAreaBox.Text = ofile.ReadLine()
            initialDepthBox.Text = ofile.ReadLine()
            maxDepthBox.Text = ofile.ReadLine()
            massXferBox.Text = ofile.ReadLine()



            currentrow = ofile.ReadFields
            calculate_prben.Checked = currentrow(0)

            Const_prben.Checked = Not calculate_prben.Checked
            prbenBox.Text = currentrow(1)


            benthicdepthBox.Text = ofile.ReadLine()
            porosityBox.Text = ofile.ReadLine()
            bdBox.Text = ofile.ReadLine()
            foc2Box.Text = ofile.ReadLine()
            DOC2Box.Text = ofile.ReadLine()
            biomass2Box.Text = ofile.ReadLine()
            dfacBox.Text = ofile.ReadLine()
            ssBox.Text = ofile.ReadLine()


            ChlorophyllBox.Text = ofile.ReadLine()
            foc1Box.Text = ofile.ReadLine()
            DOC1Box.Text = ofile.ReadLine()
            Biomass1Box.Text = ofile.ReadLine()


            EpaDefaultsCheck.Checked = ofile.ReadLine()

            Dim thisrow() As String
            'new feature may not be present in older input files
            thisrow = Split(ofile.ReadLine(), ",")
            If thisrow.Length > 1 Then
                ReservoirCroppedAreaBox.Text = thisrow(0)
                CustomCroppedAreaBox.Text = thisrow(1)
            End If

            ofile.ReadLine()
        Catch ex As Exception
            MsgBox(ex.Message & "Error in reading Water Body info.")
        End Try

    End Sub

    Private Sub getWaterBodyInfo(ByVal ofile As Microsoft.VisualBasic.FileIO.TextFieldParser)
        Dim currentrow As String()

        Try

            scenarioID.Text = ofile.ReadLine()
            weatherBox.Text = ofile.ReadLine()


            ofile.ReadLine()
            'currentrow = ofile.ReadFields
            'Select Case currentrow(0)
            '    Case "0"
            '        ReservoirPond.Checked = True
            '    Case "1"
            '        VVWM.Checked = True
            '    Case "2"
            '        VVWMnoFlow.Checked = True
            '    Case "3"
            '        VVWMflow.Checked = True
            '    Case "4"
            '        ReservoirOnly.Checked = True
            '    Case "5"
            '        PondOnly.Checked = True
            '    Case "6"
            '        RervoirWithUserAvg.Checked = True
            '        ReservoirFlowAvgDays.Text = currentrow(1)
            'End Select


            currentrow = ofile.ReadFields


            If BatchRun.Checked = False Then
                '**************************************************
                'During a batch scenario run these will be bypassed 
                'whatever state the interface is in  will be kept for the run
                '**************************************************
                EPAreservoir.Checked = currentrow(0)
                EPApond.Checked = currentrow(1)

                VaryVolFlow.Checked = currentrow(2)
                ConstVolNoFlow.Checked = currentrow(3)
                ConstVolFlow.Checked = currentrow(4)

                Try  'Older version will not have this checked dsate

                    GroundWater.Checked = currentrow(5)
                    PRZMonly.Checked = currentrow(6)
                Catch ex As Exception

                End Try

            End If



            currentrow = ofile.ReadFields 'place for averaging time
            If BatchRun.Checked = False Then
                '**************************************************
                'During a batch scenario run these will be bypassed 
                'whatever state the interface is in  will be kept for the run
                '**************************************************
                UserSpecifiedFlowAvg.Checked = currentrow(0)
                ReservoirFlowAvgDays.Text = currentrow(1)
            End If

            Try
                'Older versions will not have this
                CustomFlowAvgDays.Text = currentrow(2)
            Catch ex As Exception

            End Try


            If (ofile.ReadLine()) Then
                BurialButton.Checked = True
            Else
                noBurialButton.Checked = True
            End If




            fieldAreaBox.Text = ofile.ReadLine()
            waterAreaBox.Text = ofile.ReadLine()
            initialDepthBox.Text = ofile.ReadLine()
            maxDepthBox.Text = ofile.ReadLine()
            massXferBox.Text = ofile.ReadLine()

            'new version has additional info on this line
            prbenBox.Text = ofile.ReadLine()


            benthicdepthBox.Text = ofile.ReadLine()
            porosityBox.Text = ofile.ReadLine()
            bdBox.Text = ofile.ReadLine()
            foc2Box.Text = ofile.ReadLine()
            DOC2Box.Text = ofile.ReadLine()
            biomass2Box.Text = ofile.ReadLine()
            dfacBox.Text = ofile.ReadLine()
            ssBox.Text = ofile.ReadLine()
            ChlorophyllBox.Text = ofile.ReadLine()
            foc1Box.Text = ofile.ReadLine()
            DOC1Box.Text = ofile.ReadLine()
            Biomass1Box.Text = ofile.ReadLine()


            EpaDefaultsCheck.Checked = ofile.ReadLine()

            Dim thisrow() As String
            'new feature may not be present in older input files
            thisrow = Split(ofile.ReadLine(), ",")
            If thisrow.Length > 1 Then
                ReservoirCroppedAreaBox.Text = thisrow(0)
                CustomCroppedAreaBox.Text = thisrow(1)
            End If

            ofile.ReadLine()
        Catch ex As Exception
            MsgBox(ex.Message & "Error in reading Water Body info.")
        End Try

    End Sub

    Private Sub getPRZMinfo(ByVal ofile As Microsoft.VisualBasic.FileIO.TextFieldParser)
        'This routine is for reading in the old input files
        Dim test As Integer
        Dim currentRow As String()

        Try
            dayEmerge1.Text = ofile.ReadLine()
            monthEmerge1.Text = ofile.ReadLine()
            dayMature1.Text = ofile.ReadLine()
            monthMature1.Text = ofile.ReadLine()
            dayHarvest1.Text = ofile.ReadLine()
            monthHarvest1.Text = ofile.ReadLine()

            currentRow = ofile.ReadFields
            rootDepth1.Text = currentRow(0)
            canopyHeight1.Text = currentRow(1)

            canopyCover1.Text = ofile.ReadLine()
            canopyHoldup1.Text = ofile.ReadLine()


        Catch ex As Exception
            MsgBox("Could be that you are trying to load an old input file. Check those crop inputs")
            'MsgBox("ther be chans dat da input crops fils yu red be ol an it be gudder if yu chek dem crop inputs")
        End Try



        Try

            test = ofile.ReadLine()

            Select Case test
                Case 1
                    mb11.Checked = True
                Case 2
                    mb12.Checked = True
                Case 3
                    mb13.Checked = True
            End Select

            pfac.Text = ofile.ReadLine()
            snowmelt.Text = ofile.ReadLine()
            evapDepth.Text = ofile.ReadLine()

            test = ofile.ReadLine()
            Select Case test
                Case 0
                    noIrrigation.Checked = True
                Case 1
                    overCanopy.Checked = True
                Case 2
                    underCanopy.Checked = True
            End Select



            fleach.Text = ofile.ReadLine()
            depletion.Text = ofile.ReadLine()



            Dim teststring As String
            teststring = ofile.PeekChars(10)
            'Old version had no commas at this point
            If teststring.Contains(",") Then
                currentRow = ofile.ReadFields
                rateIrrig.Text = currentRow(0)
                UserSpecifiesIrrigDepth.Checked = currentRow(1)
                IrrigDepthRootZone.Checked = Not Convert.ToBoolean(currentRow(1))
                IrrigationDepthUserSpec.Text = currentRow(2)
            Else
                rateIrrig.Text = ofile.ReadLine()
            End If

            'NEW PWC uses daily irrigation, old version used hourly, so convert here
            If Not noIrrigation.Checked Then
                rateIrrig.Text = rateIrrig.Text * 24
            End If

        Catch ex As Exception
            MsgBox("The irrigation and evaporation inputs might be strange.")
            'MsgBox("maybe dem earugashun an stuff lik evaprashun not reel gudd enuf to wurk")
        End Try


        Try
            uslek.Text = ofile.ReadLine()
            uslels.Text = ofile.ReadLine()
            uslep.Text = ofile.ReadLine()

            ireg.Text = ofile.ReadLine()
            slope.Text = ofile.ReadLine()
            hl.Text = ofile.ReadLine()

            numHoriz.Text = ofile.ReadLine()
            updatehorizonsub()


            currentRow = ofile.ReadFields
            For i As Integer = 0 To numHoriz.Text - 1
                SoilProperty.thick(i).Text = currentRow(i)
            Next
            currentRow = ofile.ReadFields
            For i As Integer = 0 To numHoriz.Text - 1
                SoilProperty.bulkden(i).Text = currentRow(i)
            Next
            currentRow = ofile.ReadFields
            For i As Integer = 0 To numHoriz.Text - 1
                SoilProperty.maxcap(i).Text = currentRow(i)
            Next
            currentRow = ofile.ReadFields
            For i As Integer = 0 To numHoriz.Text - 1
                SoilProperty.mincap(i).Text = currentRow(i)
            Next
            currentRow = ofile.ReadFields
            For i As Integer = 0 To numHoriz.Text - 1
                SoilProperty.oc(i).Text = currentRow(i)
            Next
            currentRow = ofile.ReadFields
            For i As Integer = 0 To numHoriz.Text - 1
                SoilProperty.compartment(i).Text = currentRow(i)
            Next
            currentRow = ofile.ReadFields
            For i As Integer = 0 To numHoriz.Text - 1
                SoilProperty.sand(i).Text = currentRow(i)
            Next

            currentRow = ofile.ReadFields
            For i As Integer = 0 To numHoriz.Text - 1
                SoilProperty.clay(i).Text = currentRow(i)
            Next

            albedoBox.Text = ofile.ReadLine()
            bcTemp.Text = ofile.ReadLine()

            Try
                Dim tempsim As String
                tempsim = ofile.ReadLine()
                simTemperature.Checked = Convert.ToBoolean(tempsim)
            Catch ex As Exception
                MsgBox(ex.Message)
            End Try


            '****** READ IN USLE and CN Values ************************

            'First Clear all the USLE/CN numbers from previous run
            For i As Integer = 0 To USLE.MaxHydroErosionFactors - 1
                USLE.day(i).Text = ""
                USLE.mon(i).Text = ""
                USLE.cn(i).Text = ""
                USLE.C(i).Text = ""
                USLE.n(i).Text = ""
                USLE.year(i).Text = ""
            Next

            NumberOfFactors.Text = ofile.ReadLine()

            'Added this definition and conditions to avoid failures when developing scenarios that are still incomplete
            Dim NumberOfFactorsInteger As Integer
            If IsNumeric(NumberOfFactors.Text) Then
                NumberOfFactorsInteger = NumberOfFactors.Text - 1
                If NumberOfFactorsInteger > USLE.MaxHydroErosionFactors - 1 Then
                    NumberOfFactorsInteger = USLE.MaxHydroErosionFactors - 1
                    NumberOfFactors.Text = USLE.MaxHydroErosionFactors
                End If

            Else
                NumberOfFactorsInteger = -1
            End If


            currentRow = ofile.ReadFields
            For i As Integer = 0 To NumberOfFactorsInteger
                USLE.day(i).Text = currentRow(i)
            Next

            currentRow = ofile.ReadFields
            For i As Integer = 0 To NumberOfFactorsInteger
                USLE.mon(i).Text = currentRow(i)
            Next

            currentRow = ofile.ReadFields
            For i As Integer = 0 To NumberOfFactorsInteger
                USLE.cn(i).Text = currentRow(i)
            Next

            currentRow = ofile.ReadFields
            For i As Integer = 0 To NumberOfFactorsInteger
                USLE.C(i).Text = currentRow(i)
            Next

            currentRow = ofile.ReadFields
            For i As Integer = 0 To NumberOfFactorsInteger
                USLE.n(i).Text = currentRow(i)
            Next


            '******************************************************************************


            '******************************************************************************
            currentRow = ofile.ReadFields


            rDepthBox.Text = currentRow(0)
            rDeclineBox.Text = currentRow(1)
            rBypassBox.Text = currentRow(2)

            currentRow = ofile.ReadFields
            eDepthBox.Text = currentRow(0)
            eDeclineBox.Text = currentRow(1)
            If currentRow(2).Trim = "" Then
                eBypassBox.Text = "1.0"
            Else
                eBypassBox.Text = currentRow(2)
            End If


            useUSLEYear.Checked = ofile.ReadLine()
            currentRow = ofile.ReadFields
            For i As Integer = 0 To NumberOfFactorsInteger
                USLE.year(i).Text = currentRow(i)
            Next



            If Trim(ofile.PeekChars(1)) IsNot "" Then  ' backward cpomaptible version
                volatilizationBoundaryBox.Text = ofile.ReadLine()
            Else
                volatilizationBoundaryBox.Text = "5.0"  'default boundary thickness
            End If



        Catch ex As Exception
            MsgBox(ex.Message & " Problem in reading USLE variable or runoff parameters of scenario")
        End Try




    End Sub

    Private Sub SaveToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SaveToolStripMenuItem.Click
        Dim msg As String
        Dim result As System.Windows.Forms.DialogResult

        SaveFileDialog1.Filter = "PWC INPUT Files (*.SWI)|*.SWI|PWC INPUT Files (*.PWC)|*.PWC|ALL Files (*.*)|*.*"
        SaveFileDialog1.FilterIndex = 2

        SaveFileDialog1.InitialDirectory = FileNameClass.WorkingDirectory
        SaveFileDialog1.FileName = ""
        result = SaveFileDialog1.ShowDialog(Me)


        'Cancel button will cuase return without further execution
        If result = Windows.Forms.DialogResult.Cancel Then
            Return
        End If

        Dim extension As String
        extension = System.IO.Path.GetExtension(SaveFileDialog1.FileName).ToUpper


        FileNameClass.WorkingDirectory = System.IO.Path.GetDirectoryName(SaveFileDialog1.FileName) & "\"
        workingDirectoryLabel.Text = FileNameClass.WorkingDirectory


        'FamilyFileName = System.IO.Path.GetFileNameWithoutExtension(SaveFileDialog1.FileName)
        ''      outputfilenameBox.Text = FamilyFileName & "VVWM.out"
        'ioFamilyNameBox.Text = FamilyFileName
        ioFamilyNameBox.Text = System.IO.Path.GetFileNameWithoutExtension(SaveFileDialog1.FileName)

        workingDirectoryLabel.ForeColor = Color.Black
        ioFamilyNameBox.ForeColor = Color.Black

        msg = StandardParameters.ProgramName & StandardParameters.VersionNumber
        msg = msg & vbNewLine & ChemInfo()
        msg = msg & vbNewLine & ApplicationInfo.AppInfoString()   'attach application schedule
        msg = msg & vbNewLine & WaterBodyInfo()


        If extension = ".PWC" Then
            msg = msg & vbNewLine & PRZMinfo_V2() 'attach scenario information
            msg = msg & vbNewLine & SaveBatchRunInfo()
        ElseIf extension = ".SWI" Then
            msg = msg & vbNewLine & PRZMinfo()
        End If





        SaveFileDialog1.InitialDirectory = workingDirectoryLabel.Text

        Try
            My.Computer.FileSystem.WriteAllText(SaveFileDialog1.FileName, msg, False, System.Text.Encoding.ASCII)
        Catch ex As Exception
            MsgBox(ex.Message)
        End Try
    End Sub


    Private Sub RetrieveToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RetrieveToolStripMenuItem.Click
        Dim result As System.Windows.Forms.DialogResult

        OpenFileDialog1.Filter = "SWCC INPUT Files (*.SWI)|*.SWI|PWC INPUT FILES (*.PWC)|*.PWC|ALL Files (*.*)|*.*"
        '  OpenFileDialog1.ShowDialog()
        OpenFileDialog1.FilterIndex = 2

        OpenFileDialog1.InitialDirectory = FileNameClass.WorkingDirectory
        OpenFileDialog1.FileName = ""

        result = OpenFileDialog1.ShowDialog(Me)

        'Cancel button will cause return without further execution
        If result = Windows.Forms.DialogResult.Cancel Then
            Return
        End If

        ' Dim ofile As New System.IO.StreamReader(OpenFileDialog1.FileName)

        ReadInputFile(OpenFileDialog1.FileName)

        updateApplications()

        UpdateApplicationChecks()

    End Sub

    Private Sub ReadInputFile(ByVal inputfilename As String)
        Using MyReader As New Microsoft.VisualBasic.FileIO.TextFieldParser(inputfilename)
            Dim currentrow As String()
            Dim inputFileVersion As Single
            Dim teststring As String

            workingDirectoryLabel.Text = System.IO.Path.GetDirectoryName(inputfilename) & "\"
            FileNameClass.WorkingDirectory = System.IO.Path.GetDirectoryName(inputfilename) & "\"


            'FamilyFileName = System.IO.Path.GetFileNameWithoutExtension(OpenFileDialog1.FileName)
            'ioFamilyNameBox.Text = FamilyFileName

            ioFamilyNameBox.Text = System.IO.Path.GetFileNameWithoutExtension(inputfilename)

            workingDirectoryLabel.ForeColor = Color.Black
            ioFamilyNameBox.ForeColor = Color.Black

            Dim extension As String
            extension = System.IO.Path.GetExtension(inputfilename)
            extension = extension.ToUpper()




            Try
                teststring = MyReader.PeekChars(100)
                If teststring.Contains("Version") Then
                    'new file has a header with version number
                    ' make it backward compatible
                    ' As long as the header has "version" in it the program will be able to get version number
                    MyReader.TextFieldType = FileIO.FieldType.Delimited
                    MyReader.SetDelimiters("Version")

                    currentrow = MyReader.ReadFields()
                    inputFileVersion = currentrow(1)
                Else
                    inputFileVersion = 0.0
                End If

                MyReader.TextFieldType = FileIO.FieldType.Delimited
                MyReader.SetDelimiters(",")




                getChemInfo(MyReader, inputFileVersion)


                getAppInfo(MyReader)


                If extension = ".SWI" Then
                    getWaterBodyInfo(MyReader)
                    getPRZMinfo(MyReader)

                ElseIf extension = ".PWC" Then
                    getWaterBodyInfo2(MyReader)
                    getPRZMinfo_2(MyReader)

                    getBatchInfo(MyReader)

                End If



            Catch ex As Exception
                MsgBox(ex.Message & "  Check input file")
            End Try
        End Using



    End Sub

    Private Sub ContactInfoToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ContactInfoToolStripMenuItem.Click
        MsgBox("Dirk Young" & vbNewLine & "USEPA" & vbNewLine & "young.dirk@epa.gov", , "Contact Information")
    End Sub

    Private Sub Deg1CheckBox_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Deg1CheckBox.CheckedChanged

        If Deg1CheckBox.Checked = True Then
            Deg1PropPanel.Visible = True
            Deg2CheckBox.Visible = True
            ConversionLabels.Visible = True

        Else
            Deg2CheckBox.Visible = False
            Deg1PropPanel.Visible = False
            Deg2CheckBox.Checked = False
            ConversionLabels.Visible = False
        End If
    End Sub
    Private Sub Deg2CheckBox_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Deg2CheckBox.CheckedChanged

        If Deg2CheckBox.Checked = True Then
            Deg2Panel.Visible = True
        Else
            Deg2Panel.Visible = False
        End If

    End Sub

    Private Sub AppsAbsolute_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles AppsAbsolute.CheckedChanged

        AdjustApplicationDates()

    End Sub

    Private Sub AdjustApplicationDates()
        If AppsAbsolute.Checked = True Then

            PanelEmMatHarv.Visible = False

            'daylabel.Visible = True
            'monthlabel.Visible = True
            'relativelabel.Visible = False
            relativelabel.Text = vbNewLine & "   Day Mon"


            For i As Integer = 0 To ApplicationInfo.MaximumApplications - 1
                ApplicationInfo.RelApp(i).Visible = False
            Next

            For i As Integer = 0 To appNumber.Text - 1
                ApplicationInfo.mon(i).Visible = True
                ApplicationInfo.day(i).Visible = True
            Next

            For i As Integer = appNumber.Text To ApplicationInfo.MaximumApplications - 1
                ApplicationInfo.mon(i).Visible = False
                ApplicationInfo.day(i).Visible = False
            Next

        Else
            PanelEmMatHarv.Visible = True

            'daylabel.Visible = False
            'monthlabel.Visible = False

            'relativelabel.Visible = True



            relativelabel.Text = "Days Since" & vbNewLine & ApplicationInfo.EmergenceMaturityHarvest


            For i As Integer = 0 To ApplicationInfo.MaximumApplications - 1
                ApplicationInfo.mon(i).Visible = False
                ApplicationInfo.day(i).Visible = False
            Next

            For i As Integer = 0 To appNumber.Text - 1
                ApplicationInfo.RelApp(i).Visible = True
                ApplicationInfo.year(i).Visible = True
            Next

            For i As Integer = appNumber.Text To ApplicationInfo.MaximumApplications - 1
                ApplicationInfo.RelApp(i).Visible = False
                ApplicationInfo.year(i).Visible = False
            Next

        End If


        If SpecifyYears.Checked Then

            yrlabel.Visible = True
            For i As Integer = 0 To appNumber.Text - 1
                ApplicationInfo.year(i).Visible = True
            Next

            For i As Integer = appNumber.Text To ApplicationInfo.MaximumApplications - 1
                ApplicationInfo.year(i).Visible = False
            Next
        Else
            yrlabel.Visible = False
            For i As Integer = 0 To ApplicationInfo.MaximumApplications - 1
                ApplicationInfo.year(i).Visible = False
            Next

        End If

    End Sub

    Private Sub GetBatchScenarios_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles GetBatchScenarios.Click
        Dim result As System.Windows.Forms.DialogResult
        OpenScenarioDirectory.Filter = "SCN2 files (*.SCN2)|*.SCN2|SCN files (*.SCN)|*.SCN|All files (*.*)|*.*"


        If System.IO.Directory.Exists(FileNameClass.previousBatchScenarioPath) Then
            OpenScenarioDirectory.InitialDirectory = FileNameClass.previousScenarioPath
        End If

        result = OpenScenarioDirectory.ShowDialog() 'display Open dialog box


        'Cancel button will cuase return without further execution
        If result = Windows.Forms.DialogResult.Cancel Then
            Return
        End If

        FileNameClass.previousBatchScenarioPath = System.IO.Path.GetDirectoryName(OpenScenarioDirectory.FileName)


        Dim selectedScenario As String

        For Each selectedScenario In OpenScenarioDirectory.FileNames
            ' ScenariosList.Items.Add(System.IO.Path.GetFileName(selectedScenario))
            'need path
            ScenariosList.Items.Add(selectedScenario)
        Next

        FileNameClass.previousBatchScenarioPath = System.IO.Path.GetDirectoryName(OpenScenarioDirectory.FileName)

    End Sub
    Private Sub ClearAllScenarios_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ClearAllScenarios.Click
        ScenariosList.Items.Clear()
    End Sub
    Private Sub ClearSelectedScenarios_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ClearSelectedScenarios.Click

        Dim selectedScenarios = (From i In ScenariosList.SelectedItems).ToArray()

        For Each selectedScenario In selectedScenarios
            ScenariosList.Items.Remove(selectedScenario)
        Next

    End Sub

    Private Sub WeatherButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        OpenFileDialogWeather.Filter = "DVF files (*.DVF)|*.DVF|MET files (*.MET)|*.MET|All files (*.*)|*.*"

        OpenFileDialogWeather.ShowDialog() 'display Open dialog box
        weatherBox.Text = OpenFileDialogWeather.FileName
    End Sub

    Private Function SplitString(ByVal str As String) As String()
        'str is input with multiple delimitters
        'TestArray is Output
        Dim TestArray() As String
        Dim lastNonEmpty = -1

        TestArray = Split(str)
        For i As Integer = 0 To TestArray.Length - 1
            If TestArray(i) <> "" Then
                lastNonEmpty = lastNonEmpty + 1
                TestArray(lastNonEmpty) = TestArray(i)
            End If

        Next
        ReDim Preserve TestArray(lastNonEmpty)

        Return TestArray


    End Function

    Private Function Pack(ByVal str As String) As String
        Dim words As Object
        Dim x As Long
        Dim temp As String = ""

        words = Split(str, " ")
        For x = LBound(words) To UBound(words)
            If words(x) <> "" Then
                temp = temp & " " & words(x)
            End If
        Next x
        Pack = temp
    End Function

    Private Sub SaveScenarioMenuItem_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SaveScenarioMenuItem.Click

        Dim result As System.Windows.Forms.DialogResult

        SaveScenarioDialog.Filter = "Scenario Files V2 (*.scn2)|*.SCN2"

        SaveScenarioDialog.InitialDirectory = FileNameClass.previousScenarioPath

        'make the file default to be the last file retrieved
        SaveScenarioDialog.FileName = FileNameClass.PreviousScenarioFile


        result = SaveScenarioDialog.ShowDialog(Me)


        'Cancel button will cuase return without further execution
        If result = Windows.Forms.DialogResult.Cancel Then
            Return
        End If

        FileNameClass.previousScenarioPath = System.IO.Path.GetDirectoryName(SaveScenarioDialog.FileName)


        writeScenarioInfoToFile(SaveScenarioDialog.FileName)


    End Sub

    Sub writeScenarioInfoToFile(ByVal filename As String)
        'Creates A STRING OF ALL SCENARIO INPUTS and writes them to "filename"
        Dim msg As String

        msg = WaterBodyInfo() & vbNewLine
        msg = msg & PRZMinfo_V2()

        Try
            My.Computer.FileSystem.WriteAllText(filename, msg, False, System.Text.Encoding.ASCII)
        Catch ex As Exception
            MsgBox(ex.Message)
        End Try


    End Sub

    Private Function LoadScenario(ByVal filename As String) As Boolean
        Dim extension As String
        extension = System.IO.Path.GetExtension(filename)
        extension = extension.ToUpper()




        LoadScenario = True
        Try


            Using MyReader As New Microsoft.VisualBasic.FileIO.TextFieldParser(filename)



                MyReader.TextFieldType = FileIO.FieldType.Delimited
                MyReader.SetDelimiters(",")

                If extension = ".SCN2" Then


                    getWaterBodyInfo2(MyReader)
                    getPRZMinfo_2(MyReader)
                Else
                    getWaterBodyInfo(MyReader)
                    getPRZMinfo(MyReader)
                End If

            End Using


            '*******CSTR ADDED HERE **********************
            If OverideExtracValues.Checked Then
                rDepthBox.Text = depthOveride.Text
                rBypassBox.Text = EffOverride.Text
                rDeclineBox.Text = declineOverride.Text
            End If


            If AddCSTR.Checked Then
                SoilProperty.AddCompartmentForCSTR(depthOveride.Text, numHoriz.Text)
                rDeclineBox.Text = 0.0
                rBypassBox.Text = EffOverride.Text
            End If

            '*****************************************


        Catch ex As Exception
            LoadScenario = False
        End Try

    End Function



    'Private Sub RetrieveScenarioMenuItem_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RetrieveScenarioMenuItem.Click

    '    RetrieveScenarioDialog.Filter = "Scenario Files V2 (*.SCN2)|*.SCN2| Scenario Files (*.SCN)|*.SCN"
    '    '  OpenFileDialog1.ShowDialog()

    '    If System.IO.Directory.Exists(FileNameClass.previousScenarioPath) Then
    '        RetrieveScenarioDialog.InitialDirectory = FileNameClass.previousScenarioPath
    '    End If


    '    Dim result As System.Windows.Forms.DialogResult
    '    result = RetrieveScenarioDialog.ShowDialog(Me)
    '    'Cancel button will cuase return without further execution
    '    If result = Windows.Forms.DialogResult.Cancel Then
    '        Return
    '    End If

    '    FileNameClass.previousScenarioPath = System.IO.Path.GetDirectoryName(RetrieveScenarioDialog.FileName)

    '    LoadScenario(RetrieveScenarioDialog.FileName)

    'End Sub

    'Private Sub RetrieveScenarioMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
    '    Dim result As System.Windows.Forms.DialogResult

    '    RetrieveScenarioDialog.Filter = "Scenario Files (*.SCN)|*.SCN"
    '    '  OpenFileDialog1.ShowDialog()

    '    result = RetrieveScenarioDialog.ShowDialog(Me)

    '    'Cancel button will cuase return without further execution
    '    If result = Windows.Forms.DialogResult.Cancel Then
    '        Return
    '    End If

    '    Using MyReader As New Microsoft.VisualBasic.FileIO.TextFieldParser(RetrieveScenarioDialog.FileName)

    '        MyReader.TextFieldType = FileIO.FieldType.Delimited
    '        MyReader.SetDelimiters(",")

    '        getWaterBodyInfo(MyReader)
    '        getPRZMinfo(MyReader)

    '    End Using
    'End Sub






    Private Sub updateHoriz_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles updateHoriz.Click
        updatehorizonsub()
    End Sub

    Private Sub simTemperature_CheckedChanged_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles simTemperature.CheckedChanged


        If simTemperature.Checked = True Then
            For i As Integer = 0 To SoilProperty.maxSoilLayers - 1
                SoilProperty.sand(i).Visible = True
                SoilProperty.clay(i).Visible = True
            Next
            Label69.Visible = True
            Label70.Visible = True
            Label83.Visible = True
            Label84.Visible = True
            albedoBox.Visible = True
            bcTemp.Visible = True
        Else
            For i As Integer = 0 To SoilProperty.maxSoilLayers - 1
                SoilProperty.sand(i).Visible = False
                SoilProperty.clay(i).Visible = False
            Next
            Label69.Visible = False
            Label70.Visible = False
            Label83.Visible = False
            Label84.Visible = False
            albedoBox.Visible = False
            bcTemp.Visible = False
        End If

        updatehorizonsub()
    End Sub
    Private Sub WeatherButton_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles WeatherButton.Click
        Dim result As System.Windows.Forms.DialogResult
        OpenFileDialogWeather.Filter = "WEA files (*.WEA)|*.WEA|DVF files (*.DVF)|*.DVF|All files (*.*)|*.*"

        If System.IO.Directory.Exists(FileNameClass.previousWeatherPath) Then
            OpenFileDialogWeather.InitialDirectory = FileNameClass.previousWeatherPath
        End If

        result = OpenFileDialogWeather.ShowDialog() 'display Open dialog box

        'Cancel button will cuase return without further execution
        If result = Windows.Forms.DialogResult.Cancel Then
            Return
        End If

        weatherBox.Text = OpenFileDialogWeather.FileName
        FileNameClass.previousWeatherPath = System.IO.Path.GetDirectoryName(OpenFileDialogWeather.FileName)

    End Sub

    Private Sub DisplayGraphs(ByVal WaterBodyType As String)

        Select Case WaterBodyType   'Output Graph #1 Pond
            Case StandardParameters.EpaReservoir

                ParentGraph2.Checked = False  'Clear previous graph
                ParentGraph2.Enabled = True

                'the following 2 lines are not absolutely necessary because it is redundant with "ParentOutput.Checked = True" but it allows 
                ' easier locating of the post process routine with GO TO DEFINITION
                postprocess(OutputDisplay.Display2, FileNameClass.reservoirParentFile)
                PlaceGraphTitle(Chart2, "Parent")

                ParentGraph2.Checked = True  'This starts the graphing.

                If Deg1CheckBox.Checked Then
                    Degradate1Graph2.Enabled = True
                Else
                    Degradate1Graph2.Enabled = False
                End If

                If Deg2CheckBox.Checked Then
                    Degradate2Graph2.Enabled = True
                Else
                    Degradate2Graph2.Enabled = False
                End If

            Case StandardParameters.EpaPond

                ParentOutput.Checked = False  'Clear previous graph
                ParentOutput.Enabled = True

                'the following 2 lines are not absolutely necessary because it is redundant with "ParentOutput.Checked = True" but it allows 
                ' easier locating of the post process routine with GO TO DEFINITION
                postprocess(OutputDisplay.Display1, FileNameClass.pondParentFile)
                PlaceGraphTitle(Chart1, "Parent")

                ParentOutput.Checked = True  'This starts the graphing.

                If Deg1CheckBox.Checked Then
                    Deg1Output.Enabled = True
                Else
                    Deg1Output.Enabled = False
                End If

                If Deg2CheckBox.Checked Then
                    Deg2Output.Enabled = True
                Else
                    Deg2Output.Enabled = False
                End If

            Case Else


                ParentGraph3.Checked = False  'Clear previous graph
                ParentGraph3.Enabled = True

                'the following 2 lines are not absolutely necessary because it is redundant with "ParentOutput.Checked = True" but it allows 
                ' easier locating of the post process routine with GO TO DEFINITION


                postprocess(OutputDisplay.Display3, FileNameClass.customParentFile)
                PlaceGraphTitle(Chart1, "Parent")




                ParentGraph3.Checked = True  'This starts the graphing.

                If Deg1CheckBox.Checked Then
                    Degradate1Graph3.Enabled = True
                Else
                    Degradate1Graph3.Enabled = False
                End If

                If Deg2CheckBox.Checked Then
                    Degradate2Graph3.Enabled = True
                Else
                    Degradate2Graph3.Enabled = False
                End If


        End Select

    End Sub
    Private Sub ParentOutput_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ParentOutput.CheckedChanged
        If ParentOutput.Checked = True Then
            Dim filename As String
            filename = FileNameClass.pondParentFile
            postprocess(OutputDisplay.Display1, filename)
            PlaceGraphTitle(Chart1, "Parent")
        End If
    End Sub
    Private Sub Deg1Output_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Deg1Output.CheckedChanged
        If Deg1Output.Checked = True Then
            Dim filename As String

            filename = FileNameClass.pondDeg1File
            postprocess(OutputDisplay.Display1, filename)
            PlaceGraphTitle(Chart1, "Degradate1")
        End If
    End Sub
    Private Sub Deg2Output_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Deg2Output.CheckedChanged
        If Deg2Output.Checked = True Then
            Dim filename As String

            filename = FileNameClass.pondDeg2File
            postprocess(OutputDisplay.Display1, filename)
            PlaceGraphTitle(Chart1, "Degradate2")
        End If
    End Sub

    Private Sub ParentGraph2_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ParentGraph2.CheckedChanged
        If ParentGraph2.Checked = True Then
            Dim filename As String
            filename = FileNameClass.reservoirParentFile
            postprocess(OutputDisplay.Display2, filename)
            PlaceGraphTitle(Chart2, "Parent")
        End If
    End Sub
    Private Sub Degradate1Graph2_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Degradate1Graph2.CheckedChanged

        If Degradate1Graph2.Checked = True Then
            Dim filename As String
            filename = FileNameClass.reservoirDeg1File
            postprocess(OutputDisplay.Display2, filename)
            PlaceGraphTitle(Chart2, "Degradate1")
        End If

    End Sub
    Private Sub Degradate2Graph2_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Degradate2Graph2.CheckedChanged
        If Degradate2Graph2.Checked = True Then
            Dim filename As String
            filename = FileNameClass.reservoirDeg2File
            postprocess(OutputDisplay.Display2, filename)
            PlaceGraphTitle(Chart2, "Degradate2")
        End If
    End Sub

    Private Sub ParentGraph3_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ParentGraph3.CheckedChanged
        If ParentGraph3.Checked = True Then
            Dim filename As String
            filename = FileNameClass.customParentFile
            postprocess(OutputDisplay.Display3, filename)
            PlaceGraphTitle(Chart3, "Parent")
        End If

    End Sub

    Private Sub Degradate1Graph3_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Degradate1Graph3.CheckedChanged
        If Degradate1Graph3.Checked = True Then
            Dim filename As String
            filename = FileNameClass.customDeg1File
            postprocess(OutputDisplay.Display3, filename)
            PlaceGraphTitle(Chart3, "Degradate1")
        End If

    End Sub

    Private Sub Degradate2Graph3_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Degradate2Graph3.CheckedChanged
        If Degradate2Graph3.Checked = True Then
            Dim filename As String
            filename = FileNameClass.customDeg2File
            postprocess(OutputDisplay.Display3, filename)
            PlaceGraphTitle(Chart3, "Degradate2")
        End If
    End Sub


    Private Sub postprocess(ByVal Display As Object, ByVal filename As String)

        Dim appinfo As New ApplicationInfo



        Try
            Using reader As New Microsoft.VisualBasic.FileIO.TextFieldParser(filename)

                reader.TextFieldType = Microsoft.VisualBasic.FileIO.FieldType.FixedWidth

                'Dim total As Single = 0.0
                'Dim totalAverage As Single
                'Dim maxConc As Single = 0.0
                Dim year As Integer
                Dim peak As Single
                Dim chronic4 As Single
                Dim chronic21 As Single
                Dim chronic60 As Single
                Dim chronic90 As Single
                Dim chronic365 As Single

                Dim benthicPeak As Single
                Dim benthic21 As Single
                Dim currentrow As String()

                Dim concPostBreakthrough As Single = 0.0
                Dim postCount As Integer = 0
                Dim maxPeak As Single = 0.0



                'skip the 5 Header Lines
                For i As Integer = 1 To 5
                    reader.ReadLine()
                Next



                reader.SetFieldWidths(21, 15, -1)
                currentrow = reader.ReadFields()

                Display(5).text = currentrow(1)

                currentrow = reader.ReadFields()
                Display(7).text = currentrow(1)    'Year90th.Text = currentrow(1)

                currentrow = reader.ReadFields()
                Display(24).text = currentrow(1)    'simulation average = currentrow(1)


                currentrow = reader.ReadFields()
                Display(6).text = currentrow(1)    'FourDay90th.Text = currentrow(1)

                currentrow = reader.ReadFields()
                Display(28).text = currentrow(1)    'FourDay90th.Text = currentrow(1)

                currentrow = reader.ReadFields()
                Display(29).text = currentrow(1)    'FourDay90th.Text = currentrow(1)


                reader.ReadLine() '90 day avg 

                currentrow = reader.ReadFields()
                Display(32).text = currentrow(1) '1 day avg 




                reader.SetFieldWidths(40, 15, -1)
                currentrow = reader.ReadFields()
                Display(8).text = currentrow(1)    'BenthicPeakTB.Text = currentrow(1)

                currentrow = reader.ReadFields()
                Display(9).text = currentrow(1)    'Benthic21DayTB.Text = currentrow(1)

                currentrow = reader.ReadFields()  'Get the Conversion factor for pore water to total normalized to dry sed

                Display(25).text = currentrow(1) * Display(8).text    'This is the peak total sediment concentration.
                Display(26).text = currentrow(1) * Display(9).text    'This is the 21-day total sediment concentration.

                currentrow = reader.ReadFields()

                Display(27).text = Convert.ToSingle(currentrow(1))

                For i As Integer = 1 To 2
                    reader.ReadLine()
                Next

                reader.SetFieldWidths(4, 11, 11, 11, 11, 11, 11, 11, 11, -1)
                'clear all point from previous run
                Display(0).Series("Water Column").Points.Clear()
                Display(0).Series("Benthic").Points.Clear()

                '   Chart1.BackColor = Color.White
                '  Display(0).ChartAreas("ChartArea1").AxisX.ScaleView.Zoomable = True


                Do
                    Try

                        currentrow = reader.ReadFields()

                        If currentrow(0) = "****" Then
                            Exit Do
                        End If

                        year = currentrow(0)
                        peak = currentrow(1)
                        chronic4 = currentrow(2)
                        chronic21 = currentrow(3)
                        chronic60 = currentrow(4)
                        chronic90 = currentrow(5)
                        chronic365 = currentrow(6)
                        benthicPeak = currentrow(7)
                        benthic21 = currentrow(8)

                        Display(0).Series("Water Column").Points.AddXY(year, peak)
                        Display(0).Series("Benthic").Points.AddXY(year, benthicPeak)

                        If peak > maxPeak Then
                            maxPeak = peak
                        End If


                    Catch ex As Exception
                        MsgBox(ex.Message)
                    End Try
                Loop


                Chart1.ChartAreas("ChartArea1").AxisX.Maximum = year + 1

                reader.ReadLine()
                reader.SetFieldWidths(38, -1)


                currentrow = reader.ReadFields()
                'WashoutText.Text = String.Format("{0,6:F1}", Convert.ToSingle(currentrow(1)))
                Display(18).text = String.Format("{0,6:F1}", Convert.ToSingle(currentrow(1)))


                currentrow = reader.ReadFields()
                'Metabolism1Text.Text = String.Format("{0,6:F1}", Convert.ToSingle(currentrow(1)))
                Display(19).text = String.Format("{0,6:F1}", Convert.ToSingle(currentrow(1)))

                currentrow = reader.ReadFields()
                'Hydrolysis1Text.Text = String.Format("{0,6:F1}", Convert.ToSingle(currentrow(1)))
                Display(20).text = String.Format("{0,6:F1}", Convert.ToSingle(currentrow(1)))


                currentrow = reader.ReadFields()
                ' Photolysis1Text.Text = String.Format("{0,6:F1}", Convert.ToSingle(currentrow(1)))
                Display(21).text = String.Format("{0,6:F1}", Convert.ToSingle(currentrow(1)))

                currentrow = reader.ReadFields()
                'Volatiliz1Text.Text = String.Format("{0,6:F1}", Convert.ToSingle(currentrow(1)))

                Display(22).text = String.Format("{0,6:F1}", Convert.ToSingle(currentrow(1)))

                currentrow = reader.ReadFields()
                'TotalDeg1Text.Text = String.Format("{0,6:F1}", Convert.ToSingle(currentrow(1)))
                Display(23).text = String.Format("{0,6:F1}", Convert.ToSingle(currentrow(1)))

                reader.ReadLine()

                currentrow = reader.ReadFields()
                'BurialText.Text = String.Format("{0,6:F1}", Convert.ToSingle(currentrow(1)))
                Display(14).text = String.Format("{0,6:F1}", Convert.ToSingle(currentrow(1)))

                currentrow = reader.ReadFields()
                'Metabolism2Text.Text = String.Format("{0,6:F1}", Convert.ToSingle(currentrow(1)))
                Display(15).text = String.Format("{0,6:F1}", Convert.ToSingle(currentrow(1)))

                currentrow = reader.ReadFields()
                'Hydrolysis2Text.Text = String.Format("{0,6:F1}", Convert.ToSingle(currentrow(1)))
                Display(16).text = String.Format("{0,6:F1}", Convert.ToSingle(currentrow(1)))

                currentrow = reader.ReadFields()
                'TotalDeg2Text.Text = String.Format("{0,6:F1}", Convert.ToSingle(currentrow(1)))
                Display(17).text = String.Format("{0,6:F1}", Convert.ToSingle(currentrow(1)))


                reader.ReadLine()
                reader.ReadLine()
                reader.ReadLine()

                Dim totalMass2Water As Single = 0.0

                reader.SetFieldWidths(18, 10, -1)
                currentrow = reader.ReadFields()
                ' RunoffText1.Text = String.Format("{0,6:F4}", Convert.ToSingle(currentrow(1)))
                Display(10).text = String.Format("{0,6:F4}", Convert.ToSingle(currentrow(1)))

                totalMass2Water = currentrow(2)

                currentrow = reader.ReadFields()
                'ErosionText1.Text = String.Format("{0,6:F4}", Convert.ToSingle(currentrow(1)))
                Display(11).text = String.Format("{0,6:F4}", Convert.ToSingle(currentrow(1)))


                totalMass2Water = totalMass2Water + currentrow(2)

                currentrow = reader.ReadFields()
                ' DriftText1.Text = String.Format("{0,6:F4}", Convert.ToSingle(currentrow(1)))
                Display(12).text = String.Format("{0,6:F4}", Convert.ToSingle(currentrow(1)))

                totalMass2Water = totalMass2Water + currentrow(2)

                ' Field2WaterText.Text = String.Format("{0,7:F5}", (totalMass2Water / ApplicationInfo.TotalMassApplied))
                Display(13).text = String.Format("{0,7:F5}", (totalMass2Water / ApplicationInfo.TotalMassApplied))


                ' PeakOutputLab
                Display(4).text = "Absolute Peak = " & maxPeak

                Display(0).Series("Water Column").ChartType = DataVisualization.Charting.SeriesChartType.FastPoint
                Display(0).Series("Benthic").ChartType = DataVisualization.Charting.SeriesChartType.FastPoint

                ' Chart1.ChartAreas(0).AxisX.Interval = 1
                '      Display(0).ChartAreas(0).AxisX.IntervalType = DataVisualization.Charting.DateTimeIntervalType.Auto

                '  Display(0).ChartAreas(0).AxisX.MajorGrid.Interval = 1
                '   Display(0).ChartAreas(0).AxisX.MajorGrid.LineColor = Color.LightGray
                'Chart1.ChartAreas(0).AxisX.MajorTickMark.Interval = 1

                PlaceGraphTitle(Display(0), "")

                '    Display(0).Legends.Add("Legend1")




                '      Display(0).Series("Series1").Name = "Water Column"
                '     Display(0).Series("Series2").Name = "Benthic"

                Display(0).Series("Water Column").Legend = "Legend1"
                Display(0).Series("Benthic").Legend = "Legend1"

                Display(0).Series("Water Column").IsVisibleInLegend = True
                Display(0).Series("Benthic").IsVisibleInLegend = True

            End Using


        Catch ex As Exception
            MsgBox("Post Processing Problem. " & ex.Message)
        End Try






    End Sub

    Private Sub PlaceGraphTitle(ByRef chart As System.Windows.Forms.DataVisualization.Charting.Chart, ByVal chemical As String)
        chart.Titles.Clear()
        chart.Titles.Add(chemID.Text & ", " & scenarioID.Text & ", " & chart.Tag & " " & chemical)

    End Sub

    Function checkWaterBodyType(ByVal WaterBodyType As String) As String

        ' just check the waterbody selection specifics Application Drift Efficiency
        Dim aa As String

        checkWaterBodyType = ""

        Select Case WaterBodyType
            Case StandardParameters.EpaReservoir
                ShowReservoirCheck.BackColor = Color.Transparent

                For i As Integer = 0 To appNumber.Text - 1
                    aa = testRealNumbers(ApplicationInfo.sprayr(i))
                    If aa <> "" Then Return aa
                    aa = testRealNumbers(ApplicationInfo.Effr(i))
                    If aa <> "" Then Return aa
                    'If ApplicationInfo.Effr(i).Text > 1 Then
                    '    ShowReservoirCheck.BackColor = Color.Red
                    '    MsgBox("Application efficiency is typically not greater than 1.  You can continue, but just saying...")
                    '    ApplicationInfo.Effr(i).BackColor = Color.BlueViolet
                    'End If



                    If UserSpecifiedFlowAvg.Checked Then
                        aa = testIntegers(ReservoirFlowAvgDays)
                        If aa <> "" Then Return aa
                    End If



                Next
            Case StandardParameters.EpaPond
                ShowPondCheck.BackColor = Color.Transparent
                For i As Integer = 0 To appNumber.Text - 1
                    aa = testRealNumbers(ApplicationInfo.sprayp(i))
                    If aa <> "" Then Return aa
                    aa = testRealNumbers(ApplicationInfo.Effp(i))
                    If aa <> "" Then Return aa
                Next
            Case StandardParameters.CustomVVWM, StandardParameters.ConstVolNoFlo

                For i As Integer = 0 To appNumber.Text - 1
                    aa = testRealNumbers(ApplicationInfo.spray(i))
                    If aa <> "" Then Return aa

                    aa = testRealNumbers(ApplicationInfo.Eff(i))
                    If aa <> "" Then Return aa
                Next


            Case StandardParameters.ConstVolFlo


                For i As Integer = 0 To appNumber.Text - 1
                    aa = testRealNumbers(ApplicationInfo.spray(i))
                    If aa <> "" Then Return aa

                    aa = testRealNumbers(ApplicationInfo.Eff(i))
                    If aa <> "" Then Return aa
                Next

                aa = testIntegers(CustomFlowAvgDays)
                If aa <> "" Then Return aa


            Case StandardParameters.NoWaterBody
                aa = testRealNumbers(hl) 'still need Hydraulic length and field area
                If aa <> "" Then Return aa
                aa = testRealNumbers(fieldAreaBox)
                If aa <> "" Then Return aa
                'For No water body efficiency will be ste to 1.
        End Select

        '****************************************************************************************************


    End Function

    Function Checkvalues() As String


        Dim aa As String
        Checkvalues = ""


        If AdjustForRain.Checked Then
            aa = TestRealNumbers(rain_limit)
            If aa <> "" Then Return aa

            aa = TestIntegers(intolerable_rain_window)
            If aa <> "" Then Return aa

            aa = TestIntegers(optimum_application_window)
            If aa <> "" Then Return aa

        End If




        'Make Sure a water body is selected

        If EPApond.Checked Then
        ElseIf EPAreservoir.Checked Then
        ElseIf VaryVolFlow.Checked Then
        ElseIf ConstVolNoFlow.Checked Then
        ElseIf ConstVolFlow.Checked Then
        ElseIf GroundWater.Checked Then
        ElseIf PRZMonly.Checked Then
        Else
            Return "Select a waterbody."
        End If

        If AppsRelative.Checked And evergreen.Checked Then
            Return "Relative applications cannot be used with Evergeen"
        End If


        '******CHECK CHEMICAL PROPERTIES ************************
        aa = testRealNumbers(KocBox1)
        If aa <> "" Then Return aa

        aa = testRealNumbers(waterMetabBox1, "")
        If aa <> "" Then Return aa

        If waterMetabBox1.Text <> "" Then
            aa = testRealNumbers(waterTempBox1)
            If aa <> "" Then Return aa
        End If

        aa = testRealNumbers(benthicMetabBox1, "")
        If aa <> "" Then Return aa

        If benthicMetabBox1.Text <> "" Then
            aa = testRealNumbers(benthicTempBox1)
            If aa <> "" Then Return aa
        End If

        aa = testRealNumbers(photoBox1, "")
        If aa <> "" Then Return aa

        If photoBox1.Text <> "" Then
            aa = testRealNumbers(rlatBox1)
            If aa <> "" Then Return aa
        End If

        aa = testRealNumbers(hydroBox1, "")
        If aa <> "" Then Return aa




        aa = testRealNumbers(soilDegradation1, "")
        If aa <> "" Then Return aa



        'If soilDegradation1.Text <> "" Then
        '    aa = testRealNumbers(soilTempBox1)
        '    If aa <> "" Then Return aa
        'End If

        'need a value for the ref temp if soil temp is simulated
        If simTemperature.Checked Then
            aa = testRealNumbers(soilTempBox1)
            If aa <> "" Then Return aa
        End If


        aa = testRealNumbers(foliarDeg1, "")
        If aa <> "" Then Return aa

        aa = testRealNumbers(mwtBox1)
        If aa <> "" Then Return aa


        '*********Volatilization-Chem Props*********************************
        aa = testRealNumbers(vpBox1, "")
        If aa <> "" Then Return aa

        aa = testRealNumbers(solBox1, "")
        If aa <> "" Then Return aa





        aa = testRealNumbers(henry1, "")
        If aa <> "" Then Return aa

        aa = testRealNumbers(airDiffusion1, "")
        If aa <> "" Then Return aa

        aa = testRealNumbers(enthalpyGas1, "")
        If aa <> "" Then Return aa




        aa = testRealNumbers(Q10Box)
        If aa <> "" Then Return aa

        If Deg1CheckBox.Checked Then
            aa = testRealNumbers(KocBox2)
            If aa <> "" Then Return aa

            aa = testRealNumbers(waterMetabBox2, "")
            If aa <> "" Then Return aa

            If waterMetabBox2.Text <> "" Then
                aa = testRealNumbers(waterTempBox2)
                If aa <> "" Then Return aa
            End If

            aa = testRealNumbers(benthicMetabBox2, "")
            If aa <> "" Then Return aa

            If benthicMetabBox2.Text <> "" Then
                aa = testRealNumbers(benthicTempBox2)
                If aa <> "" Then Return aa
            End If

            aa = testRealNumbers(photoBox2, "")
            If aa <> "" Then Return aa

            If photoBox2.Text <> "" Then
                aa = testRealNumbers(rlatBox2)
                If aa <> "" Then Return aa
            End If



            aa = testRealNumbers(hydroBox2, "")
            If aa <> "" Then Return aa

            aa = testRealNumbers(soilDegradation2, "")
            If aa <> "" Then Return aa
            aa = testRealNumbers(foliarDeg2, "")
            If aa <> "" Then Return aa

            aa = testRealNumbers(mwtBox2)
            If aa <> "" Then Return aa


            aa = testRealNumbers(vpBox2, "")
            If aa <> "" Then Return aa

            aa = testRealNumbers(solBox2, "")
            If aa <> "" Then Return aa



            aa = testRealNumbers(henry2, "")
            If aa <> "" Then Return aa
            aa = testRealNumbers(enthalpyGas2, "")
            If aa <> "" Then Return aa
            aa = testRealNumbers(airDiffusion2, "")
            If aa <> "" Then Return aa


            aa = testRealNumbers(convertWater1)
            If aa <> "" Then Return aa
            aa = testRealNumbers(convertBenthic1)
            If aa <> "" Then Return aa
            aa = testRealNumbers(convertPhoto1)
            If aa <> "" Then Return aa
            aa = testRealNumbers(convertHydro1)
            If aa <> "" Then Return aa
            aa = testRealNumbers(convertSoil1)
            If aa <> "" Then Return aa
            aa = testRealNumbers(convertFoliar1)
            If aa <> "" Then Return aa
        End If

        If Deg2CheckBox.Checked Then
            aa = testRealNumbers(KocBox3)
            If aa <> "" Then Return aa


            aa = testRealNumbers(waterMetabBox3, "")
            If aa <> "" Then Return aa


            If waterMetabBox3.Text <> "" Then
                aa = testRealNumbers(waterTempBox3)
                If aa <> "" Then Return aa
            End If

            aa = testRealNumbers(benthicMetabBox3, "")
            If aa <> "" Then Return aa

            If benthicMetabBox3.Text <> "" Then
                aa = testRealNumbers(benthicTempBox3)
                If aa <> "" Then Return aa
            End If

            aa = testRealNumbers(photoBox3, "")
            If aa <> "" Then Return aa

            If photoBox3.Text <> "" Then
                aa = testRealNumbers(rlatBox3)
                If aa <> "" Then Return aa
            End If


            aa = testRealNumbers(hydroBox3, "")
            If aa <> "" Then Return aa
            aa = testRealNumbers(soilDegradation3, "")
            If aa <> "" Then Return aa
            aa = testRealNumbers(foliarDeg3, "")
            If aa <> "" Then Return aa
            aa = testRealNumbers(mwtBox3)
            If aa <> "" Then Return aa


            aa = testRealNumbers(vpBox3, "")
            If aa <> "" Then Return aa
            aa = testRealNumbers(solBox3, "")
            If aa <> "" Then Return aa



            aa = testRealNumbers(henry3, "")
            If aa <> "" Then Return aa
            aa = testRealNumbers(airDiffusion3, "")
            If aa <> "" Then Return aa
            aa = testRealNumbers(enthalpyGas3, "")
            If aa <> "" Then Return aa

            aa = testRealNumbers(convertWater2)
            If aa <> "" Then Return aa
            aa = testRealNumbers(convertBenthic2)
            If aa <> "" Then Return aa
            aa = testRealNumbers(convertPhoto2)
            If aa <> "" Then Return aa
            aa = testRealNumbers(convertHydro2)
            If aa <> "" Then Return aa
            aa = testRealNumbers(convertSoil2)
            If aa <> "" Then Return aa
            aa = testRealNumbers(convertFoliar2)
            If aa <> "" Then Return aa
        End If


        '***********WEATHER FILE EXISTENCE ******************************
        weatherBox.BackColor = Color.White
        Dim fullweatherfile As String
        If UseWeatherDirecory.Checked Then
            fullweatherfile = WeatherDirectoryBox.Text & weatherBox.Text
        Else
            fullweatherfile = weatherBox.Text

        End If


        If Not System.IO.File.Exists(fullweatherfile) Then
            weatherBox.BackColor = Color.LimeGreen
            Return "Weather file does not exist."
        End If


        aa = testRealNumbers(ScenarioLatBox)
        If aa <> "" Then Return aa



        '********Check Application information*************************
        aa = testIntegers(appNumber)
        If aa <> "" Then Return aa

        aa = testPositiveNumbers(appNumber)
        If aa <> "" Then Return aa

        If ApplyWindow.Checked Then
            aa = testIntegers(SpanTB)
            If aa <> "" Then Return aa
            aa = testIntegers(IntervalTB)
            If aa <> "" Then Return aa

            If Convert.ToInt32(IntervalTB.Text) <= 0 Then
                IntervalTB.BackColor = Color.HotPink
                Return "The step for batch applications analysis must be greater than zero"
            End If
        End If

        '****************************************************************************************************
        If AppsAbsolute.Checked Then
            For i As Integer = 0 To appNumber.Text - 1
                aa = calendarCheck(ApplicationInfo.day(i), ApplicationInfo.mon(i))
                If aa <> "" Then Return aa
            Next
        End If
        '****************************************************************************************************
        If AppsRelative.Checked Then
            For i As Integer = 0 To appNumber.Text - 1
                aa = testIntegers(ApplicationInfo.RelApp(i))
                If aa <> "" Then Return aa
            Next
            'Relative dates get sorted before writing to przm file so need to for order on GUI
        End If
        '****************************************************************************************************

        If SpecifyYears.Checked Then
            For i As Integer = 0 To appNumber.Text - 1
                aa = testIntegers(ApplicationInfo.year(i))
                If aa <> "" Then Return aa
            Next
        End If


        For i As Integer = 0 To appNumber.Text - 1
            aa = testRealNumbers(ApplicationInfo.rate(i))
            If aa <> "" Then Return aa

            If ApplicationInfo.methodbutton(2 + i * ApplicationInfo.NumberofApplicationTypes).Checked Or
               ApplicationInfo.methodbutton(3 + i * ApplicationInfo.NumberofApplicationTypes).Checked Or
               ApplicationInfo.methodbutton(4 + i * ApplicationInfo.NumberofApplicationTypes).Checked Or
               ApplicationInfo.methodbutton(5 + i * ApplicationInfo.NumberofApplicationTypes).Checked Or
               ApplicationInfo.methodbutton(6 + i * ApplicationInfo.NumberofApplicationTypes).Checked Then
                aa = testRealNumbers(ApplicationInfo.DepthIncorp(i))
                If aa <> "" Then Return aa
            End If

            If ApplicationInfo.methodbutton(4 + i * ApplicationInfo.NumberofApplicationTypes).Checked Then
                aa = testRealNumbers(ApplicationInfo.tBand(i))
                If aa <> "" Then Return aa
            End If

        Next




        'Check to see if application methods are specified
        Dim AppMethodIsSpecified As Boolean
        For i As Integer = 0 To appNumber.Text - 1
            ApplicationInfo.MethodPanels(i).BorderStyle = BorderStyle.None
            AppMethodIsSpecified = False
            For Each AppMethodButton In ApplicationInfo.MethodPanels(i).Controls
                If AppMethodButton.Checked Then
                    AppMethodIsSpecified = True
                End If
            Next

            If AppMethodIsSpecified = False Then
                ApplicationInfo.MethodPanels(i).BorderStyle = BorderStyle.FixedSingle
                Return String.Format("Specify an Application Method for Application Number {0}", i + 1)
            End If
        Next



        If VaryVolFlow.Checked Or ConstVolFlow.Checked Or ConstVolNoFlow.Checked Then
            ' EPA Scenario Defined Parameters
            aa = testRealNumbers(fieldAreaBox)
            If aa <> "" Then Return aa
            aa = testRealNumbers(waterAreaBox)
            If aa <> "" Then Return aa
            aa = testRealNumbers(initialDepthBox)
            If aa <> "" Then Return aa
            aa = testRealNumbers(maxDepthBox)
            If aa <> "" Then Return aa
            aa = testRealNumbers(hl)
            If aa <> "" Then Return aa
            aa = testRealNumbers(CustomCroppedAreaBox)
            If aa <> "" Then Return aa
        End If

        If EPAreservoir.Checked Then
            aa = testRealNumbers(ReservoirCroppedAreaBox)
            If aa <> "" Then Return aa
        End If

        aa = testRealNumbers(massXferBox)
        If aa <> "" Then Return aa
        aa = testRealNumbers(prbenBox)
        If aa <> "" Then Return aa
        aa = testRealNumbers(benthicdepthBox)
        If aa <> "" Then Return aa
        aa = testRealNumbers(porosityBox)
        If aa <> "" Then Return aa
        aa = testRealNumbers(bdBox)
        If aa <> "" Then Return aa
        aa = testRealNumbers(foc2Box)
        If aa <> "" Then Return aa
        aa = testRealNumbers(DOC2Box)
        If aa <> "" Then Return aa
        aa = testRealNumbers(biomass2Box)
        If aa <> "" Then Return aa
        aa = testRealNumbers(dfacBox)
        If aa <> "" Then Return aa
        aa = testRealNumbers(ssBox)
        If aa <> "" Then Return aa
        aa = testRealNumbers(ChlorophyllBox)
        If aa <> "" Then Return aa
        aa = testRealNumbers(foc1Box)
        If aa <> "" Then Return aa
        aa = testRealNumbers(DOC1Box)
        If aa <> "" Then Return aa
        aa = testRealNumbers(Biomass1Box)
        If aa <> "" Then Return aa


        '*******************CROP PROPERTIES *********************

        If simpleRB.Checked Then
            aa = calendarCheck(dayEmerge1, monthEmerge1)
            If aa <> "" Then Return aa
            aa = calendarCheck(dayMature1, monthMature1)
            If aa <> "" Then Return aa
            aa = calendarCheck(dayHarvest1, monthHarvest1)
            If aa <> "" Then Return aa

            aa = testRealNumbers(rootDepth1)
            If aa <> "" Then Return aa
            aa = testRealNumbers(canopyCover1)
            If aa <> "" Then Return aa
            aa = testRealNumbers(canopyHeight1)
            If aa <> "" Then Return aa
            aa = testRealNumbers(canopyHoldup1)
            If aa <> "" Then Return aa

        Else
            If lessThanAnnualGrowth.Checked Then
                aa = testIntegers(CropCyclesPerYear)
                If aa <> "" Then Return aa

                For i As Integer = 0 To Convert.ToInt16(CropCyclesPerYear.Text) - 1
                    aa = calendarCheck(CropProperties.emergenceDay(i), CropProperties.emergenceMonth(i))
                    If aa <> "" Then Return aa
                    aa = calendarCheck(CropProperties.maturityDay(i), CropProperties.maturityMonth(i))
                    If aa <> "" Then Return aa
                    aa = calendarCheck(CropProperties.harvestDay(i), CropProperties.harvestMonth(i))
                    If aa <> "" Then Return aa
                    aa = testRealNumbers(CropProperties.rootDepth(i))
                    If aa <> "" Then Return aa
                    aa = testRealNumbers(CropProperties.canopyCover(i))
                    If aa <> "" Then Return aa
                    aa = testRealNumbers(CropProperties.canopyHeight(i))
                    If aa <> "" Then Return aa
                    aa = testRealNumbers(CropProperties.canopyHoldup(i))
                    If aa <> "" Then Return aa
                    aa = testIntegers(CropProperties.plantingFrequency(i))
                    If aa <> "" Then Return aa
                    aa = testIntegers(CropProperties.plantingLag(i))
                    If aa <> "" Then Return aa
                Next

            Else
                aa = testRealNumbers(altRootDepth)
                If aa <> "" Then Return aa
                aa = testRealNumbers(altCanopyCover)
                If aa <> "" Then Return aa
                aa = testRealNumbers(altCanopyHeight)
                If aa <> "" Then Return aa
                aa = testRealNumbers(altCanopyHoldup)
                If aa <> "" Then Return aa

                If greaterThanAnualGrowth.Checked Then
                    aa = calendarCheck(altEmergeDay, altEmergeMon)
                    If aa <> "" Then Return aa
                    aa = testIntegers(altDaysToMaturity)
                    If aa <> "" Then Return aa
                    aa = testIntegers(altDaysToHarvest)
                    If aa <> "" Then Return aa
                End If
            End If
        End If


        '**** HYDROOLGY ******************
        If aa <> "" Then Return aa
        aa = testRealNumbers(pfac)
        If aa <> "" Then Return aa
        aa = testRealNumbers(snowmelt)
        If aa <> "" Then Return aa
        aa = testRealNumbers(evapDepth)
        If aa <> "" Then Return aa



        '********** IRIGATION************************

        If noIrrigation.Checked = False Then
            aa = testRealNumbers(fleach)
            If aa <> "" Then Return aa
            aa = testRealNumbers(depletion)
            If aa <> "" Then Return aa
            aa = testRealNumbers(rateIrrig)
            If aa <> "" Then Return aa

            If UserSpecifiesIrrigDepth.Checked Then
                aa = testRealNumbers(IrrigationDepthUserSpec)
                If aa <> "" Then Return aa
            End If

        End If

        aa = testIntegers(ireg)
        If aa <> "" Then Return aa
        aa = testRealNumbers(slope)
        If aa <> "" Then Return aa

        'Slope should be greater than zero or crashes
        If slope.Text <= 0 Then
            slope.BackColor = Color.OliveDrab
            Return "Slope must be greater than zero."
        End If



        aa = testRealNumbers(uslek)
        If aa <> "" Then Return aa
        aa = testRealNumbers(uslels)
        If aa <> "" Then Return aa
        aa = testRealNumbers(uslep)
        If aa <> "" Then Return aa


        aa = testIntegers(NumberOfFactors)
        If aa <> "" Then Return aa

        For i As Integer = 0 To NumberOfFactors.Text - 1
            aa = calendarCheck(USLE.day(i), USLE.mon(i))
            If aa <> "" Then Return aa
            aa = testRealNumbers(USLE.cn(i))
            If aa <> "" Then Return aa
            aa = testRealNumbers(USLE.C(i))
            If aa <> "" Then Return aa


            If Not Test3new.Checked Then   'Lag method does not need manning
                aa = testRealNumbers(USLE.n(i))
                If aa <> "" Then Return aa
            End If




            If USLE.cn(i).Text < 1 Or USLE.cn(i).Text > 100 Then
                USLE.cn(i).BackColor = Color.Chartreuse
                Return "Curve Number must be in range of 1 to 100"
            End If

            If useUSLEYear.Checked Then
                aa = testIntegers(USLE.year(i))
                If aa <> "" Then Return aa
            End If
        Next

        scenarioID.BackColor = Color.White

        If scenarioID.Text.Contains("\") Then
            scenarioID.BackColor = Color.Aquamarine
            Return "Scenario ID can not conrtain a slash ('\')."
        End If

        If scenarioID.Text.Contains(":") Then
            scenarioID.BackColor = Color.Khaki
            Return "Scenario ID can not conrtain a colon (':')."
        End If


        aa = testRealNumbers(volatilizationBoundaryBox, "")
        If volatilizationBoundaryBox.Text = "" Then
            volatilizationBoundaryBox.Text = 0.5
        End If
        If aa <> "" Then Return aa
        '******SOIL PROPERTIES ******************************************************

        Dim totalSoilDepth As Single
        totalSoilDepth = 0.0
        For i As Integer = 0 To numHoriz.Text - 1
            aa = testRealNumbers(SoilProperty.thick(i))
            If aa <> "" Then Return aa
            aa = testRealNumbers(SoilProperty.bulkden(i))
            If aa <> "" Then Return aa
            aa = testRealNumbers(SoilProperty.maxcap(i))
            If aa <> "" Then Return aa
            aa = testRealNumbers(SoilProperty.mincap(i))
            If aa <> "" Then Return aa
            aa = testRealNumbers(SoilProperty.oc(i))
            If aa <> "" Then Return aa
            aa = testIntegers(SoilProperty.compartment(i))
            If aa <> "" Then Return aa & "Value is :" & SoilProperty.compartment(i).Text

            If simTemperature.Checked Then
                aa = testRealNumbers(SoilProperty.sand(i))
                If aa <> "" Then Return aa
                aa = testRealNumbers(SoilProperty.clay(i))
                If aa <> "" Then Return aa
            End If
            totalSoilDepth = totalSoilDepth + SoilProperty.thick(i).Text
        Next

        If simTemperature.Checked Then
            aa = testRealNumbers(bcTemp)
            If aa <> "" Then Return aa
            aa = testRealNumbers(albedoBox)
            If aa <> "" Then Return aa
        End If


        If totalSoilDepth < rootDepth1.Text + 0.5 Then
            rootDepth1.BackColor = Color.Orange
            Return "Root depth should be at least 0.5 cm less than the soil profile. Extend the profile or shorten the root."
        End If


        If totalSoilDepth < evapDepth.Text + 0.5 Then
            evapDepth.BackColor = Color.Orange
            Return "Evaporation depth  should be at least 0.5 cm less than the soil profile. Extend the profile or shorten the evaporation depth."
        End If


        aa = testRealNumbers(eDepthBox)
        If aa <> "" Then Return aa

        If totalSoilDepth < eDepthBox.Text + 0.5 Then
            eDepthBox.BackColor = Color.Orange
            Return "Erosion extraction depth should be at least 0.5 cm less than the soil profile. Extend the profile or shorten the erosion depth."
        End If

        aa = testRealNumbers(rDepthBox)
        If aa <> "" Then Return aa

        If totalSoilDepth < rDepthBox.Text + 0.5 Then
            rDepthBox.BackColor = Color.Orange
            Return "Runoff extraction depth should be at least .5 cm less than the soil profile. Extend the profile or shorten the erosion depth."
        End If

        '************EROSION & RUNOFF EXTRACTION *******************************
        aa = testRealNumbers(rDepthBox)
        If aa <> "" Then Return aa
        aa = testRealNumbers(rDeclineBox)
        If aa <> "" Then Return aa
        aa = testRealNumbers(rBypassBox)
        If aa <> "" Then Return aa
        aa = testRealNumbers(eBypassBox)
        If aa <> "" Then Return aa
        aa = testRealNumbers(eDepthBox)
        If aa <> "" Then Return aa
        aa = testRealNumbers(eDeclineBox)
        If aa <> "" Then Return aa
        aa = testRealNumbers(eBypassBox)
        If aa <> "" Then Return aa

        '****Optional Output *********************************************

        If OutputDecayed.Checked Then
            aa = testRealNumbers(OutputDecayDepth1)
            If aa <> "" Then Return aa
            aa = testRealNumbers(OutputDecayDepth2)
            If aa <> "" Then Return aa
        End If

        If OutputMassSoilSpecific.Checked Then
            aa = testRealNumbers(OutputMassDepth1)
            If aa <> "" Then Return aa
            aa = testRealNumbers(OutputMassDepth2)
            If aa <> "" Then Return aa
        End If

        If OutputInfiltrationAtDepth.Checked Then
            aa = testRealNumbers(OutputInfiltrationDepth)
            If aa <> "" Then Return aa
        End If

    End Function

    Public Function RelativeDateConverter(ByVal appdate As Integer, cropNumber As Integer) As Date
        'gets the text box array of relative applications and sends back a date  with absolute dates
        Dim dateHolder As Date

        'If RelToEmerge.Checked Then
        '    dateHolder = DateSerial(1900, monthEmerge1.Text, dayEmerge1.Text).AddDays(appdate)
        'ElseIf RelToMat.Checked Then
        '    dateHolder = DateSerial(1900, monthMature1.Text, dayMature1.Text).AddDays(appdate)
        'Else
        '    dateHolder = DateSerial(1900, monthHarvest1.Text, dayHarvest1.Text).AddDays(appdate)
        'End If



        If RelToEmerge.Checked Then
            dateHolder = DateSerial(1900, CropProperties.emergenceMonth(cropNumber).Text, CropProperties.emergenceDay(cropNumber).Text).AddDays(appdate)
        ElseIf RelToMat.Checked Then
            dateHolder = DateSerial(1900, CropProperties.maturityMonth(cropNumber).Text, CropProperties.maturityDay(cropNumber).Text).AddDays(appdate)
        Else
            dateHolder = DateSerial(1900, CropProperties.harvestMonth(cropNumber).Text, CropProperties.harvestDay(cropNumber).Text).AddDays(appdate)
        End If



        'reset the year to 1900 so that all apps occur on the same year, even if they actually fall on previous or next calendar year
        dateHolder = DateSerial(1900, dateHolder.Month, dateHolder.Day)



        Return dateHolder
    End Function

    Private Sub UpdateApplicationChecks()
        Dim NumAppTypes As Integer = ApplicationInfo.NumberofApplicationTypes
        For i As Integer = 0 To appNumber.Text - 1
            If ApplicationInfo.methodbutton(NumAppTypes * i).Checked Or ApplicationInfo.methodbutton(1 + NumAppTypes * i).Checked Then
                ApplicationInfo.DepthIncorp(i).Visible = False
            Else
                ApplicationInfo.DepthIncorp(i).Visible = True
            End If


            If ApplicationInfo.methodbutton(4 + NumAppTypes * i).Checked Then
                ApplicationInfo.tBand(i).Visible = True
            Else
                ApplicationInfo.tBand(i).Visible = False
            End If



        Next
    End Sub

    Private Sub RB11_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RB11.CheckedChanged, RB12.CheckedChanged, RB13.CheckedChanged, RB14.CheckedChanged, RB15.CheckedChanged,
        RB21.CheckedChanged, RB22.CheckedChanged, RB23.CheckedChanged, RB24.CheckedChanged, RB25.CheckedChanged,
        RB31.CheckedChanged, RB32.CheckedChanged, RB33.CheckedChanged, RB34.CheckedChanged, RB35.CheckedChanged,
        RB41.CheckedChanged, RB42.CheckedChanged, RB43.CheckedChanged, RB44.CheckedChanged, RB45.CheckedChanged,
        RB51.CheckedChanged, RB52.CheckedChanged, RB53.CheckedChanged, RB54.CheckedChanged, RB55.CheckedChanged,
        RB61.CheckedChanged, RB62.CheckedChanged, RB63.CheckedChanged, RB64.CheckedChanged, RB65.CheckedChanged,
        RB71.CheckedChanged, RB72.CheckedChanged, RB73.CheckedChanged, RB74.CheckedChanged, RB75.CheckedChanged,
        RB81.CheckedChanged, RB82.CheckedChanged, RB83.CheckedChanged, RB84.CheckedChanged, RB85.CheckedChanged,
        RB91.CheckedChanged, RB92.CheckedChanged, RB93.CheckedChanged, RB94.CheckedChanged, RB95.CheckedChanged,
        RB101.CheckedChanged, RB102.CheckedChanged, RB103.CheckedChanged, RB104.CheckedChanged, RB105.CheckedChanged,
        RB111.CheckedChanged, RB112.CheckedChanged, RB113.CheckedChanged, RB114.CheckedChanged, RB115.CheckedChanged,
        RB121.CheckedChanged, RB122.CheckedChanged, RB123.CheckedChanged, RB124.CheckedChanged, RB125.CheckedChanged,
        RB131.CheckedChanged, RB132.CheckedChanged, RB133.CheckedChanged, RB134.CheckedChanged, RB135.CheckedChanged,
        RB141.CheckedChanged, RB142.CheckedChanged, RB143.CheckedChanged, RB144.CheckedChanged, RB145.CheckedChanged,
        RB151.CheckedChanged, RB152.CheckedChanged, RB153.CheckedChanged, RB154.CheckedChanged, RB155.CheckedChanged,
        RB161.CheckedChanged, RB162.CheckedChanged, RB163.CheckedChanged, RB164.CheckedChanged, RB165.CheckedChanged,
        RB171.CheckedChanged, RB172.CheckedChanged, RB173.CheckedChanged, RB174.CheckedChanged, RB175.CheckedChanged,
        RB181.CheckedChanged, RB182.CheckedChanged, RB183.CheckedChanged, RB184.CheckedChanged, RB185.CheckedChanged,
        RB191.CheckedChanged, RB192.CheckedChanged, RB193.CheckedChanged, RB194.CheckedChanged, RB195.CheckedChanged,
        RB201.CheckedChanged, RB202.CheckedChanged, RB203.CheckedChanged, RB204.CheckedChanged, RB205.CheckedChanged,
        RB211.CheckedChanged, RB212.CheckedChanged, RB213.CheckedChanged, RB214.CheckedChanged, RB215.CheckedChanged,
        RB221.CheckedChanged, RB222.CheckedChanged, RB223.CheckedChanged, RB224.CheckedChanged, RB225.CheckedChanged,
        RB231.CheckedChanged, RB232.CheckedChanged, RB233.CheckedChanged, RB234.CheckedChanged, RB235.CheckedChanged,
        RB241.CheckedChanged, RB242.CheckedChanged, RB243.CheckedChanged, RB244.CheckedChanged, RB245.CheckedChanged,
        RB251.CheckedChanged, RB252.CheckedChanged, RB253.CheckedChanged, RB254.CheckedChanged, RB255.CheckedChanged,
        RB261.CheckedChanged, RB262.CheckedChanged, RB263.CheckedChanged, RB264.CheckedChanged, RB265.CheckedChanged,
        RB271.CheckedChanged, RB272.CheckedChanged, RB273.CheckedChanged, RB274.CheckedChanged, RB275.CheckedChanged,
        RB281.CheckedChanged, RB282.CheckedChanged, RB283.CheckedChanged, RB284.CheckedChanged, RB285.CheckedChanged,
        RB291.CheckedChanged, RB292.CheckedChanged, RB293.CheckedChanged, RB294.CheckedChanged, RB295.CheckedChanged,
        RB301.CheckedChanged, RB302.CheckedChanged, RB303.CheckedChanged, RB304.CheckedChanged, RB305.CheckedChanged,
        RB311.CheckedChanged, RB312.CheckedChanged, RB313.CheckedChanged, RB314.CheckedChanged, RB315.CheckedChanged,
        RB321.CheckedChanged, RB322.CheckedChanged, RB323.CheckedChanged, RB324.CheckedChanged, RB325.CheckedChanged,
        RB331.CheckedChanged, RB332.CheckedChanged, RB333.CheckedChanged, RB334.CheckedChanged, RB335.CheckedChanged,
        RB341.CheckedChanged, RB342.CheckedChanged, RB343.CheckedChanged, RB344.CheckedChanged, RB345.CheckedChanged,
        RB351.CheckedChanged, RB352.CheckedChanged, RB353.CheckedChanged, RB354.CheckedChanged, RB355.CheckedChanged,
        RB361.CheckedChanged, RB362.CheckedChanged, RB363.CheckedChanged, RB364.CheckedChanged, RB365.CheckedChanged,
        RB371.CheckedChanged, RB372.CheckedChanged, RB373.CheckedChanged, RB374.CheckedChanged, RB375.CheckedChanged,
        RB381.CheckedChanged, RB382.CheckedChanged, RB383.CheckedChanged, RB384.CheckedChanged, RB385.CheckedChanged,
        RB391.CheckedChanged, RB392.CheckedChanged, RB393.CheckedChanged, RB394.CheckedChanged, RB395.CheckedChanged,
        RB401.CheckedChanged, RB402.CheckedChanged, RB403.CheckedChanged, RB404.CheckedChanged, RB405.CheckedChanged,
        RB411.CheckedChanged, RB412.CheckedChanged, RB413.CheckedChanged, RB414.CheckedChanged, RB415.CheckedChanged,
        RB421.CheckedChanged, RB422.CheckedChanged, RB423.CheckedChanged, RB424.CheckedChanged, RB425.CheckedChanged,
        RB431.CheckedChanged, RB432.CheckedChanged, RB433.CheckedChanged, RB434.CheckedChanged, RB435.CheckedChanged,
        RB441.CheckedChanged, RB442.CheckedChanged, RB443.CheckedChanged, RB444.CheckedChanged, RB445.CheckedChanged,
        RB451.CheckedChanged, RB452.CheckedChanged, RB453.CheckedChanged, RB454.CheckedChanged, RB455.CheckedChanged,
        RB461.CheckedChanged, RB462.CheckedChanged, RB463.CheckedChanged, RB464.CheckedChanged, RB465.CheckedChanged,
        RB471.CheckedChanged, RB472.CheckedChanged, RB473.CheckedChanged, RB474.CheckedChanged, RB475.CheckedChanged,
        RB481.CheckedChanged, RB482.CheckedChanged, RB483.CheckedChanged, RB484.CheckedChanged, RB485.CheckedChanged,
        RB491.CheckedChanged, RB492.CheckedChanged, RB493.CheckedChanged, RB494.CheckedChanged, RB495.CheckedChanged,
        RB501.CheckedChanged, RB502.CheckedChanged, RB503.CheckedChanged, RB504.CheckedChanged, RB505.CheckedChanged


        UpdateApplicationChecks()
    End Sub


    Private Sub HelpToolStripMenuItem1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HelpToolStripMenuItem1.Click
        My.Forms.HelpInfo.Show()
    End Sub


    Private Sub ShowReservoirCheck_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ShowReservoirCheck.CheckedChanged




        If ShowReservoirCheck.Checked Then

            For i As Integer = 0 To ApplicationInfo.MaximumApplications - 1
                ApplicationInfo.Effr(i).Visible = False
                ApplicationInfo.sprayr(i).Visible = False
            Next

            Label188.Visible = False
            Label74.Visible = False
        Else
            Label188.Visible = True
            Label74.Visible = True

            For i As Integer = 0 To appNumber.Text - 1
                ApplicationInfo.Effr(i).Visible = True
                ApplicationInfo.sprayr(i).Visible = True
            Next

            For i As Integer = appNumber.Text To ApplicationInfo.MaximumApplications - 1
                ApplicationInfo.Effr(i).Visible = False
                ApplicationInfo.sprayr(i).Visible = False
            Next
        End If



    End Sub

    Private Sub ShowPondCheck_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ShowPondCheck.CheckedChanged

        If ShowPondCheck.Checked Then

            For i As Integer = 0 To ApplicationInfo.MaximumApplications - 1
                ApplicationInfo.Effp(i).Visible = False
                ApplicationInfo.sprayp(i).Visible = False
            Next

            Label280.Visible = False
            Label281.Visible = False
        Else
            Label280.Visible = True
            Label281.Visible = True

            For i As Integer = 0 To appNumber.Text - 1
                ApplicationInfo.Effp(i).Visible = True
                ApplicationInfo.sprayp(i).Visible = True
            Next

            For i As Integer = appNumber.Text To ApplicationInfo.MaximumApplications - 1
                ApplicationInfo.Effp(i).Visible = False
                ApplicationInfo.sprayp(i).Visible = False
            Next


        End If

    End Sub

    Private Sub ShowCustomCheck_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ShowCustomCheck.CheckedChanged
        If ShowCustomCheck.Checked Then
            For i As Integer = 0 To ApplicationInfo.MaximumApplications - 1
                ApplicationInfo.Eff(i).Visible = False
                ApplicationInfo.spray(i).Visible = False
            Next

            Label282.Visible = False
            Label283.Visible = False
        Else
            Label282.Visible = True
            Label283.Visible = True

            For i As Integer = 0 To appNumber.Text - 1
                ApplicationInfo.Eff(i).Visible = True
                ApplicationInfo.spray(i).Visible = True
            Next

            For i As Integer = appNumber.Text To ApplicationInfo.MaximumApplications - 1
                ApplicationInfo.Eff(i).Visible = False
                ApplicationInfo.spray(i).Visible = False
            Next

        End If

    End Sub


    Private Sub AdjustSprayAndEfficiency()
        If ShowCustomCheck.Checked = False Then

            'For i As Integer = 0 To ApplicationInfo.MaximumApplications - 1
            '    ApplicationInfo.Effp(i).Visible = False
            '    ApplicationInfo.Effr(i).Visible = False
            '    ApplicationInfo.sprayp(i).Visible = False
            '    ApplicationInfo.sprayr(i).Visible = False
            'Next

            For i As Integer = 0 To appNumber.Text - 1
                ApplicationInfo.Eff(i).Visible = True
                ApplicationInfo.spray(i).Visible = True
            Next

            For i As Integer = appNumber.Text To ApplicationInfo.MaximumApplications - 1
                ApplicationInfo.Eff(i).Visible = False
                ApplicationInfo.spray(i).Visible = False
            Next
        End If

        If ShowPondCheck.Checked = False Then

            For i As Integer = 0 To appNumber.Text - 1
                ApplicationInfo.Effp(i).Visible = True
                ApplicationInfo.sprayp(i).Visible = True
            Next

            For i As Integer = appNumber.Text To ApplicationInfo.MaximumApplications - 1
                ApplicationInfo.Effp(i).Visible = False
                ApplicationInfo.sprayp(i).Visible = False
            Next
        End If

        If ShowReservoirCheck.Checked = False Then

            For i As Integer = 0 To appNumber.Text - 1
                ApplicationInfo.Effr(i).Visible = True
                ApplicationInfo.sprayr(i).Visible = True
            Next

            For i As Integer = appNumber.Text To ApplicationInfo.MaximumApplications - 1
                ApplicationInfo.Effr(i).Visible = False
                ApplicationInfo.sprayr(i).Visible = False
            Next

        End If

    End Sub

    Private Sub useUSLEYear_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles useUSLEYear.CheckedChanged

        Panel10.VerticalScroll.Value = 0


        If useUSLEYear.Checked Then
            yearLabel.Visible = True
            For i = 0 To USLE.MaxHydroErosionFactors - 1
                USLE.year(i).Visible = True
            Next

        Else
            yearLabel.Visible = False
            For i = 0 To USLE.MaxHydroErosionFactors - 1
                USLE.year(i).Visible = False
            Next
        End If
    End Sub

    Private Sub SpecifyYears_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SpecifyYears.CheckedChanged
        AdjustApplicationDates()
    End Sub



    Private Sub FindCalibrationData_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FindCalibrationData.Click
        FindCalibrationFile.Filter = "DAT files (*.DAT)|*.DAT|TXT files (*.TXT)|*.TXT|All files (*.*)|*.*"

        FindCalibrationFile.ShowDialog() 'display Open dialog box
        calibrationFileBox.Text = FindCalibrationFile.FileName
    End Sub

    Private Sub ReadCalibrationData_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ReadCalibrationData.CheckedChanged
        If ReadCalibrationData.Checked Then
            FindCalibrationData.Visible = True
            calibrationFileBox.Visible = True
        Else
            FindCalibrationData.Visible = False
            calibrationFileBox.Visible = False
        End If

    End Sub

    Private Sub EpaDefaultsCheck_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles EpaDefaultsCheck.CheckedChanged

        If EpaDefaultsCheck.Checked Then
            dfacBox.Enabled = False
            ssBox.Enabled = False
            ChlorophyllBox.Enabled = False
            foc1Box.Enabled = False
            DOC1Box.Enabled = False
            Biomass1Box.Enabled = False
            benthicdepthBox.Enabled = False
            porosityBox.Enabled = False
            bdBox.Enabled = False
            foc2Box.Enabled = False
            DOC2Box.Enabled = False
            biomass2Box.Enabled = False
            massXferBox.Enabled = False
            prbenBox.Enabled = False

            dfacBox.Text = StandardParameters.DFAC
            ssBox.Text = StandardParameters.SS
            ChlorophyllBox.Text = StandardParameters.Chlorophyll
            foc1Box.Text = StandardParameters.foc1
            DOC1Box.Text = StandardParameters.DOC1
            Biomass1Box.Text = StandardParameters.biomass1
            benthicdepthBox.Text = StandardParameters.benthicDepth
            porosityBox.Text = StandardParameters.porosity
            bdBox.Text = StandardParameters.bulkdensity
            foc2Box.Text = StandardParameters.foc2
            DOC2Box.Text = StandardParameters.DOC2
            biomass2Box.Text = StandardParameters.biomass2
            massXferBox.Text = StandardParameters.massTransCoeff
            prbenBox.Text = StandardParameters.prben
        Else

            dfacBox.Enabled = True
            ssBox.Enabled = True
            ChlorophyllBox.Enabled = True
            foc1Box.Enabled = True
            DOC1Box.Enabled = True
            Biomass1Box.Enabled = True
            benthicdepthBox.Enabled = True
            porosityBox.Enabled = True
            bdBox.Enabled = True
            foc2Box.Enabled = True
            DOC2Box.Enabled = True
            biomass2Box.Enabled = True
            massXferBox.Enabled = True
            prbenBox.Enabled = True



        End If



    End Sub

    Private Sub MakeWordDoc1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MakeWordDoc1.Click
        CreateWordDocument(OutputDisplay.Display1)
    End Sub

    Private Sub MakeWordDoc2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MakeWordDoc2.Click
        CreateWordDocument(OutputDisplay.Display2)
    End Sub

    Private Sub MakeWordDoc3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MakeWordDoc3.Click
        CreateWordDocument(OutputDisplay.Display3)
    End Sub

    Private Sub CreateWordDocument(ByVal Display As Object)

        If Display(5).text = "" Then
            MsgBox("Yous needs dus a run first")
            Return
        End If

        Try   ' general error catiching for Word document

            Dim oWord As Microsoft.Office.Interop.Word.Application

            Dim oDoc As Microsoft.Office.Interop.Word.Document

            Dim oPara0 As Microsoft.Office.Interop.Word.Paragraph
            Dim oPara1 As Microsoft.Office.Interop.Word.Paragraph
            Dim oPara2 As Microsoft.Office.Interop.Word.Paragraph
            Dim oPara3 As Microsoft.Office.Interop.Word.Paragraph
            Dim oPara4 As Microsoft.Office.Interop.Word.Paragraph
            Dim oPara5 As Microsoft.Office.Interop.Word.Paragraph


            Dim oTable As Microsoft.Office.Interop.Word.Table
            Dim oTable2 As Microsoft.Office.Interop.Word.Table
            Dim oTable3 As Microsoft.Office.Interop.Word.Table

            oWord = CreateObject("Word.Application")
            oDoc = oWord.Documents.Add()

            oWord.Visible = True


            Dim msg As String

            Dim pesticideName As String

            If Display(1).checked Then
                pesticideName = chemID.Text
            ElseIf Display(2).checked Then
                pesticideName = "Degradate #1 of " & chemID.Text
            Else
                pesticideName = "Degradate #2 of " & chemID.Text
            End If


            oPara0 = oDoc.Content.Paragraphs.Add
            oPara0.Range.Font.Bold = True
            oPara0.Range.Font.Size = 16
            oPara0.Range.Text = "Summary of Water Modeling of " & pesticideName & " and the " &
                CultureInfo.CurrentCulture.TextInfo.ToTitleCase(Display(30))
            oPara0.Range.InsertParagraphAfter()



            'oPara1 = oDoc.Content.Paragraphs.Add(oDoc.Bookmarks.Item("\endofdoc").Range)
            'oPara1.Format.LineSpacing.Equals(1)

            'Add Intro paragrapgh at beginning of document
            'oPara1 = oDoc.Content.Paragraphs.Add
            ' oPara1.Range.Font.Bold = False

            msg = "Estimated Environmental Concentrations for " & pesticideName & " are presented in Table 1 for the " & Display(30) &
                " with the " & scenarioID.Text & " field scenario. A graphical presentation of the year-to-year acute values is presented in Figure 1. "
            msg = msg & "These values were generated with the " & Me.Text & ". "
            msg = msg & "Critical input values for the model are summarized in Tables 2 and 3."


            msg = msg & vbNewLine
            Dim aword As String
            If Display(1).checked Then
                aword = " applied to "
            Else
                aword = " produced on "
            End If


            msg = msg & String.Format("This model estimates that about {0,2:G2}% of " & pesticideName & aword &
                                 "the field eventually reaches the water body. ", (Display(13).text * 100))

            Dim highestflag As Integer = 0
            Dim previousMechanism As Integer
            Dim storeprocess As String = ""
            Dim submsg As String = ""

            Dim field2water() As Organizer = {New Organizer("runoff", Display(10).text),
                                              New Organizer("erosion", Display(11).text),
                                              New Organizer("spray drift", Display(12).text)}




            Dim field2waterValues(field2water.Length - 1) As Single
            For i As Integer = 0 To field2water.Length - 1
                field2waterValues(i) = field2water(i).value
            Next

            'Apparently you cant sort a collection directly that is why it is sorted via the new ARRAY of values taken from the collection
            Array.Sort(field2waterValues, field2water)


            previousMechanism = 0

            If Display(13).text > 0.0 Then

                For i As Integer = field2water.Length - 1 To 0 Step -1
                    If field2water(i).value > 0.0 And highestflag = 0 Then
                        highestflag = 1
                        msg = msg & String.Format("The main mechanism of transport from the field to the water body is by {0} ({1,3:G3}% of the total transport)", field2water(i).process, field2water(i).value * 100)
                    ElseIf field2water(i).value > 0.0 And highestflag = 1 Then
                        If previousMechanism = 1 Then
                            submsg = storeprocess
                        ElseIf previousMechanism > 1 Then
                            submsg = submsg & ", " & storeprocess
                        End If
                        previousMechanism = previousMechanism + 1
                        storeprocess = String.Format("{0} ({1,3:G3}%)", field2water(i).process, field2water(i).value * 100)
                    End If
                Next

                If previousMechanism > 0 Then
                    If previousMechanism = 1 Then
                        msg = msg & " followed by " & storeprocess & "."
                    Else
                        msg = msg & ", followed by " & submsg & " and " & storeprocess & "."
                    End If

                Else
                    msg = msg & "."
                End If



            End If

            '************************  WATER COLUMN DISSIPATION  ********************************************
            msg = msg & String.Format("{0}In the water body, pesticide dissipates with an effective water column half-life of {1} days. ", vbNewLine, Display(23).text.trim)
            Dim waterHalflives() As Organizer = {New Organizer("washout", Display(18).text),
                                                 New Organizer("metabolism", Display(19).text),
                                                 New Organizer("hydrolysis", Display(20).text),
                                                 New Organizer("photolysis", Display(21).text),
                                                 New Organizer("volatilization", Display(22).text)}

            msg = msg & "(This value does not include dissipation by transport to the benthic region; it includes only processes that result in removal of pesticide from the complete system.) "

            Dim WaterHalfLifeValues(waterHalflives.Length - 1) As Single
            For i As Integer = 0 To waterHalflives.Length - 1
                WaterHalfLifeValues(i) = waterHalflives(i).value
            Next
            Array.Sort(WaterHalfLifeValues, waterHalflives)

            highestflag = 0
            previousMechanism = 0
            storeprocess = ""
            submsg = ""

            For i As Integer = 0 To waterHalflives.Length - 1
                If waterHalflives(i).value > 0.0 And highestflag = 0 Then
                    highestflag = 1
                    msg = msg & String.Format("The main source of dissipation in the water column is {0} (effective average half-life = {1} days)", waterHalflives(i).process, waterHalflives(i).value)
                ElseIf waterHalflives(i).value > 0.0 And highestflag = 1 Then
                    If previousMechanism = 1 Then
                        submsg = storeprocess
                    ElseIf previousMechanism > 1 Then
                        submsg = submsg & ", " & storeprocess
                    End If
                    previousMechanism = previousMechanism + 1
                    storeprocess = String.Format("{0} ({1} days)", waterHalflives(i).process, waterHalflives(i).value)
                End If
            Next



            Select Case previousMechanism
                Case 1
                    msg = msg & " followed by " & storeprocess & "."
                Case 2

                    msg = msg & " followed by " & submsg & " and " & storeprocess & "."
                Case Is > 2

                    msg = msg & " followed by " & submsg & ", and " & storeprocess & "."
                Case 0
                    msg = msg & "."
                Case Else
                    msg = msg & "."
            End Select





            'If previousMechanism > 0 Then

            '    If previousMechanism = 1 Then
            '        msg = msg & " followed by " & storeprocess & "."
            '    ElseIf previousMechanism = 2 Then
            '        msg = msg & " followed by " & submsg & ", and " & storeprocess & "."

            '    End If


            'Else
            '    msg = msg & "."
            'End If



            '**********************BENTHIC REGION DESCRIPTION********************************
            Dim qualifier As String
            If Display(17).text > 1000 Then
                qualifier = String.Format(" dissipation is negligible ({0} days)", Display(17).text.trim)
            ElseIf Display(17).text > 300 And Display(17).text <= 1000 Then
                qualifier = String.Format(" dissipates very slowly ({0} days)", Display(17).text.trim)
            ElseIf Display(17).text > 60 And Display(17).text <= 300 Then
                qualifier = String.Format(" dissipates slowly ({0} days)", Display(17).text.trim)
            ElseIf Display(17).text > 5 And Display(17).text <= 60 Then
                qualifier = String.Format(" dissipates ({0} days)", Display(17).text.trim)
            ElseIf Display(17).text > 0 And Display(17).text <= 5 Then
                qualifier = String.Format(" dissipates relatively quickly ({0} days)", Display(17).text.trim)
            ElseIf Display(17).text = 0 Then
                qualifier = String.Format(" is stable")
            Else
                qualifier = " dispensation is confusing"
            End If

            msg = msg & vbNewLine & "In the benthic region, pesticide" & qualifier & ". "

            Dim benthicHalflives() As Organizer = {New Organizer("burial", Display(14).text),
                                                   New Organizer("metabolism", Display(15).text),
                                                   New Organizer("hydrolysis", Display(16).text)}

            Dim halflifevalues(benthicHalflives.Length - 1) As Single
            For i As Integer = 0 To benthicHalflives.Length - 1
                halflifevalues(i) = benthicHalflives(i).value
            Next
            'sort benthic half lives based on the half life values:
            Array.Sort(halflifevalues, benthicHalflives)

            highestflag = 0
            previousMechanism = 0
            storeprocess = ""
            submsg = ""

            For i As Integer = 0 To benthicHalflives.Length - 1
                If benthicHalflives(i).value > 0.0 And highestflag = 0 Then
                    highestflag = 1
                    msg = msg & String.Format(
                        "The main source of dissipation in the benthic region is {0} (effective average half-life = {1} days)",
                        benthicHalflives(i).process, benthicHalflives(i).value)
                ElseIf benthicHalflives(i).value > 0.0 And highestflag = 1 Then
                    If previousMechanism = 1 Then
                        submsg = storeprocess
                    ElseIf previousMechanism > 1 Then
                        submsg = submsg & ", " & storeprocess
                    End If
                    previousMechanism = previousMechanism + 1
                    storeprocess = String.Format("{0} ({1} days)", benthicHalflives(i).process, benthicHalflives(i).value)
                End If
            Next

            If previousMechanism > 1 Then
                msg = msg & " followed by " & submsg & " and " & storeprocess & "."
            ElseIf previousMechanism = 1 Then
                msg = msg & " followed by " & storeprocess & "."

            Else
                If highestflag <> 0 Then
                    msg = msg & "."
                End If

            End If



            Dim dist As Single
            dist = Convert.ToSingle(Display(27).text)
            Select Case dist
                Case Is > 0.9
                    submsg = String.Format(
                        " The vast majority of the pesticide in the benthic region ({0,2:G4}%) is in the pore water rather than sorbed to sediment.", dist * 100)
                Case 0.6 To 0.9
                    submsg = String.Format(
                        " Most of the pesticide in the benthic region (about {0,2:G2}%) is in the pore water rather than sorbed to sediment.", dist * 100)
                Case 0.1 To 0.4
                    submsg = String.Format(
                        " Most of the pesticide in the benthic region ({0,2:G2}%) is sorbed to sediment rather than in the pore water.", (1 - dist) * 100)
                Case Is < 0.1
                    submsg = String.Format(
                        " The vast majority of the pesticide in the benthic region ({0,5:G4}%) is sorbed to sediment rather than in the pore water.",
                          (1 - dist) * 100)
                Case Else
                    submsg = " The pesticide is about evenly distributed in the benthic region between the pore water and sorbed to sediment."
            End Select


            msg = msg & submsg



            '********************************************************************************
            oPara1 = oDoc.Content.Paragraphs.Add
            oPara1.Range.Font.Bold = False
            oPara1.Range.Font.Size = 11
            oPara1.Range.Text = msg

            oPara1.Range.InsertParagraphAfter()
            oPara1.Format.SpaceAfter = 4

            oPara2 = oDoc.Content.Paragraphs.Add
            oPara2.Range.Font.Size = 11
            oPara2.Range.Text = vbNewLine & "Table 1. Estimated Environmental Concentrations (ppb) for " & pesticideName & "."
            oPara2.Range.Font.Bold = True

            'TABLE 1. Estimated Environmental Concentrations
            oTable = oDoc.Tables.Add(oDoc.Bookmarks.Item("\endofdoc").Range, 6, 2)
            oTable.Columns.Item(1).Width = oWord.InchesToPoints(2)   'Change width of columns 1 & 2
            oTable.Columns.Item(2).Width = oWord.InchesToPoints(1)
            oTable.Borders.InsideLineStyle = Microsoft.Office.Interop.Word.WdLineStyle.wdLineStyleSingle

            oTable.Borders.OutsideLineStyle = Microsoft.Office.Interop.Word.WdLineStyle.wdLineStyleSingle
            oTable.Borders.Item(1).LineWidth = Microsoft.Office.Interop.Word.WdLineWidth.wdLineWidth150pt
            oTable.Borders.Item(3).LineWidth = Microsoft.Office.Interop.Word.WdLineWidth.wdLineWidth150pt
            oTable.Borders.Item(2).Visible = False
            oTable.Borders.Item(4).Visible = False
            oTable.Range.Font.Size = 11

            oTable.Cell(1, 1).Range.Text = "1-day Avg (1-in-10 yr)"
            oTable.Cell(2, 1).Range.Text = "4-day Avg (1-in-10 yr)"
            oTable.Cell(3, 1).Range.Text = "21-day Avg (1-in-10 yr)"
            oTable.Cell(4, 1).Range.Text = "60-day Avg (1-in-10 yr)"
            oTable.Cell(5, 1).Range.Text = "365-day Avg (1-in-10 yr)"
            oTable.Cell(6, 1).Range.Text = "Entire Simulation Mean"

            oTable.Cell(1, 2).Range.Text = Display(5).text
            oTable.Cell(2, 2).Range.Text = Display(6).Text
            oTable.Cell(3, 2).Range.Text = Display(28).Text
            oTable.Cell(4, 2).Range.Text = Display(29).Text
            oTable.Cell(5, 2).Range.Text = Display(7).Text
            oTable.Cell(6, 2).Range.Text = Display(24).Text

            oTable.Range.Font.Bold = False
            oTable.Range.ParagraphFormat.SpaceAfter = 6

            oPara3 = oDoc.Content.Paragraphs.Add
            oPara3.Range.Font.Size = 11
            oPara3.Range.Text = vbNewLine & "Table 2. Summary of Model Inputs for " & pesticideName & "."
            oPara3.Range.Font.Bold = True

            'TABLE 2. Summary of Model Inputs
            oTable2 = oDoc.Tables.Add(oDoc.Bookmarks.Item("\endofdoc").Range, 13, 2)
            oTable2.Range.Font.Bold = False
            oTable2.Range.Font.Size = 11
            oTable2.Borders.InsideLineStyle = Microsoft.Office.Interop.Word.WdLineStyle.wdLineStyleSingle
            oTable2.Columns.Item(1).Width = oWord.InchesToPoints(2.5)   'Change width of columns 1 & 2
            oTable2.Columns.Item(2).Width = oWord.InchesToPoints(2)

            oTable2.Borders.OutsideLineStyle = Microsoft.Office.Interop.Word.WdLineStyle.wdLineStyleSingle
            oTable2.Borders.Item(1).LineWidth = Microsoft.Office.Interop.Word.WdLineWidth.wdLineWidth150pt
            oTable2.Borders.Item(3).LineWidth = Microsoft.Office.Interop.Word.WdLineWidth.wdLineWidth150pt
            oTable2.Borders.Item(2).Visible = False
            oTable2.Borders.Item(4).Visible = False

            oTable2.Cell(1, 1).Range.Text = "Scenario"
            oTable2.Cell(2, 1).Range.Text = "Cropped Area Fraction"


            If IsKocRadioButton.Checked Then
                oTable2.Cell(3, 1).Range.Text = "Koc (ml/g)"
            Else
                oTable2.Cell(3, 1).Range.Text = "Kd (ml/g)"
            End If


            oTable2.Cell(7, 1).Range.Text = "Hydrolysis Half-Life (days)"

            oTable2.Cell(9, 1).Range.Text = "Foliar Half-Life (days)"
            oTable2.Cell(10, 1).Range.Text = "Molecular Weight"
            oTable2.Cell(11, 1).Range.Text = "Vapor Pressure (torr)"
            oTable2.Cell(12, 1).Range.Text = "Solubility (mg/l)"
            oTable2.Cell(13, 1).Range.Text = "Henry's Constant"



            'temp for soil deg is the same for all, no option in przm
            oTable2.Cell(8, 1).Range.Text = String.Format("Soil Half-Life (days) @ {0} °C", soilTempBox1.Text)

            If Display(1).checked Then
                oTable2.Cell(1, 2).Range.Text = scenarioID.Text

                oTable2.Cell(2, 2).Range.Text = Display(31).text

                oTable2.Cell(3, 2).Range.Text = KocBox1.Text
                oTable2.Cell(4, 2).Range.Text = waterMetabBox1.Text
                oTable2.Cell(5, 2).Range.Text = benthicMetabBox1.Text
                oTable2.Cell(6, 2).Range.Text = photoBox1.Text
                oTable2.Cell(7, 2).Range.Text = hydroBox1.Text
                oTable2.Cell(8, 2).Range.Text = soilDegradation1.Text
                oTable2.Cell(9, 2).Range.Text = foliarDeg1.Text
                oTable2.Cell(10, 2).Range.Text = mwtBox1.Text
                oTable2.Cell(11, 2).Range.Text = vpBox1.Text
                oTable2.Cell(12, 2).Range.Text = solBox1.Text
                oTable2.Cell(13, 2).Range.Text = henry1.Text


                oTable2.Cell(4, 1).Range.Text = String.Format("Water Half-Life (days) @ {0} °C", waterTempBox1.Text)
                oTable2.Cell(5, 1).Range.Text = String.Format("Benthic Half-Life (days) @ {0} °C", benthicTempBox1.Text)
                oTable2.Cell(6, 1).Range.Text = String.Format("Photolysis Half-Life (days) @ {0} °Lat", rlatBox1.Text)


            ElseIf Display(2).checked Then

                oTable2.Cell(1, 2).Range.Text = scenarioID.Text
                oTable2.Cell(2, 2).Range.Text = Display(31).text


                oTable2.Cell(3, 2).Range.Text = KocBox2.Text
                oTable2.Cell(4, 2).Range.Text = waterMetabBox2.Text
                oTable2.Cell(5, 2).Range.Text = benthicMetabBox2.Text
                oTable2.Cell(6, 2).Range.Text = photoBox2.Text
                oTable2.Cell(7, 2).Range.Text = hydroBox2.Text
                oTable2.Cell(8, 2).Range.Text = soilDegradation2.Text
                oTable2.Cell(9, 2).Range.Text = foliarDeg2.Text
                oTable2.Cell(10, 2).Range.Text = mwtBox2.Text
                oTable2.Cell(11, 2).Range.Text = vpBox2.Text
                oTable2.Cell(12, 2).Range.Text = solBox2.Text
                oTable2.Cell(13, 2).Range.Text = henry2.Text



                oTable2.Cell(4, 1).Range.Text = String.Format("Water Half-Life (days) @ {0} °C", waterTempBox2.Text)
                oTable2.Cell(5, 1).Range.Text = String.Format("Benthic Half-Life (days) @ {0} °C", benthicTempBox2.Text)
                oTable2.Cell(6, 1).Range.Text = String.Format("Photolysis Half-Life (days) @ {0} °Lat", rlatBox2.Text)


                Dim extraRow As Integer = 0

                If convertWater1.Text > 0 Then
                    extraRow = extraRow + 1
                    oTable2.Rows.Add()
                    oTable2.Cell(12 + extraRow, 1).Range.Text = "Molar Conversion: Metabolism"
                    oTable2.Cell(12 + extraRow, 2).Range.Text = convertWater1.Text
                End If

                If convertBenthic1.Text > 0 Then
                    extraRow = extraRow + 1
                    oTable2.Rows.Add()
                    oTable2.Cell(12 + extraRow, 1).Range.Text = "Molar Conversion: Benthic"
                    oTable2.Cell(12 + extraRow, 2).Range.Text = convertBenthic1.Text
                End If


                If convertPhoto1.Text > 0 Then
                    extraRow = extraRow + 1
                    oTable2.Rows.Add()
                    oTable2.Cell(12 + extraRow, 1).Range.Text = "Molar Conversion: Photolysis"
                    oTable2.Cell(12 + extraRow, 2).Range.Text = convertPhoto1.Text
                End If

                If convertHydro1.Text > 0 Then
                    extraRow = extraRow + 1
                    oTable2.Rows.Add()
                    oTable2.Cell(12 + extraRow, 1).Range.Text = "Molar Conversion: Hydrolysis"
                    oTable2.Cell(12 + extraRow, 2).Range.Text = convertHydro1.Text
                End If

                If convertSoil1.Text > 0 Then
                    extraRow = extraRow + 1
                    oTable2.Rows.Add()
                    oTable2.Cell(12 + extraRow, 1).Range.Text = "Molar Conversion: Soil Degradation"
                    oTable2.Cell(12 + extraRow, 2).Range.Text = convertSoil1.Text
                End If

                If convertFoliar1.Text > 0 Then
                    extraRow = extraRow + 1
                    oTable2.Rows.Add()
                    oTable2.Cell(12 + extraRow, 1).Range.Text = "Molar Conversion: Foliar"
                    oTable2.Cell(12 + extraRow, 2).Range.Text = convertFoliar1.Text
                End If


            Else
                oTable2.Cell(1, 2).Range.Text = scenarioID.Text
                oTable2.Cell(2, 2).Range.Text = Display(31).text

                oTable2.Cell(3, 2).Range.Text = KocBox3.Text
                oTable2.Cell(4, 2).Range.Text = waterMetabBox3.Text
                oTable2.Cell(5, 2).Range.Text = benthicMetabBox3.Text
                oTable2.Cell(6, 2).Range.Text = photoBox3.Text
                oTable2.Cell(7, 2).Range.Text = hydroBox3.Text
                oTable2.Cell(8, 2).Range.Text = soilDegradation3.Text
                oTable2.Cell(9, 2).Range.Text = foliarDeg3.Text
                oTable2.Cell(10, 2).Range.Text = mwtBox3.Text
                oTable2.Cell(11, 2).Range.Text = vpBox3.Text
                oTable2.Cell(12, 2).Range.Text = solBox3.Text
                oTable2.Cell(13, 2).Range.Text = henry3.Text


                oTable2.Cell(4, 1).Range.Text = String.Format("Water Half-Life (days) @ {0} °C", waterTempBox3.Text)
                oTable2.Cell(5, 1).Range.Text = String.Format("Benthic Half-Life (days) @ {0} °C", benthicTempBox3.Text)
                oTable2.Cell(6, 1).Range.Text = String.Format("Photolysis Half-Life (days) @ {0} °Lat", rlatBox3.Text)


                Dim extraRow As Integer = 0

                If convertWater2.Text > 0 Then
                    extraRow = extraRow + 1
                    oTable2.Rows.Add()
                    oTable2.Cell(12 + extraRow, 1).Range.Text = "Molar Conversion: Metabolism"
                    oTable2.Cell(12 + extraRow, 2).Range.Text = convertWater2.Text
                End If

                If convertBenthic2.Text > 0 Then
                    extraRow = extraRow + 1
                    oTable2.Rows.Add()
                    oTable2.Cell(12 + extraRow, 1).Range.Text = "Molar Conversion: Benthic"
                    oTable2.Cell(12 + extraRow, 2).Range.Text = convertBenthic2.Text
                End If


                If convertPhoto2.Text > 0 Then
                    extraRow = extraRow + 1
                    oTable2.Rows.Add()
                    oTable2.Cell(12 + extraRow, 1).Range.Text = "Molar Conversion: Photolysis"
                    oTable2.Cell(12 + extraRow, 2).Range.Text = convertPhoto2.Text
                End If

                If convertHydro2.Text > 0 Then
                    extraRow = extraRow + 1
                    oTable2.Rows.Add()
                    oTable2.Cell(12 + extraRow, 1).Range.Text = "Molar Conversion: Hydrolysis"
                    oTable2.Cell(12 + extraRow, 2).Range.Text = convertHydro2.Text
                End If

                If convertSoil2.Text > 0 Then
                    extraRow = extraRow + 1
                    oTable2.Rows.Add()
                    oTable2.Cell(12 + extraRow, 1).Range.Text = "Molar Conversion: Soil Degradation"
                    oTable2.Cell(12 + extraRow, 2).Range.Text = convertSoil2.Text
                End If

                If convertFoliar2.Text > 0 Then
                    extraRow = extraRow + 1
                    oTable2.Rows.Add()
                    oTable2.Cell(12 + extraRow, 1).Range.Text = "Molar Conversion: Foliar"
                    oTable2.Cell(12 + extraRow, 2).Range.Text = convertFoliar2.Text
                End If

            End If

            oTable2.Range.ParagraphFormat.SpaceAfter = 6

            If Display(1).checked Then
                oPara5 = oDoc.Content.Paragraphs.Add
                oPara5.Range.Font.Size = 11
                oPara5.Range.Text = vbNewLine & "Table 3. Application Schedule for " & pesticideName & "."
                oPara5.Range.Font.Bold = True


                'TABLE 3. Application Scedule
                oTable3 = oDoc.Tables.Add(oDoc.Bookmarks.Item("\endofdoc").Range, appNumber.Text + 1, 5)
                oTable3.Range.Font.Bold = False
                oTable3.Range.Font.Size = 11
                oTable3.Borders.InsideLineStyle = Microsoft.Office.Interop.Word.WdLineStyle.wdLineStyleSingle
                'oTable3.Columns.Item(1).Width = oWord.InchesToPoints(1)   'Change width of columns 1 & 2
                'oTable3.Columns.Item(2).Width = oWord.InchesToPoints(1)

                oTable3.Borders.OutsideLineStyle = Microsoft.Office.Interop.Word.WdLineStyle.wdLineStyleSingle
                oTable3.Borders.Item(1).LineWidth = Microsoft.Office.Interop.Word.WdLineWidth.wdLineWidth150pt
                oTable3.Borders.Item(3).LineWidth = Microsoft.Office.Interop.Word.WdLineWidth.wdLineWidth150pt
                oTable3.Borders.Item(2).Visible = False
                oTable3.Borders.Item(4).Visible = False


                If AppsAbsolute.Checked Then
                    oTable3.Cell(1, 1).Range.Text = "Date (Mon/Day)"
                Else
                    oTable3.Cell(1, 1).Range.Text = "Date (Days Since Emergence)"
                End If

                oTable3.Cell(1, 2).Range.Text = "Type"
                oTable3.Cell(1, 3).Range.Text = "Amount (kg/ha)"
                oTable3.Cell(1, 4).Range.Text = "Eff."
                oTable3.Cell(1, 5).Range.Text = "Drift"




                Dim methodUsed As String
                For i As Integer = 2 To appNumber.Text + 1


                    If AppsAbsolute.Checked Then

                        oTable3.Cell(i, 1).Range.Text = ApplicationInfo.mon(i - 2).Text & "/" & ApplicationInfo.day(i - 2).Text
                    Else
                        oTable3.Cell(i, 1).Range.Text = ApplicationInfo.RelApp(i - 2).Text
                    End If

                    '   NumberofApplicationTypes()


                    '*****************************************************
                    If ApplicationInfo.methodbutton(ApplicationInfo.NumberofApplicationTypes * (i - 2)).Checked Then
                        methodUsed = "Ground"

                    ElseIf ApplicationInfo.methodbutton(ApplicationInfo.NumberofApplicationTypes * (i - 2) + 1).Checked Then
                        methodUsed = "Above Crop (Foliar)"

                    ElseIf ApplicationInfo.methodbutton(ApplicationInfo.NumberofApplicationTypes * (i - 2) + 2).Checked Then
                        methodUsed = "Uniformly Incorporated to " & ApplicationInfo.DepthIncorp(i - 2).Text & " cm"

                    ElseIf ApplicationInfo.methodbutton(ApplicationInfo.NumberofApplicationTypes * (i - 2) + 3).Checked Then
                        methodUsed = "Placed at a depth of " & ApplicationInfo.DepthIncorp(i - 2).Text & " cm"

                    ElseIf ApplicationInfo.methodbutton(ApplicationInfo.NumberofApplicationTypes * (i - 2) + 4).Checked Then
                        methodUsed = "T-band: top 2 cm fraction =  " & ApplicationInfo.tBand(i - 2).Text & ", depth = " &
                                      ApplicationInfo.DepthIncorp(i - 2).Text & " cm"

                    ElseIf ApplicationInfo.methodbutton(ApplicationInfo.NumberofApplicationTypes * (i - 2) + 5).Checked Then
                        methodUsed = "Linearly increasing to " & ApplicationInfo.DepthIncorp(i - 2).Text & " cm"

                    Else
                        methodUsed = "Linearly decreasing to " & ApplicationInfo.DepthIncorp(i - 2).Text & " cm"

                    End If
                    oTable3.Cell(i, 2).Range.Text = methodUsed
                    '*****************************************************


                    oTable3.Cell(i, 3).Range.Text = ApplicationInfo.rate(i - 2).Text

                    Select Case Display(30)
                        Case "USEPA standard pond"
                            oTable3.Cell(i, 4).Range.Text = ApplicationInfo.Effp(i - 2).Text
                            oTable3.Cell(i, 5).Range.Text = ApplicationInfo.sprayp(i - 2).Text
                        Case "USEPA standard reservoir"
                            oTable3.Cell(i, 4).Range.Text = ApplicationInfo.Effr(i - 2).Text
                            oTable3.Cell(i, 5).Range.Text = ApplicationInfo.sprayr(i - 2).Text
                        Case Else
                            oTable3.Cell(i, 4).Range.Text = ApplicationInfo.Eff(i - 2).Text
                            oTable3.Cell(i, 5).Range.Text = ApplicationInfo.spray(i - 2).Text
                    End Select




                Next

            End If

            'For r As Integer = 1 To 6
            '    For c As Integer = 1 To 2
            '        oTable.Cell(r, c).Range.Font.Bold = False
            '    Next
            'Next

            'oTable.Borders.OutsideLineStyle = Microsoft.Office.Interop.Word.WdLineStyle.wdLineStyleSingle

            'oPara1 = oDoc.Content.Paragraphs.Add(oDoc.Bookmarks.Item("\endofdoc").Range)
            'oPara1.Format.LineSpacing.Equals(1)
            'oPara1.Range.Text = msg

            'oPara1.Format.SpaceAfter = 6
            'oPara1.Range.InsertParagraphAfter()

            'oTable2.Cell(1, 1).Range.Text = "XXXXXXX"
            'oTable2.Range.ParagraphFormat.SpaceAfter = 6
            ' oPara3.Range.InsertParagraphAfter()

            oPara4 = oDoc.Content.Paragraphs.Add
            oPara4.Range.Font.Size = 11
            oPara4.Range.Text = vbNewLine & "Figure 1. Yearly Highest 1-day Average Concentrations"
            oPara4.Range.Font.Bold = True





            Dim stream As New System.IO.MemoryStream()
            ' Save the chart image to the stream    
            Display(0).SaveImage(stream, System.Drawing.Imaging.ImageFormat.Bmp)

            ' Create a bitmap using the stream    
            Dim bmp As New Bitmap(stream)

            ' Save the bitmap to the clipboard    
            Clipboard.SetDataObject(bmp)

            'Add a paragraph
            ' oPara1.Range.InsertParagraphAfter()
            oPara4.Range.InsertParagraphAfter()
            'select paragrapgh and paste your picture from clipboard
            oPara4.Range.Select()

            oWord.Selection.Paste()

        Catch ex As Exception
            MsgBox("Can't generate complete Word document.  Did you run the simulation?")
        End Try



    End Sub

    Structure Organizer
        Public process As String
        Public value As Single

        Sub New(ByVal process As String, ByVal value As Single)
            Me.value = value
            Me.process = process
        End Sub
    End Structure

    Private Sub checkpassword_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles checkpassword.Click
        If passwordAdv.Text = "" Then
            Panel1.Visible = True

            Label112.Visible = True
            Peak90thOutput1.Visible = True

            Label254.Visible = True
            Peak90thOutput2.Visible = True

            Label206.Visible = True
            Peak90thOutput3.Visible = True
        Else
            Panel1.Visible = False

            Label112.Visible = False
            Peak90thOutput1.Visible = False

            Label254.Visible = False
            Peak90thOutput2.Visible = False

            Label206.Visible = False
            Peak90thOutput3.Visible = False

        End If



    End Sub

    Private Sub VaryVolFlow_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles VaryVolFlow.CheckedChanged
        If VaryVolFlow.Checked Then
            ConstVolFlow.Checked = False
            ConstVolNoFlow.Checked = False
        End If

    End Sub


    Private Sub ConstVolNoFlow_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ConstVolNoFlow.CheckedChanged
        If ConstVolNoFlow.Checked Then
            VaryVolFlow.Checked = False
            ConstVolFlow.Checked = False
        End If
    End Sub

    Private Sub ConstVolFlow_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ConstVolFlow.CheckedChanged
        If ConstVolFlow.Checked Then
            VaryVolFlow.Checked = False
            ConstVolNoFlow.Checked = False
        End If

    End Sub

    Private Sub UserSpecifiedFlowAvg_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles UserSpecifiedFlowAvg.CheckedChanged
        If UserSpecifiedFlowAvg.Checked Then
            ReservoirFlowAvgDays.Visible = True
        Else
            ReservoirFlowAvgDays.Visible = False
        End If
    End Sub


    Private Sub EPApond_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles EPApond.CheckedChanged, EPAreservoir.CheckedChanged
        ReservoirAreaLabel.Text = StandardParameters.reservoirArea
        ReservoirDepthLabel.Text = StandardParameters.reservoirDepth
        ReservoirDepthMaxLabel.Text = StandardParameters.reservoirDepthMax
        ReservoirFieldLabel.Text = StandardParameters.reservoirField
        ReservoirHLLabel.Text = StandardParameters.reservoirHL


        PondAreaLabel.Text = StandardParameters.pondArea
        PondDepthLabel.Text = StandardParameters.pondDepth
        PondDepthMaxLabel.Text = StandardParameters.pondDepthMax
        PondFieldLabel.Text = StandardParameters.pondField
        PondHLLabel.Text = StandardParameters.pondHL
        PondCropAreaLabel.Text = StandardParameters.PondCroppedAreaFraction
    End Sub

    Private Sub EstimateHenryConst_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles EstimateHenryConst.Click
        Dim henryConstant As Single

        If MsgBox("Do you want to overwrite the Henry's Law value with an estimate based on the solubility and vapor pressure?", 4, "Overwrite Warning") = 7 Then
            Return
        End If




        If testRealNumbers(vpBox1, True) <> "" Then
            MsgBox("Enter a vapor pressure")
            Exit Sub
        End If

        If testRealNumbers(solBox1, True) <> "" Then
            MsgBox("Enter a solubility")
            Return
        End If

        If testRealNumbers(mwtBox1, True) <> "" Then
            MsgBox("Enter a molecular weight")
            Return
        End If

        henryConstant = Henry.UnitlessVolumetric(vpBox1.Text, solBox1.Text, mwtBox1.Text)
        henry1.Text = String.Format("{0:G3}", henryConstant)

        If Deg1CheckBox.Checked Then
            If testRealNumbers(vpBox2, True) <> "" Then
                MsgBox("Enter a vapor pressure for degradate 1")
                Return
            End If
            If testRealNumbers(solBox2, True) <> "" Then
                MsgBox("Enter a solubility for degradate 1")
                Return
            End If
            If testRealNumbers(mwtBox2, True) <> "" Then
                MsgBox("Enter a molecular weight for degradate 1")
                Return
            End If
            henryConstant = Henry.UnitlessVolumetric(vpBox2.Text, solBox2.Text, mwtBox2.Text)
            henry2.Text = String.Format("{0:G3}", henryConstant)
        End If

        If Deg2CheckBox.Checked Then
            If testRealNumbers(vpBox3, True) <> "" Then
                MsgBox("Enter a vapor pressure for degradate 2")
                Return
            End If
            If testRealNumbers(solBox3, True) <> "" Then
                MsgBox("Enter a solubility for degradate 2")
                Return
            End If
            If testRealNumbers(mwtBox3, True) <> "" Then
                MsgBox("Enter a molecular weight for degradate 2")
                Return
            End If
            henryConstant = Henry.UnitlessVolumetric(vpBox3.Text, solBox3.Text, mwtBox3.Text)
            henry3.Text = String.Format("{0:G3}", henryConstant)


        End If

    End Sub

    Private Sub NodeCalculator_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)

        If testRealNumbers(inputDepth, True) <> "" Then Return

        Try
            nodeBox.Text = SoilProperty.FindNode(inputDepth.Text, numHoriz.Text)
            DepthCalculated.Text = SoilProperty.FindApproximateDepth(inputDepth.Text, numHoriz.Text)
        Catch ex As Exception
            MsgBox("You need to populate the horizons first.")
        End Try


    End Sub


    Private Sub PRZMonly_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles PRZMonly.CheckedChanged
        If PRZMonly.Checked Then
            EPApond.Checked = False
            EPAreservoir.Checked = False
            UserSpecifiedFlowAvg.Checked = False
            VaryVolFlow.Checked = False
            ConstVolNoFlow.Checked = False
            ConstVolFlow.Checked = False
            GroundWater.Checked = False

            EPApond.Enabled = False
            EPAreservoir.Enabled = False
            UserSpecifiedFlowAvg.Enabled = False
            VaryVolFlow.Enabled = False
            ConstVolNoFlow.Enabled = False
            ConstVolFlow.Enabled = False
            GroundWater.Enabled = False
        Else
            EPApond.Enabled = True
            EPAreservoir.Enabled = True
            UserSpecifiedFlowAvg.Enabled = True
            VaryVolFlow.Enabled = True
            ConstVolNoFlow.Enabled = True
            ConstVolFlow.Enabled = True
            GroundWater.Enabled = True
        End If
    End Sub

    Private Sub ChooseBatchChemFile_Click(sender As System.Object, e As System.EventArgs) Handles ChooseBatchChemFile.Click
        Dim result As System.Windows.Forms.DialogResult

        OpenEsaChemicalFile.Filter = "Comma Separated File (*.CSV)|*.CSV|ALL Files (*.*)|*.*"
        '  OpenFileDialog1.ShowDialog()

        result = OpenEsaChemicalFile.ShowDialog(Me)

        'Cancel button will cause return without further execution
        If result = Windows.Forms.DialogResult.Cancel Then
            Return
        End If

        BatchChemFile.Text = System.IO.Path.GetFullPath(OpenEsaChemicalFile.FileName)
        workingDirectoryLabel.Text = System.IO.Path.GetDirectoryName(BatchChemFile.Text) & "\"
        FileNameClass.WorkingDirectory = System.IO.Path.GetDirectoryName(BatchChemFile.Text) & "\"
    End Sub

    Private Sub ChooseBatchScenarioDirectory_Click(sender As System.Object, e As System.EventArgs) Handles ChooseBatchScenarioDirectory.Click
        Dim result As System.Windows.Forms.DialogResult

        result = EsaFolderBrowser.ShowDialog(Me)


        'Cancel button will cuase return without further execution
        If result = Windows.Forms.DialogResult.Cancel Then

            Return
        End If


        EsaBatchscenarioDirectory.Text = EsaFolderBrowser.SelectedPath & "\"


        'Dim selectedScenario As String

        'For Each selectedScenario In OpenScenarioDirectory.FileNames
        '    ' ScenariosList.Items.Add(System.IO.Path.GetFileName(selectedScenario))
        '    'need path
        '    ScenariosList.Items.Add(selectedScenario)
        'Next

        'FileNameClass.previousBatchScenarioPath = System.IO.Path.GetDirectoryName(OpenScenarioDirectory.FileName)

    End Sub




    Private Sub IrrigDepthRootZone_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles IrrigDepthRootZone.CheckedChanged
        If IrrigDepthRootZone.Checked Then
            IrrigationDepthUserSpec.Visible = False
        Else
            IrrigationDepthUserSpec.Visible = True
        End If
    End Sub

    Private Sub MakeNewCurveNumbers()
        Dim outfile As String


        ' outfile = Replace(calibrationFileBox.Text, ".dat", "_CalibrateOut.txt")
        outfile = FileNameClass.PrZMCalibrationOutputFile


        Using reader As New Microsoft.VisualBasic.FileIO.TextFieldParser(outfile)
            Dim currentrow As String()
            Dim DataModelDifference As Single
            Dim direction As Integer
            Dim NewCN As Single

            reader.TextFieldType = Microsoft.VisualBasic.FileIO.FieldType.FixedWidth
            reader.SetFieldWidths(87, 11, -1)

            reader.ReadLine()
            reader.ReadLine()
            reader.ReadLine()
            reader.ReadLine()
            reader.ReadLine()

            For i = 0 To NumberOfFactors.Text - 1
                currentrow = reader.ReadFields()
                DataModelDifference = currentrow(1)

                If DataModelDifference > 0 Then
                    direction = 1
                Else
                    direction = -1
                End If

                If firstrun = False Then
                    'Check if direction has changed
                    If direction + USLE.CNdirection(i) = 0 Then   ' usle.cndirection is the previous direction
                        'change in direction, increase precision level
                        USLE.CNlevel(i) = USLE.CNlevel(i) + 1
                    End If

                    NewCN = Min(Convert.ToSingle(USLE.cn(i).Text) + 1 * direction / (10 ^ USLE.CNlevel(i)), 100.0)
                    NewCN = Max(NewCN, 0.0)
                    USLE.cn(i).Text = NewCN
                End If

                'update the directions
                USLE.CNdirection(i) = direction
            Next


            For i As Integer = 1 To 50
                reader.ReadLine()
            Next

            reader.SetFieldWidths(4, 20)
            For i As Integer = 0 To NumberOfFactors.Text - 1
                currentrow = reader.ReadFields()
                USLE.C(i).Text = currentrow(1)
            Next

        End Using


    End Sub


    Private Sub ResetIteration_Click(sender As Object, e As EventArgs) Handles ResetIteration.Click
        firstrun = True
    End Sub


    Private Sub GetBigCalibFileAndDirectory_Click(sender As Object, e As EventArgs) Handles GetBigCalibFileAndDirectory.Click
        Dim result As System.Windows.Forms.DialogResult

        result = OpenBigCalibrationFileDialog.ShowDialog()


        'Cancel button will cuase return without further execution
        If result = Windows.Forms.DialogResult.Cancel Then
            Return
        End If

        BigCalibrationFile.Text = OpenBigCalibrationFileDialog.FileName

    End Sub


    Private Sub DoBigCalibration(ByRef ThereIsproblem As Boolean, ByRef Parameter1 As TextBox, ByRef Parameter2 As TextBox, ByRef Parameter3 As TextBox, ByVal isRunofforErosion As Boolean)

        ReadCalibrationData.Checked = True

        Using MyReader As New Microsoft.VisualBasic.FileIO.TextFieldParser(BigCalibrationFile.Text)
            Dim inputfile(100) As String
            Dim datafile(100) As String

            Dim currentrow As String()
            Dim depth_start, depth_end, depth_incr As Single
            Dim eff_start, eff_end, eff_incr As Single
            Dim decline_start, decline_end, decline_incr As Single
            Dim relevantDirectory As String
            Dim waterbodytype As String
            Dim runflag As Boolean
            Dim msg, msg2, msg3, msg4, msg5 As String
            Dim outputFile, outputfile2, outputfile3, outputfile4, outputfile5 As String
            Dim ErrorMap, ErrorMap2, ErrorMap3, ErrorMap4, ErrorMap5 As String
            Dim HoldEfficiencies, HoldEfficiencies2, HoldEfficiencies3, HoldEfficiencies4, HoldEfficiencies5 As String

            Dim MapHeader As String
            Dim ErrorMessage As String
            ErrorMessage = ""

            relevantDirectory = System.IO.Path.GetDirectoryName(BigCalibrationFile.Text) & "\"

            outputFile = relevantDirectory & "SummaryCalibration_OverallMass.txt"
            outputfile2 = relevantDirectory & "SummaryCalibration_Events_dataNorm.txt"
            outputfile3 = relevantDirectory & "SummaryCalibration_Events_AppNorm.txt"
            outputfile4 = relevantDirectory & "SummaryCalibration_Events_Unweigted.txt"
            outputfile5 = relevantDirectory & "SummaryCalibration_Events_erosion.txt"


            ErrorMap = relevantDirectory & "ErrorMap_overallMass.txt"
            ErrorMap2 = relevantDirectory & "ErrorMap_dataNorm.txt"
            ErrorMap3 = relevantDirectory & "ErrorMap_AppNorm.txt"
            ErrorMap4 = relevantDirectory & "ErrorMap_Unweigted.txt"
            ErrorMap5 = relevantDirectory & "ErrorMap_erosion.txt"


            MyReader.TextFieldType = FileIO.FieldType.Delimited
            MyReader.SetDelimiters(",")

            currentrow = MyReader.ReadFields()
            depth_start = currentrow(1)
            depth_end = currentrow(2)
            depth_incr = currentrow(3)

            currentrow = MyReader.ReadFields()
            eff_start = currentrow(1)
            eff_end = currentrow(2)
            eff_incr = currentrow(3)

            currentrow = MyReader.ReadFields()
            decline_start = currentrow(1)
            decline_end = currentrow(2)
            decline_incr = currentrow(3)


            waterbodytype = StandardParameters.NoWaterBody

            Dim count As Integer
            count = 0
            While Not MyReader.EndOfData
                currentrow = MyReader.ReadFields()

                inputfile(count) = currentrow(0)
                datafile(count) = currentrow(1)
                count = count + 1
            End While
            count = count - 1

            Dim numberOfDepths As Integer
            Dim numberOfDeclines As Integer
            Dim numberOfEfficincies As Integer

            numberOfDepths = (depth_end - depth_start) / depth_incr
            numberOfDeclines = (decline_end - decline_start) / decline_incr
            numberOfEfficincies = (eff_end - eff_start) / eff_incr

            My.Computer.FileSystem.WriteAllText(ErrorMap, "Error Maps on total Mass in Each Simulation Runoff" & vbNewLine, False, System.Text.Encoding.ASCII)
            My.Computer.FileSystem.WriteAllText(ErrorMap2, "Error Maps of Individual Events with Data Normalization " & vbNewLine, False, System.Text.Encoding.ASCII)
            My.Computer.FileSystem.WriteAllText(ErrorMap3, "Error Maps of Individual Events with Applied Mass Normalization " & vbNewLine, False, System.Text.Encoding.ASCII)
            My.Computer.FileSystem.WriteAllText(ErrorMap4, "Error Maps of Individual Events Unweighted" & vbNewLine, False, System.Text.Encoding.ASCII)
            My.Computer.FileSystem.WriteAllText(ErrorMap5, "Error Maps of Total Mass--EROSION " & vbNewLine, False, System.Text.Encoding.ASCII)


            msg = String.Format("{0}, {1}, {2}{3}", numberOfDepths + 1, numberOfEfficincies + 1, numberOfDeclines + 1, vbNewLine)
            msg = msg & "   Depth,    Effic.,   Decline,  SSQ "

            My.Computer.FileSystem.WriteAllText(outputFile, msg, False, System.Text.Encoding.ASCII)
            My.Computer.FileSystem.WriteAllText(outputfile2, msg, False, System.Text.Encoding.ASCII)
            My.Computer.FileSystem.WriteAllText(outputfile3, msg, False, System.Text.Encoding.ASCII)
            My.Computer.FileSystem.WriteAllText(outputfile4, msg, False, System.Text.Encoding.ASCII)
            My.Computer.FileSystem.WriteAllText(outputfile5, msg, False, System.Text.Encoding.ASCII)


            For extraction_decline As Single = decline_start To decline_end Step decline_incr

                MapHeader = "Decline = " & extraction_decline & vbNewLine & "Depth/Eff"

                For extraction_eff As Single = eff_start To eff_end Step eff_incr
                    MapHeader = MapHeader & String.Format(" {0, 10:F4}", extraction_eff)
                Next
                My.Computer.FileSystem.WriteAllText(ErrorMap, MapHeader, True, System.Text.Encoding.ASCII)
                My.Computer.FileSystem.WriteAllText(ErrorMap2, MapHeader, True, System.Text.Encoding.ASCII)
                My.Computer.FileSystem.WriteAllText(ErrorMap3, MapHeader, True, System.Text.Encoding.ASCII)
                My.Computer.FileSystem.WriteAllText(ErrorMap4, MapHeader, True, System.Text.Encoding.ASCII)
                My.Computer.FileSystem.WriteAllText(ErrorMap5, MapHeader, True, System.Text.Encoding.ASCII)




                For extraction_depth As Single = depth_start To depth_end Step depth_incr
                    HoldEfficiencies = ""
                    HoldEfficiencies2 = ""
                    HoldEfficiencies3 = ""
                    HoldEfficiencies4 = ""
                    HoldEfficiencies5 = ""

                    For extraction_eff As Single = eff_start To eff_end Step eff_incr

                        Calibration.DataCumulativeCalibration = 0.0
                        Calibration.ModelCumulativeCalibration = 0.0

                        Calibration.SSQ_totalFromSimulationMassNormalzed = 0.0

                        Calibration.SSQ_Unweighted = 0.0
                        Calibration.SSQ_AppNormalized = 0.0
                        Calibration.SSQ_DataNormalized = 0.0

                        Calibration.SSQ_Erosion = 0.0




                        'Scenario Loop ***********************************************************
                        For i As Integer = 0 To count

                            ReadInputFile(relevantDirectory & inputfile(i))
                            System.IO.Directory.SetCurrentDirectory(workingDirectoryLabel.Text)
                            calibrationFileBox.Text = relevantDirectory & datafile(i)

                            Parameter1.Text = extraction_depth
                            Parameter2.Text = extraction_eff
                            Parameter3.Text = extraction_decline

                            RunPrograms(waterbodytype, 0, runflag, ErrorMessage)
                            If runflag Then
                                ThereIsproblem = True
                                Exit Sub
                            End If


                            Calibration.SummarizeBigCalibration(NumberOfFactors.Text, isRunofforErosion)

                        Next
                        '*********************************************************************************
                        msg = String.Format("{0}{1,8:F2}, {2,9:F4}, {3,9:F3},  {4,9:G4}", vbNewLine, extraction_depth, extraction_eff, extraction_decline, Calibration.SSQ_totalFromSimulationMassNormalzed)
                        msg2 = String.Format("{0}{1,8:F2}, {2,9:F4}, {3,9:F3},  {4,9:G4}", vbNewLine, extraction_depth, extraction_eff, extraction_decline, Calibration.SSQ_DataNormalized)
                        msg3 = String.Format("{0}{1,8:F2}, {2,9:F4}, {3,9:F3},  {4,9:G4}", vbNewLine, extraction_depth, extraction_eff, extraction_decline, Calibration.SSQ_AppNormalized)
                        msg4 = String.Format("{0}{1,8:F2}, {2,9:F4}, {3,9:F3},  {4,9:G4}", vbNewLine, extraction_depth, extraction_eff, extraction_decline, Calibration.SSQ_Unweighted)
                        msg5 = String.Format("{0}{1,8:F2}, {2,9:F4}, {3,9:F3},  {4,9:G4}", vbNewLine, extraction_depth, extraction_eff, extraction_decline, Calibration.SSQ_Erosion)


                        '      msg = String.Format("{0}{1,8:F2}, {2,8}, {3,8}, {4,8}, {5,8}, {6,8}", vbNewLine, extraction_depth, extraction_eff, extraction_decline, Calibration.ModelCumulativeCalibration, Calibration.DataCumulativeCalibration, (Calibration.ModelCumulativeCalibration - Calibration.DataCumulativeCalibration))
                        My.Computer.FileSystem.WriteAllText(outputFile, msg, True, System.Text.Encoding.ASCII)
                        My.Computer.FileSystem.WriteAllText(outputfile2, msg2, True, System.Text.Encoding.ASCII)
                        My.Computer.FileSystem.WriteAllText(outputfile3, msg3, True, System.Text.Encoding.ASCII)
                        My.Computer.FileSystem.WriteAllText(outputfile4, msg4, True, System.Text.Encoding.ASCII)
                        My.Computer.FileSystem.WriteAllText(outputfile5, msg5, True, System.Text.Encoding.ASCII)


                        HoldEfficiencies = HoldEfficiencies & String.Format("{0, 10:G4} ", Calibration.SSQ_totalFromSimulationMassNormalzed)
                        HoldEfficiencies2 = HoldEfficiencies2 & String.Format("{0, 10:G4} ", Calibration.SSQ_DataNormalized)
                        HoldEfficiencies3 = HoldEfficiencies3 & String.Format("{0, 10:G4} ", Calibration.SSQ_AppNormalized)
                        HoldEfficiencies4 = HoldEfficiencies4 & String.Format("{0, 10:G4} ", Calibration.SSQ_Unweighted)
                        HoldEfficiencies5 = HoldEfficiencies5 & String.Format("{0, 10:G4} ", Calibration.SSQ_Erosion)

                    Next
                    msg = String.Format("{0}{1,9:F2} {2}", vbNewLine, extraction_depth, HoldEfficiencies)
                    msg2 = String.Format("{0}{1,9:F2} {2}", vbNewLine, extraction_depth, HoldEfficiencies2)
                    msg3 = String.Format("{0}{1,9:F2} {2}", vbNewLine, extraction_depth, HoldEfficiencies3)
                    msg4 = String.Format("{0}{1,9:F2} {2}", vbNewLine, extraction_depth, HoldEfficiencies4)
                    msg5 = String.Format("{0}{1,9:F2} {2}", vbNewLine, extraction_depth, HoldEfficiencies5)

                    My.Computer.FileSystem.WriteAllText(ErrorMap, msg, True, System.Text.Encoding.ASCII)
                    My.Computer.FileSystem.WriteAllText(ErrorMap2, msg2, True, System.Text.Encoding.ASCII)
                    My.Computer.FileSystem.WriteAllText(ErrorMap3, msg3, True, System.Text.Encoding.ASCII)
                    My.Computer.FileSystem.WriteAllText(ErrorMap4, msg4, True, System.Text.Encoding.ASCII)
                    My.Computer.FileSystem.WriteAllText(ErrorMap5, msg5, True, System.Text.Encoding.ASCII)

                Next

                My.Computer.FileSystem.WriteAllText(ErrorMap, vbNewLine, True, System.Text.Encoding.ASCII) ' add space
                My.Computer.FileSystem.WriteAllText(ErrorMap2, vbNewLine, True, System.Text.Encoding.ASCII) ' add space
                My.Computer.FileSystem.WriteAllText(ErrorMap3, vbNewLine, True, System.Text.Encoding.ASCII) ' add space
                My.Computer.FileSystem.WriteAllText(ErrorMap4, vbNewLine, True, System.Text.Encoding.ASCII) ' add space
                My.Computer.FileSystem.WriteAllText(ErrorMap5, vbNewLine, True, System.Text.Encoding.ASCII) ' add space

            Next

        End Using


    End Sub


    Private Sub CopyGraph_Click(sender As Object, e As EventArgs) Handles CopyGraph.Click


        ' Create a memory stream to save the chart image    
        Dim stream As New System.IO.MemoryStream()

        ' Save the chart image to the stream    
        GWChart.SaveImage(stream, System.Drawing.Imaging.ImageFormat.Bmp)

        ' Create a bitmap using the stream    
        Dim bmp As New Bitmap(stream)

        ' Save the bitmap to the clipboard    
        Clipboard.SetDataObject(bmp)

    End Sub


    Private Sub RelToEmerge_Click(sender As Object, e As EventArgs) Handles RelToEmerge.Click
        ApplicationInfo.EmergenceMaturityHarvest = "Emergence"
        relativelabel.Text = "Days Since" & vbNewLine & ApplicationInfo.EmergenceMaturityHarvest
    End Sub

    Private Sub RelToMat_Click(sender As Object, e As EventArgs) Handles RelToMat.Click
        ApplicationInfo.EmergenceMaturityHarvest = "Maturity"
        relativelabel.Text = "Days Since" & vbNewLine & ApplicationInfo.EmergenceMaturityHarvest
    End Sub

    Private Sub RelToHarv_Click(sender As Object, e As EventArgs) Handles RelToHarv.Click
        ApplicationInfo.EmergenceMaturityHarvest = "Harvest"
        relativelabel.Text = "Days Since" & vbNewLine & ApplicationInfo.EmergenceMaturityHarvest
    End Sub

    Private Sub evergreen_CheckedChanged(sender As Object, e As EventArgs) Handles evergreen.CheckedChanged
        If evergreen.Checked Then
            altGrowthTime.Visible = False
            altCropParameters.Visible = True
            singleYearCropPanel.Visible = False
        End If
    End Sub

    Private Sub greaterThanAnualGrowth_CheckedChanged(sender As Object, e As EventArgs) Handles greaterThanAnualGrowth.CheckedChanged
        If greaterThanAnualGrowth.Checked Then
            altGrowthTime.Visible = True
            altCropParameters.Visible = True
            singleYearCropPanel.Visible = False
        End If
    End Sub

    Private Sub lessThanAnnualGrowth_CheckedChanged(sender As Object, e As EventArgs) Handles lessThanAnnualGrowth.CheckedChanged
        If lessThanAnnualGrowth.Checked Then
            altGrowthTime.Visible = False
            altCropParameters.Visible = False
            singleYearCropPanel.Visible = True
        End If
    End Sub

    Private Sub simpleRB_CheckedChanged(sender As Object, e As EventArgs)

        If simpleRB.Checked Then


        End If



    End Sub

    Private Sub UpdateCropCycleButton_Click(sender As Object, e As EventArgs) Handles UpdateCropCycleButton.Click

        updateCropCycles()
    End Sub


    Private Sub updateCropCycles()

        If Not IsNumeric(CropCyclesPerYear.Text) Then
            CropCyclesPerYear.Text = 1
        End If

        If Convert.ToInt16(CropCyclesPerYear.Text) > CropProperties.maxCropCycles Then
            CropCyclesPerYear.Text = CropProperties.maxCropCycles
        End If





        For i As Integer = 0 To CropCyclesPerYear.Text - 1
            CropProperties.emergenceDay(i).Visible = True
            CropProperties.emergenceMonth(i).Visible = True

            CropProperties.maturityDay(i).Visible = True
            CropProperties.maturityMonth(i).Visible = True

            CropProperties.harvestDay(i).Visible = True
            CropProperties.harvestMonth(i).Visible = True

            CropProperties.rootDepth(i).Visible = True
            CropProperties.canopyCover(i).Visible = True
            CropProperties.canopyHeight(i).Visible = True
            CropProperties.canopyHoldup(i).Visible = True
            CropProperties.foliarPanel(i).Visible = True
            CropProperties.plantingLag(i).Visible = True
            CropProperties.plantingFrequency(i).Visible = True

        Next


        For i As Integer = CropCyclesPerYear.Text To 6
            CropProperties.emergenceDay(i).Visible = False
            CropProperties.emergenceMonth(i).Visible = False

            CropProperties.maturityDay(i).Visible = False
            CropProperties.maturityMonth(i).Visible = False

            CropProperties.harvestDay(i).Visible = False
            CropProperties.harvestMonth(i).Visible = False

            CropProperties.rootDepth(i).Visible = False
            CropProperties.canopyCover(i).Visible = False
            CropProperties.canopyHeight(i).Visible = False
            CropProperties.canopyHoldup(i).Visible = False
            CropProperties.foliarPanel(i).Visible = False
            CropProperties.plantingLag(i).Visible = False
            CropProperties.plantingFrequency(i).Visible = False
        Next




    End Sub

    Private Sub simpleRB_CheckedChanged_1(sender As Object, e As EventArgs) Handles simpleRB.CheckedChanged


        If simpleRB.Checked Then
            lessThanAnnualGrowth.Checked = True
            lessThanAnnualGrowth.Visible = False
            greaterThanAnualGrowth.Visible = False
            evergreen.Visible = False

            CropCyclesPerYear.Text = 1
            PlantFreq1.Text = 1
            Lag1.Text = 0

            updateCropCycles()

            PlantFreq1.Visible = False
            Lag1.Visible = False
            Label324.Visible = False
            Label325.Visible = False

            Label326.Visible = False
            CropCyclesPerYear.Visible = False
            UpdateCropCycleButton.Visible = False

        Else

            lessThanAnnualGrowth.Checked = True
            lessThanAnnualGrowth.Visible = True
            greaterThanAnualGrowth.Visible = True
            evergreen.Visible = True

            PlantFreq1.Visible = True
            Lag1.Visible = True
            Label324.Visible = True
            Label325.Visible = True

            Label326.Visible = True
            CropCyclesPerYear.Visible = True
            UpdateCropCycleButton.Visible = True
        End If




    End Sub

    Private Sub AdvancedRB_CheckedChanged(sender As Object, e As EventArgs) Handles AdvancedRB.CheckedChanged
        Label324.Visible = True
        Label325.Visible = True

        '  updateCropCycles()

    End Sub
    Private Sub getBatchInfo(ByVal ofile As Microsoft.VisualBasic.FileIO.TextFieldParser)
        Dim currentRow As String()
        Dim scenario As String

        ClearAllScenarios.PerformClick()

        ofile.ReadLine()  ' line with ******** start of batch info******************



        BatchChemFile.Text = ofile.ReadLine()
        EsaBatchscenarioDirectory.Text = ofile.ReadLine()

        currentRow = ofile.ReadFields
        appFrequency.Text = currentRow(0)
        firstAppYear.Text = currentRow(1)
        lastAppyear.Text = currentRow(2)
        ApplyWindow.Checked = currentRow(3)
        SpanTB.Text = currentRow(4)
        IntervalTB.Text = currentRow(5)



        currentRow = ofile.ReadFields
        BatchRun.Checked = currentRow(0)
        ExternalBatchFileRun.Checked = currentRow(2)
        isMinimumBatchOutput.Checked = currentRow(3)


        For i As Int16 = 0 To currentRow(1) - 1
            scenario = ofile.ReadLine()

            ScenariosList.Items.Add(scenario)
        Next






    End Sub

    Private Sub getPRZMinfo_2(ByVal ofile As Microsoft.VisualBasic.FileIO.TextFieldParser)
        'PWC  Version of input File
        Dim test As String
        Dim currentRow As String()
        Const numtypes As Integer = 3
        Dim j As Integer

        ofile.ReadLine()  ' line with ******** start of PRZM information ******************

        'select thye radio  button for crop cycle
        currentRow = ofile.ReadFields
        lessThanAnnualGrowth.Checked = currentRow(0)
        evergreen.Checked = currentRow(1)
        greaterThanAnualGrowth.Checked = currentRow(2)

        CropCyclesPerYear.Text = ofile.ReadLine()

        currentRow = ofile.ReadFields
        simpleRB.Checked = currentRow(0)

        Dim postharvdisp As Integer
        For i = 0 To 6

            currentRow = ofile.ReadFields


            CropProperties.emergenceDay(i).Text = currentRow(0)
            CropProperties.emergenceMonth(i).Text = currentRow(1)
            CropProperties.maturityDay(i).Text = currentRow(2)
            CropProperties.maturityMonth(i).Text = currentRow(3)
            CropProperties.harvestDay(i).Text = currentRow(4)
            CropProperties.harvestMonth(i).Text = currentRow(5)
            CropProperties.rootDepth(i).Text = currentRow(6)
            CropProperties.canopyCover(i).Text = currentRow(7)
            CropProperties.canopyHeight(i).Text = currentRow(8)
            CropProperties.canopyHoldup(i).Text = currentRow(9)

            If IsNumeric(currentRow(10)) Then
                postharvdisp = currentRow(10)
            Else
                postharvdisp = 0
            End If

            j = i * numtypes
            Select Case postharvdisp

                Case 1
                    CropProperties.foliarDisposition(j).Checked = True
                    CropProperties.foliarDisposition(j + 1).Checked = False
                    CropProperties.foliarDisposition(j + 2).Checked = False
                Case 2
                    CropProperties.foliarDisposition(j).Checked = False
                    CropProperties.foliarDisposition(j + 1).Checked = True
                    CropProperties.foliarDisposition(j + 2).Checked = False
                Case Else
                    CropProperties.foliarDisposition(j).Checked = False
                    CropProperties.foliarDisposition(j + 1).Checked = False
                    CropProperties.foliarDisposition(j + 2).Checked = True
            End Select

            CropProperties.plantingFrequency(i).Text = currentRow(11)
            CropProperties.plantingLag(i).Text = currentRow(12)

        Next


        currentRow = ofile.ReadFields
        altRootDepth.Text = currentRow(0)
        altCanopyCover.Text = currentRow(1)
        altCanopyHeight.Text = currentRow(2)
        altCanopyHoldup.Text = currentRow(3)

        currentRow = ofile.ReadFields
        altEmergeDay.Text = currentRow(0)
        altEmergeMon.Text = currentRow(1)
        altDaysToMaturity.Text = currentRow(2)
        altDaysToHarvest.Text = currentRow(3)

        Try
            currentRow = ofile.ReadFields
            pfac.Text = currentRow(0)
            snowmelt.Text = currentRow(1)
            evapDepth.Text = currentRow(2)

            ofile.ReadLine() '*** irrigation information start ***


            test = ofile.ReadLine()


            Select Case test
                Case "0"
                    noIrrigation.Checked = True
                Case "1"
                    overCanopy.Checked = True
                Case "2"
                    underCanopy.Checked = True
                Case Else
                    noIrrigation.Checked = True
            End Select


            currentRow = ofile.ReadFields

            fleach.Text = currentRow(0)
            depletion.Text = currentRow(1)
            rateIrrig.Text = currentRow(2)

            currentRow = ofile.ReadFields
            UserSpecifiesIrrigDepth.Checked = currentRow(0)
            IrrigDepthRootZone.Checked = Not UserSpecifiesIrrigDepth.Checked
            IrrigationDepthUserSpec.Text = currentRow(1)

        Catch ex As Exception
            MsgBox("Error in the irrigation paranmeters you selected")
        End Try

        ofile.ReadLine()
        ofile.ReadLine()
        ofile.ReadLine()


        Try

            currentRow = ofile.ReadFields
            uslek.Text = currentRow(0)
            uslels.Text = currentRow(1)
            uslep.Text = currentRow(2)

            currentRow = ofile.ReadFields
            ireg.Text = currentRow(0)
            slope.Text = currentRow(1)
            hl.Text = currentRow(2)

            ofile.ReadLine() 'this is: *** Horizon Info *******

            numHoriz.Text = ofile.ReadLine()
            updatehorizonsub()


            currentRow = ofile.ReadFields
            For i As Integer = 0 To numHoriz.Text - 1
                SoilProperty.thick(i).Text = currentRow(i)
            Next
            currentRow = ofile.ReadFields
            For i As Integer = 0 To numHoriz.Text - 1
                SoilProperty.bulkden(i).Text = currentRow(i)
            Next
            currentRow = ofile.ReadFields
            For i As Integer = 0 To numHoriz.Text - 1
                SoilProperty.maxcap(i).Text = currentRow(i)
            Next
            currentRow = ofile.ReadFields
            For i As Integer = 0 To numHoriz.Text - 1
                SoilProperty.mincap(i).Text = currentRow(i)
            Next
            currentRow = ofile.ReadFields
            For i As Integer = 0 To numHoriz.Text - 1
                SoilProperty.oc(i).Text = currentRow(i)
            Next
            currentRow = ofile.ReadFields
            For i As Integer = 0 To numHoriz.Text - 1
                SoilProperty.compartment(i).Text = currentRow(i)
            Next
            currentRow = ofile.ReadFields
            For i As Integer = 0 To numHoriz.Text - 1
                SoilProperty.sand(i).Text = currentRow(i)
            Next

            currentRow = ofile.ReadFields
            For i As Integer = 0 To numHoriz.Text - 1
                SoilProperty.clay(i).Text = currentRow(i)
            Next

            ofile.ReadLine() 'this is: *** Horizon Info *******

            currentRow = ofile.ReadFields
            albedoBox.Text = currentRow(0)
            bcTemp.Text = currentRow(1)

            Try
                Dim tempsim As String
                tempsim = ofile.ReadLine()
                simTemperature.Checked = Convert.ToBoolean(tempsim)
            Catch ex As Exception
                MsgBox(ex.Message)
            End Try

            ofile.ReadLine()  '***spare line for expansion
            ofile.ReadLine()  '***spare line for expansion
            ofile.ReadLine()  '***Erosion Curve Number Inputs

            '****** READ IN USLE and CN Values ************************

            'First Clear all the USLE/CN numbers from previous run
            For i As Integer = 0 To USLE.MaxHydroErosionFactors - 1
                USLE.day(i).Text = ""
                USLE.mon(i).Text = ""
                USLE.cn(i).Text = ""
                USLE.C(i).Text = ""
                USLE.n(i).Text = ""
                USLE.year(i).Text = ""
            Next

            NumberOfFactors.Text = ofile.ReadLine()

            'Added this definition and conditions to avoid failures when developing scenarios that are still incomplete
            Dim NumberOfFactorsInteger As Integer
            If IsNumeric(NumberOfFactors.Text) Then
                NumberOfFactorsInteger = NumberOfFactors.Text - 1
                If NumberOfFactorsInteger > USLE.MaxHydroErosionFactors - 1 Then
                    NumberOfFactorsInteger = USLE.MaxHydroErosionFactors - 1
                    NumberOfFactors.Text = USLE.MaxHydroErosionFactors
                End If

            Else
                NumberOfFactorsInteger = -1
            End If

            currentRow = ofile.ReadFields
            For i As Integer = 0 To NumberOfFactorsInteger
                USLE.day(i).Text = currentRow(i)
            Next

            currentRow = ofile.ReadFields
            For i As Integer = 0 To NumberOfFactorsInteger
                USLE.mon(i).Text = currentRow(i)
            Next

            currentRow = ofile.ReadFields
            For i As Integer = 0 To NumberOfFactorsInteger
                USLE.cn(i).Text = currentRow(i)
            Next

            currentRow = ofile.ReadFields
            For i As Integer = 0 To NumberOfFactorsInteger
                USLE.C(i).Text = currentRow(i)
            Next

            currentRow = ofile.ReadFields
            For i As Integer = 0 To NumberOfFactorsInteger
                USLE.n(i).Text = currentRow(i)
            Next

            '******************************************************************************

            '******************************************************************************
            currentRow = ofile.ReadFields
            rDepthBox.Text = currentRow(0)
            rDeclineBox.Text = currentRow(1)
            rBypassBox.Text = currentRow(2)

            currentRow = ofile.ReadFields
            eDepthBox.Text = currentRow(0)
            eDeclineBox.Text = currentRow(1)
            If currentRow(2).Trim = "" Then
                eBypassBox.Text = "1.0"
            Else
                eBypassBox.Text = currentRow(2)
            End If

            useUSLEYear.Checked = ofile.ReadLine()
            currentRow = ofile.ReadFields
            For i As Integer = 0 To NumberOfFactorsInteger
                USLE.year(i).Text = currentRow(i)
            Next

            volatilizationBoundaryBox.Text = ofile.ReadLine()



        Catch ex As Exception
            MsgBox(ex.Message & " Problem in reading USLE variable or runoff parameters of scenario")
        End Try


    End Sub


    Private Function PRZMinfo_V2()
        'returns msg
        Dim msg As String
        msg = "******** start of PRZM information ******************" & vbNewLine
        msg = msg & String.Format("{0},{1},{2},", lessThanAnnualGrowth.Checked, evergreen.Checked, greaterThanAnualGrowth.Checked)
        msg = msg & String.Format("{0}{1}", vbNewLine, CropCyclesPerYear.Text)
        msg = msg & String.Format("{0}{1},", vbNewLine, simpleRB.Checked)


        Dim foliarDispositionCode(6) As Int16
        Dim numTypes As Int16  ' the number of ways the pesticide can be distributed after harvest, currently 3
        numTypes = 3
        For i As Integer = 0 To 6
            If CropProperties.foliarDisposition(i * numTypes).Checked Then
                foliarDispositionCode(i) = 1
            ElseIf CropProperties.foliarDisposition(i * numTypes + 1).Checked Then
                foliarDispositionCode(i) = 2
            Else
                foliarDispositionCode(i) = 3
            End If
        Next

        For i As Integer = 0 To 6
            msg = msg & String.Format("{0}{1},{2},{3},{4},{5},{6},{7},{8},{9},{10},{11},{12},{13}", vbNewLine,
                                      CropProperties.emergenceDay(i).Text, CropProperties.emergenceMonth(i).Text,
                                      CropProperties.maturityDay(i).Text, CropProperties.maturityMonth(i).Text,
                                      CropProperties.harvestDay(i).Text, CropProperties.harvestMonth(i).Text,
                                      CropProperties.rootDepth(i).Text, CropProperties.canopyCover(i).Text, CropProperties.canopyHeight(i).Text, CropProperties.canopyHoldup(i).Text,
                                      foliarDispositionCode(i), CropProperties.plantingFrequency(i).Text, CropProperties.plantingLag(i).Text)
        Next

        msg = msg & String.Format("{0}{1},{2},{3},{4}", vbNewLine, altRootDepth.Text, altCanopyCover.Text, altCanopyHeight.Text, altCanopyHoldup.Text)

        msg = msg & String.Format("{0}{1},{2},{3},{4}", vbNewLine, altEmergeDay.Text, altEmergeMon.Text, altDaysToMaturity.Text, altDaysToHarvest.Text)


        msg = msg & String.Format("{0}{1},{2},{3}", vbNewLine, pfac.Text, snowmelt.Text, evapDepth.Text)


        msg = msg & vbNewLine & "*** irrigation information start ***"
        If noIrrigation.Checked Then
            msg = msg & String.Format("{0}0", vbNewLine)

        ElseIf overCanopy.Checked Then
            msg = msg & String.Format("{0}1", vbNewLine)

        ElseIf underCanopy.Checked Then
            msg = msg & String.Format("{0}2", vbNewLine)
        Else
            msg = msg & String.Format("{0}0", vbNewLine)
        End If

        msg = msg & String.Format("{0}{1},{2},{3}", vbNewLine, fleach.Text, depletion.Text, rateIrrig.Text)
        msg = msg & String.Format("{0}{1},{2}", vbNewLine, UserSpecifiesIrrigDepth.Checked, IrrigationDepthUserSpec.Text)
        msg = msg & vbNewLine & "*** spare line for expansion"
        msg = msg & vbNewLine & "*** spare line for expansion"
        msg = msg & vbNewLine & "*** Soil Information ***"

        msg = msg & String.Format("{0}{1},{2},{3}", vbNewLine, uslek.Text, uslels.Text, uslep.Text)
        msg = msg & String.Format("{0}{1},{2},{3}", vbNewLine, ireg.Text, slope.Text, hl.Text)


        msg = msg & vbNewLine & "*** Horizon Info *******" '



        msg = msg & String.Format("{0}{1}{0}", vbNewLine, numHoriz.Text)


        updatehorizonsub()
        For i As Integer = 0 To numHoriz.Text - 1
            msg = msg & String.Format("{0},", SoilProperty.thick(i).Text)
        Next
        msg = msg & vbNewLine
        For i As Integer = 0 To numHoriz.Text - 1
            msg = msg & String.Format("{0},", SoilProperty.bulkden(i).Text)
        Next
        msg = msg & vbNewLine
        For i As Integer = 0 To numHoriz.Text - 1
            msg = msg & String.Format("{0},", SoilProperty.maxcap(i).Text)
        Next
        msg = msg & vbNewLine
        For i As Integer = 0 To numHoriz.Text - 1
            msg = msg & String.Format("{0},", SoilProperty.mincap(i).Text)
        Next
        msg = msg & vbNewLine
        For i As Integer = 0 To numHoriz.Text - 1
            msg = msg & String.Format("{0},", SoilProperty.oc(i).Text)
        Next
        msg = msg & vbNewLine
        For i As Integer = 0 To numHoriz.Text - 1
            msg = msg & String.Format("{0},", SoilProperty.compartment(i).Text)
        Next
        msg = msg & vbNewLine
        For i As Integer = 0 To numHoriz.Text - 1
            msg = msg & String.Format("{0},", SoilProperty.sand(i).Text)
        Next
        msg = msg & vbNewLine
        For i As Integer = 0 To numHoriz.Text - 1
            msg = msg & String.Format("{0},", SoilProperty.clay(i).Text)
        Next


        msg = msg & vbNewLine & "*** Horizon End, Temperature Start ********"

        msg = msg & String.Format("{0}{1},{2}", vbNewLine, albedoBox.Text, bcTemp.Text)
        msg = msg & vbNewLine & simTemperature.Checked
        msg = msg & vbNewLine & "***spare line for expansion"
        msg = msg & vbNewLine & "***spare line for expansion"
        msg = msg & vbNewLine & "*** Erosion & Curve Number Info **********"

        msg = msg & vbNewLine & NumberOfFactors.Text


        msg = msg & vbNewLine
        For i As Integer = 0 To USLE.MaxHydroErosionFactors - 1
            msg = msg & USLE.day(i).Text & ","
        Next

        msg = msg & vbNewLine

        For i As Integer = 0 To USLE.MaxHydroErosionFactors - 1
            msg = msg & USLE.mon(i).Text & ","
        Next

        msg = msg & vbNewLine
        For i As Integer = 0 To USLE.MaxHydroErosionFactors - 1
            msg = msg & USLE.cn(i).Text & ","
        Next

        msg = msg & vbNewLine
        For i As Integer = 0 To USLE.MaxHydroErosionFactors - 1
            msg = msg & USLE.C(i).Text & ","
        Next

        msg = msg & vbNewLine
        For i As Integer = 0 To USLE.MaxHydroErosionFactors - 1
            msg = msg & USLE.n(i).Text & ","
        Next

        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, rDepthBox.Text, rDeclineBox.Text, rBypassBox.Text)
        msg = msg & String.Format("{0}{1},{2},{3},", vbNewLine, eDepthBox.Text, eDeclineBox.Text, eBypassBox.Text)


        msg = msg & vbNewLine & useUSLEYear.Checked & vbNewLine
        For i As Integer = 0 To USLE.MaxHydroErosionFactors - 1
            msg = msg & USLE.year(i).Text & ","
        Next

        msg = msg & vbNewLine & volatilizationBoundaryBox.Text

        Return msg
    End Function


    Private Function SaveBatchRunInfo() As String
        Dim i As Integer
        Dim listOfScenarios As String
        i = 0
        listOfScenarios = ""

        For Each Scenario As String In ScenariosList.Items
            listOfScenarios = listOfScenarios & vbNewLine & Scenario
            i = i + 1
        Next


        SaveBatchRunInfo = "**** Start of Batch Run Information ******"
        SaveBatchRunInfo = SaveBatchRunInfo & vbNewLine & BatchChemFile.Text
        SaveBatchRunInfo = SaveBatchRunInfo & vbNewLine & EsaBatchscenarioDirectory.Text
        SaveBatchRunInfo = SaveBatchRunInfo & vbNewLine & String.Format("{0},{1},{2},{3},{4},{5},", appFrequency.Text, firstAppYear.Text, lastAppyear.Text, ApplyWindow.Checked, SpanTB.Text, IntervalTB.Text)
        SaveBatchRunInfo = SaveBatchRunInfo & vbNewLine & BatchRun.Checked & "," & i & "," & ExternalBatchFileRun.Checked & "," & isMinimumBatchOutput.Checked & ","

        SaveBatchRunInfo = SaveBatchRunInfo & listOfScenarios



    End Function


    Private Sub RetrieveScenarioMenuItem_Click(sender As Object, e As EventArgs) Handles RetrieveScenarioMenuItem.Click

        RetrieveScenarioDialog.Filter = "Scenario Files V2 (*.SCN2)|*.SCN2| Scenario Files (*.SCN)|*.SCN"
        '  OpenFileDialog1.ShowDialog()

        If System.IO.Directory.Exists(FileNameClass.previousScenarioPath) Then
            RetrieveScenarioDialog.InitialDirectory = FileNameClass.previousScenarioPath
        End If

        Dim result As System.Windows.Forms.DialogResult
        result = RetrieveScenarioDialog.ShowDialog(Me)
        'Cancel button will cuase return without further execution
        If result = Windows.Forms.DialogResult.Cancel Then
            Return
        End If

        FileNameClass.previousScenarioPath = System.IO.Path.GetDirectoryName(RetrieveScenarioDialog.FileName)

        FileNameClass.PreviousScenarioFile = RetrieveScenarioDialog.FileName

        LoadScenario(RetrieveScenarioDialog.FileName)

    End Sub

    Private Sub Panel4_MouseMove(sender As Object, e As MouseEventArgs)
        Label254.ForeColor = Color.Black
        Peak90thOutput2.ForeColor = Color.Black
    End Sub

    Private Sub Panel4_MouseLeave(sender As Object, e As EventArgs)
        Label254.ForeColor = Color.SeaShell
        Peak90thOutput2.ForeColor = Color.SeaShell
    End Sub


    Private Sub ReservoirRB_CheckedChanged(sender As Object, e As EventArgs) Handles ReservoirRB.CheckedChanged
        High1day.Text = eec1Res
        Scenario1day.Text = scenario1Res
        High4day.Text = eec4Res
        Scenario4day.Text = scenario4Res
        High21day.Text = eec21Res
        Scenario21day.Text = scenario21Res
        High60day.Text = eec60Res
        Scenario60day.Text = scenario60Res

        High365day.Text = eec365Res
        Scenario365day.Text = scenario365Res
        HighOverall.Text = eecFullRes
        ScenarioOverall.Text = scenarioFullRes
        High1dayBenthic.Text = eecBenthic1Res
        Scenario1dayBenthic.Text = scenarioBenthic1Res
        High21dayBenthic.Text = eecbenthic21Res
        Scenario21dayBenthic.Text = scenarioBenthic1Res
    End Sub

    Private Sub PondRB_CheckedChanged(sender As Object, e As EventArgs) Handles PondRB.CheckedChanged
        High1day.Text = eec1Pond
        Scenario1day.Text = scenario1Pond
        High4day.Text = eec4Pond
        Scenario4day.Text = scenario4Pond
        High21day.Text = eec21Pond
        Scenario21day.Text = scenario21Pond
        High60day.Text = eec60Pond
        Scenario60day.Text = scenario60Pond

        High365day.Text = eec365Pond
        Scenario365day.Text = scenario365Pond
        HighOverall.Text = eecFullPond
        ScenarioOverall.Text = scenarioFullPond
        High1dayBenthic.Text = eecBenthic1Pond
        Scenario1dayBenthic.Text = scenarioBenthic1Pond
        High21dayBenthic.Text = eecbenthic21Pond
        Scenario21dayBenthic.Text = scenarioBenthic1Pond



    End Sub

    Private Sub CustomRB_CheckedChanged(sender As Object, e As EventArgs) Handles CustomRB.CheckedChanged
        High1day.Text = eec1Custom
        Scenario1day.Text = scenario1Custom
        High4day.Text = eec4Custom
        Scenario4day.Text = scenario4Custom
        High21day.Text = eec21Custom
        Scenario21day.Text = scenario21Custom
        High60day.Text = eec60Custom
        Scenario60day.Text = scenario60Custom

        High365day.Text = eec365Custom
        Scenario365day.Text = scenario365Custom
        HighOverall.Text = eecFullCustom
        ScenarioOverall.Text = scenarioFullCustom
        High1dayBenthic.Text = eecBenthic1Custom
        Scenario1dayBenthic.Text = scenarioBenthic1Custom
        High21dayBenthic.Text = eecbenthic21Custom
        Scenario21dayBenthic.Text = scenarioBenthic1Custom
    End Sub

    Private Sub ExternalBatchFileRun_CheckedChanged(sender As Object, e As EventArgs) Handles ExternalBatchFileRun.CheckedChanged

        If ExternalBatchFileRun.Checked Then

            ChooseBatchScenarioDirectory.Text = "Weather Directory"
            ESA_Run.Checked = False

            isMinimumBatchOutput.Checked = True
            isMinimumBatchOutput.Enabled = True

        Else
            ChooseBatchScenarioDirectory.Text = "Scenario Directory"
            isMinimumBatchOutput.Checked = False
            isMinimumBatchOutput.Enabled = False
        End If





    End Sub

    Private Sub ESA_Run_CheckedChanged(sender As Object, e As EventArgs) Handles ESA_Run.CheckedChanged

        If ESA_Run.Checked Then


            ExternalBatchFileRun.Checked = False



        End If

    End Sub

    Private Sub workingDirectoryLabel_DoubleClick(sender As Object, e As EventArgs) Handles workingDirectoryLabel.DoubleClick
        'Go to directory if it exists

        If System.IO.Directory.Exists(workingDirectoryLabel.Text) Then
            Process.Start("explorer.exe", workingDirectoryLabel.Text)
        Else
            Return
        End If
    End Sub

    Private Sub workingDirectoryLabel_MouseEnter(sender As Object, e As EventArgs) Handles workingDirectoryLabel.MouseEnter
        If System.IO.Directory.Exists(workingDirectoryLabel.Text) Then
            workingDirectoryLabel.ForeColor = Color.Blue
        Else
            Return
        End If
    End Sub

    Private Sub workingDirectoryLabel_MouseLeave(sender As Object, e As EventArgs) Handles workingDirectoryLabel.MouseLeave


        If System.IO.Directory.Exists(workingDirectoryLabel.Text) Then
            workingDirectoryLabel.ForeColor = Color.Black
        Else
            Return
        End If




    End Sub

    Private Sub GetWeatherDirectory_Click(sender As Object, e As EventArgs) Handles GetWeatherDirectory.Click
        Dim result As System.Windows.Forms.DialogResult

        result = WeatherFolderBrowserDialog1.ShowDialog(Me)

        'Cancel button will cuase return without further execution
        If result = Windows.Forms.DialogResult.Cancel Then
            Return
        End If

        WeatherDirectoryBox.Text = WeatherFolderBrowserDialog1.SelectedPath & "\"

    End Sub


End Class
