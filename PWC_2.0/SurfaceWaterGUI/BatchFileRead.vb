Partial Public Class Form1

    Private Sub ReadEsaInputs(ByVal MyReader As Microsoft.VisualBasic.FileIO.TextFieldParser, ByRef WaterBodyTYpe As String, ByRef errormessage As String)

        Dim currentRow As String()

        errormessage = ""

        Try
            currentRow = MyReader.ReadFields


            chemID.Text = currentRow(1)
            KocBox1.Text = currentRow(2)
            IsKocRadioButton.Checked = currentRow(3)


            waterMetabBox1.Text = currentRow(4)
            waterTempBox1.Text = currentRow(5)
            benthicMetabBox1.Text = currentRow(6)
            benthicTempBox1.Text = currentRow(7)
            photoBox1.Text = currentRow(8)
            rlatBox1.Text = currentRow(9)
            hydroBox1.Text = currentRow(10)
            soilDegradation1.Text = currentRow(11)
            soilTempBox1.Text = currentRow(12)
            foliarDeg1.Text = currentRow(13)
            mwtBox1.Text = currentRow(14)
            vpBox1.Text = currentRow(15)
            solBox1.Text = currentRow(16)


            henry1.Text = currentRow(17)
            airDiffusion1.Text = currentRow(18)
            enthalpyGas1.Text = currentRow(19)



            If Not System.IO.File.Exists(EsaBatchscenarioDirectory.Text & currentRow(21)) Then
                errormessage = "scenario does not exist"
                Exit Sub
            End If
            LoadScenario(EsaBatchscenarioDirectory.Text & currentRow(21))


            'If there is a readable file in this column then Overide the scenario weather file
            If System.IO.File.Exists(currentRow(22)) Then
                weatherBox.Text = currentRow(22)
            End If

            'Section for other Batch Overrides



            For i As Integer = 23 To 31 Step 2



                Select Case currentRow(i)
                    Case 1  'Code 1 is root depth 
                        rootDepth1.Text = currentRow(i + 1)
                    Case 2
                        canopyCover1.Text = currentRow(i + 1)
                    Case 3
                        cn1.Text = currentRow(i + 1)
                    Case 4  'the Value is a multiplier'
                        For j As Integer = 0 To numHoriz.Text - 1
                            SoilProperty.bulkden(j).Text = currentRow(i + 1) * SoilProperty.bulkden(j).Text
                        Next
                    Case 5
                        'the Value is a multiplier'
                        For j As Integer = 0 To numHoriz.Text - 1
                            SoilProperty.oc(j).Text = currentRow(i + 1) * SoilProperty.oc(j).Text
                        Next
                    Case 6
                        evapDepth.Text = currentRow(i + 1)

                    Case 7
                        pfac.Text = currentRow(i + 1)

                    Case Else
                        'do nothing
                End Select



            Next


            'overides are 23 to 32

            ioFamilyNameBox.Text = currentRow(1) & "_" & currentRow(33)   'chem_id + bin


            Select Case Convert.ToInt32(currentRow(33))

                Case 2 To 4
                    ConstVolFlow.Checked = True

                    ConstVolNoFlow.Checked = False
                    EPApond.Checked = False
                    EPAreservoir.Checked = False
                    VaryVolFlow.Checked = False
                    UserSpecifiedFlowAvg.Checked = False
                    GroundWater.Checked = False
                    PRZMonly.Checked = False

                    WaterBodyTYpe = StandardParameters.ConstVolFlo

                Case 5 To 7
                    ConstVolNoFlow.Checked = True

                    ConstVolFlow.Checked = False
                    EPApond.Checked = False
                    EPAreservoir.Checked = False
                    VaryVolFlow.Checked = False
                    UserSpecifiedFlowAvg.Checked = False
                    GroundWater.Checked = False
                    PRZMonly.Checked = False

                    WaterBodyTYpe = StandardParameters.ConstVolNoFlo

                Case 10
                    ConstVolNoFlow.Checked = False

                    ConstVolFlow.Checked = False
                    EPApond.Checked = False
                    EPAreservoir.Checked = False
                    VaryVolFlow.Checked = True
                    UserSpecifiedFlowAvg.Checked = False
                    GroundWater.Checked = False
                    PRZMonly.Checked = False

                    WaterBodyTYpe = StandardParameters.CustomVVWM

                Case 100
                    GroundWater.Checked = True

                    ConstVolNoFlow.Checked = False
                    ConstVolFlow.Checked = False
                    EPApond.Checked = False
                    EPAreservoir.Checked = False
                    VaryVolFlow.Checked = False
                    UserSpecifiedFlowAvg.Checked = False
                    PRZMonly.Checked = False

                    WaterBodyTYpe = StandardParameters.EPAGroundWater
                Case Else
                    WaterBodyTYpe = StandardParameters.ConstVolFlo
            End Select

            CustomFlowAvgDays.Text = currentRow(34)

            'TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

            fieldAreaBox.Text = currentRow(35)
            waterAreaBox.Text = currentRow(36)
            initialDepthBox.Text = currentRow(37)
            maxDepthBox.Text = currentRow(38)
            hl.Text = currentRow(39)

            baseFlow.Text = currentRow(41)

            DaySheds.NumDaySheds = currentRow(42)

            '**************Section For Daysheds ***************************
            For i As Integer = 0 To (DaySheds.NumDaySheds - 1)
                DaySheds.AreaFraction(i) = currentRow(43 + i)
            Next
            '**************************************************************

            appNumber.Text = currentRow(74)
            AppsAbsolute.Checked = currentRow(75)
            AppsRelative.Checked = Not AppsAbsolute.Checked

            Try ' in case not populated
                Select Case currentRow(76)
                    Case 1
                        RelToEmerge.Checked = True
                    Case 2
                        RelToMat.Checked = True
                    Case Else
                        RelToHarv.Checked = True
                End Select

            Catch ex As Exception

            End Try



            Dim appmethod As Integer
            For i As Integer = 0 To appNumber.Text - 1

                If AppsAbsolute.Checked Then
                    ApplicationInfo.day(i).Text = currentRow(77 + i * 8)
                    ApplicationInfo.mon(i).Text = currentRow(78 + i * 8)
                Else
                    ApplicationInfo.RelApp(i).Text = currentRow(77 + i * 8)
                End If


                ApplicationInfo.rate(i).Text = currentRow(79 + i * 8)

                appmethod = currentRow(80 + i * 8)
                If appmethod = 1 Then
                    ApplicationInfo.methodbutton(i * ApplicationInfo.NumberofApplicationTypes + 0).Checked = True
                ElseIf appmethod = 2 Then
                    ApplicationInfo.methodbutton(i * ApplicationInfo.NumberofApplicationTypes + 1).Checked = True
                ElseIf appmethod = 3 Then
                    ApplicationInfo.methodbutton(i * ApplicationInfo.NumberofApplicationTypes + 2).Checked = True
                ElseIf appmethod = 4 Then
                    ApplicationInfo.methodbutton(i * ApplicationInfo.NumberofApplicationTypes + 3).Checked = True
                ElseIf appmethod = 5 Then
                    ApplicationInfo.methodbutton(i * ApplicationInfo.NumberofApplicationTypes + 4).Checked = True
                ElseIf appmethod = 6 Then
                    ApplicationInfo.methodbutton(i * ApplicationInfo.NumberofApplicationTypes + 5).Checked = True
                ElseIf appmethod = 7 Then
                    ApplicationInfo.methodbutton(i * ApplicationInfo.NumberofApplicationTypes + 6).Checked = True
                Else
                    ApplicationInfo.methodbutton(i * ApplicationInfo.NumberofApplicationTypes + 0).Checked = True
                End If
                ApplicationInfo.DepthIncorp(i).Text = currentRow(81 + i * 8)
                ApplicationInfo.tBand(i).Text = currentRow(82 + i * 8)
                ApplicationInfo.Eff(i).Text = currentRow(83 + i * 8)
                ApplicationInfo.spray(i).Text = currentRow(84 + i * 8)
            Next


            '*********Cropped Area is now overridden for external files so that the dayshed concept can be used instead*******
            If DaySheds.NumDaySheds > 1 Then
                CustomCroppedAreaBox.Text = 1.0
            Else
                'If only one dayshed then calcs will be bypassed and fraction of crop area is taken from 43
                CustomCroppedAreaBox.Text = currentRow(43)
            End If

            '*****************************************************************************************************************

        Catch ex As Exception
            errormessage = ex.Message
        End Try



    End Sub



End Class
