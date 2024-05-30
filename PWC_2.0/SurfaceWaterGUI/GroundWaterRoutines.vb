Imports System.Windows.Forms.DataVisualization.Charting

Public Class GroundWaterRoutines


    Public Shared Sub SummarizeGW_Results(ByVal summaryFileNames() As String, ByVal nchem As Integer, ByVal GWpeakConc() As Single, ByVal GWsimAvgConc() As Single,
                                         ByVal GWthroughputs() As Single, ByVal GWpostBTavg() As Single, ByVal GWBreakThroughdays() As Single)

        Dim msg As String

        For i As Integer = 1 To nchem
            msg = String.Format("{0}{1}{2}{1}{3}{1}{4}{1}{5}{1}{6}", vbNewLine, vbTab, GWpeakConc(i), GWsimAvgConc(i), GWthroughputs(i), GWpostBTavg(i), GWBreakThroughdays(i))
            My.Computer.FileSystem.WriteAllText(summaryFileNames(i), msg, True, System.Text.Encoding.ASCII)
        Next

    End Sub

    Public Shared Sub CaptureGWOutput(ByVal nchem As Integer, ByVal filename As String, ByVal NumberOfHorizons As Integer,
                                ByRef maxConc As Single(), ByRef SimAvgConc() As Single, ByRef Throughputs() As Single,
                                ByRef PostBTAvg() As Single, ByRef BreakthroughDays() As Single)

        Dim infiltration As Single
        Dim RetardationFactor(3) As Single
        RetardationFactor(1) = 0.0
        RetardationFactor(2) = 0.0
        RetardationFactor(3) = 0.0


        Dim ThroughputConversionFactor(3) As Single
        'this calculate the throughputs as the chemical enters the top surface of the aquifer

        For i As Integer = 1 To nchem
            RetardationFactor(i) = SoilProperty.Retardation(SoilProperty.KdLayers, NumberOfHorizons, i)
            ThroughputConversionFactor(i) = 1.0 / (SoilProperty.PoreVolume(NumberOfHorizons) * SoilProperty.Retardation(SoilProperty.KdLayers, NumberOfHorizons, i))

        Next

        Form1.RetardationFactor1.Text = RetardationFactor(1)
        Form1.RetardationFactor2.Text = RetardationFactor(2)
        Form1.RetardationFactor3.Text = RetardationFactor(3)


        Using reader As New Microsoft.VisualBasic.FileIO.TextFieldParser(filename)
            reader.TextFieldType = Microsoft.VisualBasic.FileIO.FieldType.FixedWidth

            Select Case nchem    'ZTS File format   FORMAT (I2,2I3,6X,12(2X,G12.4)) 
                Case 1                  ' y  m  d  x  r   e   r1  e1  c1  I
                    reader.SetFieldWidths(2, 3, 3, 6, 14, 14, 14, 14, 14, 14, -1)
                Case 2                  ' y  m  d  x  r   e   r1  e1  r2  e2  c1  c2  I
                    reader.SetFieldWidths(2, 3, 3, 6, 14, 14, 14, 14, 14, 14, 14, 14, 14, -1)
                Case 3
                    reader.SetFieldWidths(2, 3, 3, 6, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, -1)
                Case Else
            End Select

            Dim n As Integer
            Dim currentrow As String()
            Dim conc(3) As Single
            Dim total(3) As Single
            Dim postCount(3) As Integer
            Dim concPostBreakthrough(3) As Single





            maxConc(1) = 0.0
            maxConc(2) = 0.0
            maxConc(3) = 0.0

            total(1) = 0.0
            total(2) = 0.0
            total(3) = 0.0

            postCount(1) = 0.0
            postCount(2) = 0.0
            postCount(3) = 0.0

            concPostBreakthrough(1) = 0.0
            concPostBreakthrough(2) = 0.0
            concPostBreakthrough(3) = 0.0


            'skip the 3 Header Lines
            reader.ReadLine()
            reader.ReadLine()
            reader.ReadLine()



            While Not reader.EndOfData
                Try
                    n = reader.LineNumber
                    currentrow = reader.ReadFields()
                    infiltration = currentrow(6 + nchem * 3)

                    For i As Integer = 1 To nchem

                        conc(i) = currentrow(5 + 2 * nchem + i)
                        If conc(i) > maxConc(i) Then
                            maxConc(i) = conc(i)
                        End If

                        total(i) = total(i) + conc(i)

                        'Calculate the average concentration after breakthrough
                        If infiltration * ThroughputConversionFactor(i) >= 1.0 Then
                            postCount(i) = postCount(i) + 1
                            concPostBreakthrough(i) = concPostBreakthrough(i) + conc(i)
                        End If
                    Next

                Catch ex As Exception
                    MsgBox(ex.Message)
                    Return
                End Try
            End While


            For i As Integer = 1 To nchem
                SimAvgConc(i) = total(i) / (n - 3) 'less the 3 header lines

                If postCount(i) > 0 Then
                    PostBTAvg(i) = concPostBreakthrough(i) / postCount(i)

                    BreakthroughDays(i) = ((n - 3) / (infiltration * ThroughputConversionFactor(i)))
                Else
                    PostBTAvg(i) = -999999
                    BreakthroughDays(i) = -999999
                End If

                Throughputs(i) = ThroughputConversionFactor(i) * infiltration
            Next

        End Using



    End Sub



    Public Shared Sub MakeGwGraph(ByVal nchem As Integer, ByVal filename As String, ByRef chart1 As Chart)

        Using reader As New Microsoft.VisualBasic.FileIO.TextFieldParser(filename)
            reader.TextFieldType = Microsoft.VisualBasic.FileIO.FieldType.FixedWidth

            Select Case nchem    'ZTS File format   FORMAT (I2,2I3,6X,12(2X,G12.4)) 
                Case 1                  ' y  m  d  x  r   e   r1  e1  c1  I
                    reader.SetFieldWidths(4, 3, 3, 4, 14, 14, 14, 14, 14, 14, -1)
                Case 2                  ' y  m  d  x   r   e  r1  e1  r2  e2  c1  c2  I
                    reader.SetFieldWidths(4, 3, 3, 4, 14, 14, 14, 14, 14, 14, 14, 14, 14, -1)
                Case 3
                    reader.SetFieldWidths(4, 3, 3, 4, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, -1)
                Case Else
            End Select

            Dim n As Integer
            Dim currentrow As String()
            Dim xday As Integer
            Dim xmonth As Integer
            Dim xyear As Integer
            Dim conc As Single

            Dim xdate As Date


            'skip the 3 Header Lines
            reader.ReadLine()
            reader.ReadLine()
            reader.ReadLine()

            'clear all point from previous run
            chart1.Series(0).Points.Clear()
            chart1.Series(1).Points.Clear()
            chart1.Series(2).Points.Clear()

            chart1.BackColor = Color.White
            '    chart1.ChartAreas(0).AxisX.ScaleView.Zoomable = True


            Select Case nchem
                Case 1
                    chart1.Series(0).IsVisibleInLegend = False
                    chart1.Series(1).IsVisibleInLegend = False
                    chart1.Series(2).IsVisibleInLegend = False

                Case 2
                    chart1.Series(0).IsVisibleInLegend = True
                    chart1.Series(1).IsVisibleInLegend = True
                    chart1.Series(2).IsVisibleInLegend = False
                Case 3
                    chart1.Series(0).IsVisibleInLegend = True
                    chart1.Series(1).IsVisibleInLegend = True
                    chart1.Series(2).IsVisibleInLegend = True

            End Select



            While (Not reader.EndOfData)
                Try
                    n = reader.LineNumber
                    currentrow = reader.ReadFields()

                    xday = currentrow(2)
                    xmonth = currentrow(1)

                    xyear = currentrow(0)


                    xdate = New Date(xyear, xmonth, xday)


                    conc = currentrow(5 + 2 * nchem + 1)
                    chart1.Series(0).Points.AddXY(xdate, conc)

                    Select Case nchem

                        Case 2
                            conc = currentrow(5 + 2 * nchem + 2)
                            chart1.Series("Daughter").Points.AddXY(xdate, conc)

                        Case 3
                            conc = currentrow(5 + 2 * nchem + 2)
                            chart1.Series("Daughter").Points.AddXY(xdate, conc)

                            conc = currentrow(5 + 2 * nchem + 3)
                            chart1.Series("Granddaughter").Points.AddXY(xdate, conc)
                    End Select


                Catch ex As Exception
                    MsgBox(ex.Message)
                    Return
                End Try
            End While



            chart1.ChartAreas(0).AxisX.IntervalType = DateTimeIntervalType.Years
            '   chart1.ChartAreas(0).AxisX.Interval = 2
            chart1.ChartAreas(0).AxisX.LabelStyle.Format = "yyyy"
            '        chart1.ChartAreas(0).AxisX.MajorGrid.Interval = 2
            'chart1.ChartAreas(0).AxisX.MajorTickMark.Interval = 1


            chart1.Series(0).ChartType = SeriesChartType.FastLine

            If nchem = 2 Then
                chart1.Series("Daughter").ChartType = SeriesChartType.FastLine
            End If

            If nchem = 3 Then
                chart1.Series(2).ChartType = SeriesChartType.FastLine
            End If

        End Using



    End Sub




End Class
