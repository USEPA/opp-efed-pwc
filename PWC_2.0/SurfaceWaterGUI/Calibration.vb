Public Class Calibration

    Private Shared a As Single
    Public Shared Property DataCumulativeCalibration As Single
        Get
            Return a
        End Get

        Set(value As Single)
            a = value
        End Set
    End Property



    'Private Shared ax As Single
    'Public Shared Property SSQ_individual_Cumulative As Single
    '    Get
    '        Return ax
    '    End Get

    '    Set(value As Single)
    '        ax = value
    '    End Set
    'End Property

    Private Shared ax1 As Single
    Public Shared Property SSQ_Unweighted As Single
        Get
            Return ax1
        End Get

        Set(value As Single)
            ax1 = value
        End Set
    End Property


    Private Shared ax2 As Single
    Public Shared Property SSQ_AppNormalized As Single
        Get
            Return ax2
        End Get

        Set(value As Single)
            ax2 = value
        End Set
    End Property

    Private Shared ax3 As Single
    Public Shared Property SSQ_DataNormalized As Single
        Get
            Return ax3
        End Get

        Set(value As Single)
            ax3 = value
        End Set
    End Property

    Private Shared ax4 As Single
    Public Shared Property SSQ_Erosion As Single
        Get
            Return ax4
        End Get

        Set(value As Single)
            ax4 = value
        End Set
    End Property









    Private Shared b As Single
    Public Shared Property ModelCumulativeCalibration As Single
        Get
            Return b
        End Get

        Set(value As Single)
            b = value
        End Set
    End Property

    Private Shared c As Single
    Public Shared Property SSQ_totalFromSimulationMassNormalzed As Single
        Get
            Return c
        End Get

        Set(value As Single)
            c = value
        End Set
    End Property


    Public Shared Sub SummarizeBigCalibration(ByVal NumberofFactors As Integer, ByVal isRunofforErosion As Boolean)

        Dim ModelMass, DataMass As Single

        Using MyReader As New Microsoft.VisualBasic.FileIO.TextFieldParser(FileNameClass.PrZMCalibrationOutputFile)
            Dim currentrow As String()

            MyReader.TextFieldType = Microsoft.VisualBasic.FileIO.FieldType.Delimited
            MyReader.SetDelimiters("=")
            'MyReader.TextFieldType = Microsoft.VisualBasic.FileIO.FieldType.FixedWidth
            'MyReader.SetFieldWidths(87, 11, -1)

            MyReader.ReadLine()
            MyReader.ReadLine()
            MyReader.ReadLine()
            MyReader.ReadLine()
            MyReader.ReadLine()

            For i = 0 To NumberofFactors - 1
                MyReader.ReadLine()
            Next

            MyReader.ReadLine() '****************

            Dim roDataNoramlizedSSQ, roAppliedMassNormalizedSSQ, roUnweightedSSQ, ErosionSSQ As Single

            'Read in all the differnet kinds of SSQ 
            currentrow = MyReader.ReadFields()
            roDataNoramlizedSSQ = currentrow(1)

            MyReader.ReadLine() 'Number of data points

            currentrow = MyReader.ReadFields()
            roAppliedMassNormalizedSSQ = currentrow(1)

            currentrow = MyReader.ReadFields()
            roUnweightedSSQ = currentrow(1)

            MyReader.ReadLine()
            MyReader.ReadLine()


            MyReader.ReadLine()

            currentrow = MyReader.ReadFields()


            ErosionSSQ = currentrow(1)



            Calibration.SSQ_AppNormalized = Calibration.SSQ_AppNormalized + roAppliedMassNormalizedSSQ
            Calibration.SSQ_DataNormalized = Calibration.SSQ_DataNormalized + roDataNoramlizedSSQ
            Calibration.SSQ_Unweighted = Calibration.SSQ_Unweighted + roUnweightedSSQ
            Calibration.SSQ_Erosion = Calibration.SSQ_Erosion + ErosionSSQ

            For i = 1 To 6
                MyReader.ReadLine()
            Next

            currentrow = MyReader.ReadFields()
            DataMass = currentrow(1)

            MyReader.ReadLine()
            MyReader.ReadLine()
            MyReader.ReadLine()

            currentrow = MyReader.ReadFields()
            ModelMass = currentrow(1)

            'these are not used as far as I can tell ?????????
            DataCumulativeCalibration = DataCumulativeCalibration + DataMass
            ModelCumulativeCalibration = ModelCumulativeCalibration + ModelMass
            '??????????????????????????????????????

            'If normalization = "dataNormalized" Then
            '    SumOfSquares = SumOfSquares + ((DataMass - ModelMass) / DataMass) ^ 2

            'ElseIf normalization = "appliedMassNormalized" Then

            SSQ_totalFromSimulationMassNormalzed = SSQ_totalFromSimulationMassNormalzed + ((DataMass - ModelMass) / ApplicationInfo.TotalMassApplied) ^ 2
            'Else
            '    SumOfSquares = SumOfSquares + (DataMass - ModelMass) ^ 2
            'End If


        End Using

    End Sub
















End Class
