Public Class StandardParameters
    Private Sub New()
        'Empty constructor to prevent instantiation of this purely static class
    End Sub

    Public Const VersionNumber As Single = 2.0  'PWC Version Number used on input file and Form1.text
    Public Const ProgramName As String = "Pesticide Water Calculator (PWC), Version " 'Note word "Version" must be "Version" exactly caps and all

    Public Const MaxNumWeatherData As Integer = 20000 ' used to allocate arrays in esa batch read dayshed routine

    Public Const DFAC As Single = 1.19
    Public Const SS As Single = 30.0
    Public Const Chlorophyll As Single = 0.005
    Public Const foc1 As Single = 0.04
    Public Const DOC1 As Single = 5.0
    Public Const biomass1 As Single = 0.4

    Public Const benthicDepth As Single = 0.05
    Public Const porosity As Single = 0.5
    Public Const bulkdensity As Single = 1.35
    Public Const foc2 As Single = 0.04
    Public Const DOC2 As Single = 5.0
    Public Const biomass2 As Single = 0.006

    Public Const pondField As Single = 100000
    Public Const pondArea As Single = 10000
    Public Const pondDepth As Single = 2.0
    Public Const pondDepthMax As Single = 2.0
    Public Const pondHL As Single = 356.8

    Public Const reservoirField As Single = 1728000
    Public Const reservoirArea As Single = 52600
    Public Const reservoirDepth As Single = 2.74
    Public Const reservoirDepthMax As Single = 2.74
    Public Const reservoirHL As Single = 600

    Public Const massTransCoeff As Single = 0.00000001
    Public Const prben As Single = 0.5

    Public Const EpaReservoir As String = "EPA Reservoir"
    Public Const EpaPond As String = "EPA Pond"
    Public Const CustomVVWM As String = "VVWM"
    Public Const ConstVolNoFlo As String = "Constant Volume No Flowthrough"
    Public Const ConstVolFlo As String = "Constant Volume With Flowthrough"
    Public Const EPAGroundWater As String = "EPA Ground Water"
    Public Const NoWaterBody As String = "PRZM Only Run"
    Public Const ESA As String = "ESA"

    Public Const VariableVolumeFlo As String = "Variable Volume With Flowthrough"

    Public Const PondCroppedAreaFraction As Single = 1.0

    Public Shared ReadOnly degradatioReduction() As Single = {1.0, 0.9444, 0.7778, 0.5556, 0.3333, 0.1111, 0, 0}




    Public Shared Function WaterBodyName(ByVal indicator As String)

        'these definitions are needed because the VVWM uses these in its output
        Select Case indicator
            Case StandardParameters.EpaReservoir
                WaterBodyName = "Reservoir"
            Case StandardParameters.EpaPond
                WaterBodyName = "Pond"
            Case StandardParameters.ESA
                WaterBodyName = "ESA"
            Case StandardParameters.EPAGroundWater
                WaterBodyName = "Groundwater"
            Case Else
                WaterBodyName = "Custom"
        End Select
    End Function






End Class
