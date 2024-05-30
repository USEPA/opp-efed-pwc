Public Class SoilProperty

    Private Sub New()
        'Empty constructor to prevent instantiation of this purely static class
    End Sub


    Public Const maxSoilLayers As Integer = 8
    Public Shared ReadOnly sand As TextBox() = {Form1.sand1, Form1.sand2, Form1.sand3, Form1.sand4, Form1.sand5, Form1.sand6, Form1.sand7, Form1.sand8}
    Public Shared ReadOnly clay As TextBox() = {Form1.clay1, Form1.clay2, Form1.clay3, Form1.clay4, Form1.clay5, Form1.clay6, Form1.clay7, Form1.clay8}

    Public Shared ReadOnly thick As TextBox() = {Form1.thk1, Form1.thk2, Form1.thk3, Form1.thk4, Form1.thk5, Form1.thk6, Form1.thk7, Form1.thk8}
    Public Shared ReadOnly bulkden As TextBox() = {Form1.bd1, Form1.bd2, Form1.bd3, Form1.bd4, Form1.bd5, Form1.bd6, Form1.bd7, Form1.bd8}
    Public Shared ReadOnly oc As TextBox() = {Form1.oc1, Form1.oc2, Form1.oc3, Form1.oc4, Form1.oc5, Form1.oc6, Form1.oc7, Form1.oc8}
    Public Shared ReadOnly maxcap As TextBox() = {Form1.maxCap1, Form1.maxCap2, Form1.maxCap3, Form1.maxCap4, Form1.maxCap5, Form1.maxCap6, Form1.maxCap7, Form1.maxCap8}
    Public Shared ReadOnly mincap As TextBox() = {Form1.minCap1, Form1.minCap2, Form1.minCap3, Form1.minCap4, Form1.minCap5, Form1.minCap6, Form1.minCap7, Form1.minCap8}

    Public Shared ReadOnly compartment As TextBox() = {Form1.compart1, Form1.compart2, Form1.compart3, Form1.compart4, Form1.compart5, Form1.compart6, Form1.compart7, Form1.compart8}


    Public Shared Function PoreVolume(ByVal NumberOfHorizons As Integer) As Single
        'Number of horizons is the total horizon including the aquiferhorizon
        'assumption is that the last horizon IS the aquifer
        PoreVolume = 0.0
        'Pore volumes is always determined as if the last horizon is the aquifer and 
        'pore volumes is the volume before the aquifer (thus "NumberOfHorizons - 2")
        For i As Integer = 0 To (NumberOfHorizons - 2)
            PoreVolume = PoreVolume + thick(i).Text * maxcap(i).Text
        Next


    End Function


    Public Shared Function Retardation(ByVal kd As Single(,), ByVal NumberOfHorizons As Integer, ByVal ChemID As Integer) As Single
        Dim depth As Single
        depth = 0.0

        'Upper loop limit is 1 horizon less than what is in the profile
        'because last horizon is the aquifer and retardation is calculated up
        'to the aquifer surface
        For i As Integer = 0 To (NumberOfHorizons - 2)
            depth = depth + thick(i).Text
        Next

        Retardation = 0.0
        For i As Integer = 0 To (NumberOfHorizons - 2)
            Retardation = Retardation + thick(i).Text / depth * (maxcap(i).Text + bulkden(i).Text * kd(ChemID, i)) / maxcap(i).Text
        Next


    End Function


    Public Shared Function FindDepth(ByVal TargetNode As Integer, ByVal NumberOfHorizons As Integer) As Single
        'Find depth, given a target node.

        Dim deltaX As Single
        Dim testdepth As Single
        Dim node As Integer


        node = 0
        testdepth = 0.0

        For i As Integer = 0 To NumberOfHorizons - 1
            deltaX = thick(i).Text / compartment(i).Text

            For j As Integer = 1 To compartment(i).Text
                node = node + 1
                testdepth = testdepth + deltaX

                If node = TargetNode Then
                    'Test if the overshoot was too much
                    Return testdepth
                End If
            Next
        Next



        FindDepth = testdepth

    End Function






    Public Shared Function FindApproximateDepth(ByVal depth As Single, ByVal NumberOfHorizons As Integer) As Single
        'given a depth, returns the approximated depth that will be used by the program.

        Dim deltaX As Single
        Dim node As Integer
        Dim testdepth As Single

        node = 0
        testDepth = 0.0

        For i As Integer = 0 To NumberOfHorizons - 1
            deltaX = thick(i).Text / compartment(i).Text

            For j As Integer = 1 To compartment(i).Text
                node = node + 1
                testDepth = testDepth + deltaX

                If testDepth >= depth Then
                    'Test if the overshoot was too much
                    If (testDepth - depth) < (depth - (testDepth - deltaX)) Then
                        Return testDepth
                    Else
                        testDepth = testDepth - deltaX
                        node = node - 1
                        Return testDepth
                    End If
                End If
            Next
        Next

        FindApproximateDepth = testdepth

    End Function

    Public Shared Function FindNode(ByVal depth As Single, ByVal NumberOfHorizons As Integer) As Integer
        ' Overload without returning approximate depth

        'given a depth, returns the node that is closest to that depth as well as the approximated depth (testdepth) that will be used by the program.

        Dim deltaX As Single
        Dim testdepth As Single
        Dim node As Integer

        node = 0
        testdepth = 0.0

        'Smallest node is 1
        If depth <= 0 Then
            node = 1
            Return node
        End If

        For i As Integer = 0 To NumberOfHorizons - 1
            deltaX = thick(i).Text / compartment(i).Text

            For j As Integer = 1 To compartment(i).Text
                node = node + 1
                testdepth = testdepth + deltaX

                If testdepth >= depth Then
                    'Test if the overshoot was too much
                    If (testdepth - depth) < (depth - (testdepth - deltaX)) Then
                        Return node
                    Else
                        testdepth = testdepth - deltaX
                        node = node - 1
                        Return node
                    End If
                End If
            Next
        Next


        FindNode = node

    End Function



    Private Shared x(3, maxSoilLayers - 1) As Single


    Public Shared Property KdLayers As Single(,)
        Get
            Return x
        End Get

        Set(value As Single(,))
            x = value
        End Set

    End Property

    'Private Shared y(maxSoilLayers - 1) As Single
    'Public Shared Property Kd2Layers() As Single()
    '    Get
    '        Return y
    '    End Get

    '    Set(value As Single())
    '        y = value
    '    End Set

    'End Property

    'Private Shared z(maxSoilLayers - 1) As Single
    'Public Shared Property Kd3Layers() As Single()
    '    Get
    '        Return z
    '    End Get

    '    Set(value As Single())
    '        z = value
    '    End Set

    'End Property


    Public Shared Sub AddCompartmentForCSTR(ByVal cstr_size As Single, ByRef NumberOfHorizons As Integer)
        Dim N As Integer

        ' Add top layer
        NumberOfHorizons = NumberOfHorizons + 1
        N = NumberOfHorizons

        For i As Integer = N - 1 To 1 Step -1
            thick(i).Text = thick(i - 1).Text
            bulkden(i).Text = bulkden(i - 1).Text
            oc(i).Text = oc(i - 1).Text
            maxcap(i).Text = maxcap(i - 1).Text
            mincap(i).Text = mincap(i - 1).Text
            sand(i).Text = sand(i - 1).Text
            clay(i).Text = clay(i - 1).Text
            compartment(i).Text = compartment(i - 1).Text
        Next

        'properties of cstr are same as top layer except has one compartment with the specified size size
        bulkden(0).Text = bulkden(1).Text
        oc(0).Text = oc(1).Text
        maxcap(0).Text = maxcap(1).Text
        mincap(0).Text = mincap(1).Text
        sand(0).Text = sand(1).Text
        clay(0).Text = clay(1).Text

        compartment(0).Text = "1"
        thick(0).Text = cstr_size

    End Sub










End Class
