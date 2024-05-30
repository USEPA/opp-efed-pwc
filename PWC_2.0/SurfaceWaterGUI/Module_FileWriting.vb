Module Module_FileWriting

    Public eec1Res, eec4Res, eec21Res, eec60Res, eec90Res, eec365Res, eecFullRes, eecBenthic1Res, eecbenthic21Res As Single
    Public scenario1Res, scenario4Res, scenario21Res, scenario60Res, scenario90Res, scenario365Res, scenarioFullRes, scenarioBenthic1Res, scenariobenthic21Res As String

    Public eec1Pond, eec4Pond, eec21Pond, eec60Pond, eec90Pond, eec365Pond, eecFullPond, eecBenthic1Pond, eecbenthic21Pond As Single
    Public scenario1Pond, scenario4Pond, scenario21Pond, scenario60Pond, scenario90Pond, scenario365Pond, scenarioFullPond, scenarioBenthic1Pond, scenariobenthic21Pond As String


    Public eec1Custom, eec4Custom, eec21Custom, eec60Custom, eec90Custom, eec365Custom, eecFullCustom, eecBenthic1Custom, eecbenthic21Custom As Single
    Public scenario1Custom, scenario4Custom, scenario21Custom, scenario60Custom, scenario90Custom, scenario365Custom, scenarioFullCustom, scenarioBenthic1Custom, scenariobenthic21Custom As String




    Sub WriteBatchOutputFile(ByVal filename As String, ByVal header As String, ByVal arraysize As Integer, ByVal preservedArray As String(), ByVal eec As Single(), ByRef MostOffensiveScenario As String, ByRef HighestConcentration As Single)
        Dim msg As String
        Dim ID(arraysize) As String


        'We want to keep the original run id, but byval doesnt seem to  work like I want it to, so Im creating a new copy here to preserve original order
        For i As Integer = 0 To arraysize
            ID(i) = preservedArray(i)
        Next


        Array.Sort(eec, ID)

        MostOffensiveScenario = ID(arraysize)

        HighestConcentration = eec(arraysize)



        msg = String.Format("{0}{0}{2,-30},{1}{3}", vbNewLine, vbTab, "Scenario Ranked", header)

        My.Computer.FileSystem.WriteAllText(filename, msg, True, System.Text.Encoding.ASCII)


        For i As Integer = arraysize To 0 Step -1
            msg = String.Format("{0}{2,-30},{1}{3}", vbNewLine, vbTab, ID(i), eec(i))
            My.Computer.FileSystem.WriteAllText(filename, msg, True, System.Text.Encoding.ASCII)
        Next



    End Sub







End Module
