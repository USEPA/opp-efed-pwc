Public Class DaySheds
    Private Sub New()
        'Empty constructor to prevent instantiation of this purely static class
    End Sub

    Public Shared AreaFraction(30) As Single
    Public Shared NumDaySheds As Integer

    Public Shared Sub Calculate()
        Dim HeaderLine1, HeaderLine2, HeaderLine3 As String
        Dim totalrunoff As Single
        Dim totalerosion As Single
        Dim total_runoffMass As Single
        Dim total_erosionMass As Single
        Dim currentrow As String()
        Dim runoff(StandardParameters.MaxNumWeatherData) As Single
        Dim erosion(StandardParameters.MaxNumWeatherData) As Single
        Dim pesticide_runoff(StandardParameters.MaxNumWeatherData) As Single
        Dim pesticide_erosion(StandardParameters.MaxNumWeatherData) As Single
        Dim firstpart(StandardParameters.MaxNumWeatherData) As String
        Dim lastpart(StandardParameters.MaxNumWeatherData) As String
        Dim linecount As Integer
        Dim msg As String

        Using MyReader As New Microsoft.VisualBasic.FileIO.TextFieldParser(FileNameClass.ZtsFile)
            MyReader.TextFieldType = Microsoft.VisualBasic.FileIO.FieldType.FixedWidth
            MyReader.SetFieldWidths(17, 14, 14, 14, 14, -1)

            HeaderLine1 = MyReader.ReadLine()
            HeaderLine2 = MyReader.ReadLine()
            HeaderLine3 = MyReader.ReadLine()

            linecount = 0
            While Not MyReader.EndOfData
                linecount = linecount + 1
                currentrow = MyReader.ReadFields()
                firstpart(linecount) = currentrow(0)
                runoff(linecount) = currentrow(1)
                erosion(linecount) = currentrow(2)
                pesticide_runoff(linecount) = currentrow(3)
                pesticide_erosion(linecount) = currentrow(4)
                lastpart(linecount) = currentrow(5)
            End While
        End Using

        'Write Over zts file
        My.Computer.FileSystem.WriteAllText(FileNameClass.ZtsFile, HeaderLine1, False, System.Text.Encoding.ASCII)
        My.Computer.FileSystem.WriteAllText(FileNameClass.ZtsFile, vbNewLine & HeaderLine2, True, System.Text.Encoding.ASCII)
        My.Computer.FileSystem.WriteAllText(FileNameClass.ZtsFile, vbNewLine & HeaderLine3, True, System.Text.Encoding.ASCII)

        For j As Integer = 1 To linecount
            totalrunoff = 0
            totalerosion = 0
            total_runoffMass = 0
            total_erosionMass = 0

            For i As Integer = 0 To Math.Min(DaySheds.NumDaySheds - 1, j)
                totalrunoff = totalrunoff + runoff(j - i) * DaySheds.AreaFraction(i)
                totalerosion = totalerosion + erosion(j - i) * DaySheds.AreaFraction(i)
                total_runoffMass = total_runoffMass + pesticide_runoff(j - i) * DaySheds.AreaFraction(i)
                total_erosionMass = total_erosionMass + pesticide_erosion(j - i) * DaySheds.AreaFraction(i)
            Next

            msg = String.Format("{0,-17}{1,-14}{2,-14}{3,-14}{4,-14}{5}", firstpart(j), totalrunoff, totalerosion, total_runoffMass, total_erosionMass, lastpart(j))
            My.Computer.FileSystem.WriteAllText(FileNameClass.ZtsFile, vbNewLine & msg, True, System.Text.Encoding.ASCII)
        Next

    End Sub


End Class
