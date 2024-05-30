Module Module1



    Function TestIntegers(ByVal name As TextBox) As String
        Dim TestNumber As Integer
        Dim TestReal As Single

        TestIntegers = ""


        name.BackColor = Color.White
        'test to see if its  a number

        Try
            TestNumber = name.Text
        Catch ex As Exception
            ' MsgBox("Check the value for " & name.Tag)
            name.BackColor = Color.Orange
            Return "Cannot convert " & name.Text & "to integer, Check the value for " & name.Tag
        End Try

        'testIntegers Today see if its an integer

        'This will catch real numbers that can be converted to integers but are fractional. Not absolutely
        'necessary, but may catch some unintended inputs.
        TestReal = name.Text
        If TestReal - TestNumber > 0.01 Then
            name.BackColor = Color.Orange
            Return "You likely entered a non integer number, Check the value for" & name.Tag
        End If

        If name.Text.Contains(",") Then
            ' MsgBox("No commas allowed for " & name.Tag)
            name.BackColor = Color.Orange
            Return "No commas allowed for " & name.Tag
        End If




    End Function

    Function TestRealNumbers(ByVal name As TextBox) As String
        'Tests if real
        'Return: Empty string indicates the value is good number
        'Return: Non empty string delivers a message of the error

        Dim TestNumber As Double
        TestRealNumbers = ""

        name.BackColor = Color.White
        Try
            TestNumber = name.Text
        Catch ex As Exception
            '   MsgBox("Check the value for " & name.Tag)
            name.BackColor = Color.Orange
            Return "Check the value for " & name.Tag
        End Try

        If name.Text.Contains(",") Then
            '  MsgBox("No commas allowed for " & name.Tag)
            name.BackColor = Color.Orange
            Return "No commas allowed for " & name.Tag
        End If


    End Function

    Function TestRealNumbers(ByVal name As TextBox, ByVal except As String) As String
        'this overload will allow exceptions to the real number requirement, For example the null string would
        'indicate a zero rate of degradation if box is left blank
        Dim TestNumber As Double

        TestRealNumbers = ""
        name.BackColor = Color.White
        If name.Text = except Then
            Return ""
        Else
            Try
                TestNumber = name.Text
            Catch ex As Exception

                name.BackColor = Color.Orange
                Return "Check the value for " & name.Tag
            End Try
        End If
    End Function



    Function calendarCheck(ByVal dayBox As TextBox, ByVal monthBox As TextBox) As String

        Dim monthtest As Integer
        Dim daytest As Integer

        calendarCheck = ""

        dayBox.BackColor = Color.White
        monthBox.BackColor = Color.White

        Try
            monthtest = Convert.ToInt16(monthBox.Text)
        Catch
            '  MsgBox("  Check " & monthBox.Tag)
            monthBox.BackColor = Color.Orange
            Return "  Check " & monthBox.Tag
        End Try


        Try
            daytest = Convert.ToInt16(dayBox.Text)
        Catch
            '   MsgBox(" Check " & dayBox.Tag)
            dayBox.BackColor = Color.Orange
            Return " Check " & dayBox.Tag
        End Try

        If monthtest > 12 Or monthtest < 1 Then
            '  MsgBox("The following month is not posssible: " & monthBox.Tag)
            monthBox.BackColor = Color.Orange
            Return "The following month is not posssible: " & monthBox.Tag
        End If

        Select Case monthtest
            Case 1, 3, 5, 7, 8, 10, 12
                If daytest > 31 Or daytest < 1 Then
                    '   MsgBox("Bad day for " & dayBox.Tag)
                    dayBox.BackColor = Color.Orange
                    Return "Bad day for " & dayBox.Tag
                End If
            Case 2
                If daytest > 28 Or daytest < 1 Then
                    '    MsgBox("Bad day for " & dayBox.Tag)
                    dayBox.BackColor = Color.Orange
                    Return "Bad day for " & dayBox.Tag
                End If
            Case 4, 6, 9, 11
                If daytest > 30 Or daytest < 1 Then
                    '   MsgBox("Bad day for " & dayBox.Tag)
                    dayBox.BackColor = Color.Orange
                    Return "Bad day for " & dayBox.Tag
                End If
            Case Else
                '  MsgBox("Month does not exist on Earth calendar for " & monthBox.Tag)

                monthBox.BackColor = Color.Orange
                Return "Month does not exist on Earth calendar for " & monthBox.Tag
        End Select


    End Function



    Function TestPositiveNumbers(ByVal name As TextBox) As String

        Dim TestNumber As Double
        Dim msg As String

        TestPositiveNumbers = ""
        name.BackColor = Color.White
        Try
            TestNumber = name.Text
        Catch ex As Exception
            msg = "Check the value for " & name.Tag

            name.BackColor = Color.Orange
            Return msg
        End Try

        If TestNumber < 0.0 Then
            msg = "Value must be a positive number: " & name.Tag

            name.BackColor = Color.Orange
            Return msg
        Else
            Return ""
        End If

    End Function


End Module
