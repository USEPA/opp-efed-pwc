Public Class HelpInfo

    Private Sub HelpInfo_Load(sender As System.Object, e As System.EventArgs) Handles MyBase.Load
        Dim AppPath As String
        AppPath = My.Application.Info.DirectoryPath()

        Try
            '  HelpTextBox.LoadFile(AppPath & "\SWManual.rtf")
            HelpTextBox.LoadFile(AppPath & "\PWC_Help.rtf")

        Catch ex As Exception

            MsgBox(ex.Message)
            '  HelpTextBox.Text = "help unavailable"
        End Try
    End Sub
End Class