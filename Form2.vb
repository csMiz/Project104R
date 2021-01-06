Public Class Form2

    Public Flag_MouseDown As Integer = 0
    Public Src_MousePosition As Point

    Private Sub Form2_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Me.FormBorderStyle = FormBorderStyle.None
        Me.TransparencyKey = Color.FromArgb(255, 255, 0, 255)
        Me.AllowTransparency = True
    End Sub

    Private Sub Form2_MouseDown(sender As Object, e As MouseEventArgs) Handles Me.MouseDown
        If e.Button = MouseButtons.Left Then
            Flag_MouseDown = 1
            Src_MousePosition = e.Location
        End If
    End Sub

    Private Sub Form2_MouseUp(sender As Object, e As MouseEventArgs) Handles Me.MouseUp
        Flag_MouseDown = 0
    End Sub

    Private Sub Form2_MouseMove(sender As Object, e As MouseEventArgs) Handles Me.MouseMove
        If Flag_MouseDown = 1 Then
            Dim offset As Point = e.Location - Src_MousePosition
            Me.Location += offset
        End If
    End Sub

    Private Sub Form2_DoubleClick(sender As Object, e As EventArgs) Handles Me.DoubleClick
        Me.FormBorderStyle = FormBorderStyle.Sizable
    End Sub

    Private Sub Form2_KeyDown(sender As Object, e As KeyEventArgs) Handles Me.KeyDown
        Console.WriteLine("captured:" & e.KeyCode.ToString)
    End Sub

End Class