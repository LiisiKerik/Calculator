Public Class Form1
    WithEvents BStart As Button
    Dim CB(7) As CheckBox
    Public OperationMax(7) As Integer
    Public OperationMin(7) As Integer
    Sub AddLabel(lft As Integer, tp As Integer, txt As String)
        Dim l As Label = New Label
        l.AutoSize = True
        l.Location = New Point(lft, tp)
        l.Text = txt
        Me.Controls.Add(l)
    End Sub
    Sub BStart_Click() Handles BStart.Click
        Game.NumberOfOperations = 0
        For i As Integer = 0 To 7
            If CB(i).Checked = True Then
                Game.NumberOfOperations += 1
                OperationMin(i) = 1
            Else
                OperationMin(i) = 0
                OperationMax(i) = 0
            End If
        Next
        For i As Integer = 0 To 7
            Game.Range(i) = 840 / Game.NumberOfOperations
        Next
        Game.SetRange()
        Me.Hide()
        Game.Show()
    End Sub
    Sub CBCheck()
        Dim e As Boolean = False
        For i As Integer = 0 To 7
            e = e Or CB(i).Checked
        Next
        BStart.Enabled = e
    End Sub
    Sub Me_Load() Handles Me.Load
        Me.Font = New Font("Maiandra GD", 12.0!, FontStyle.Bold)
        Me.Size = New Size(520, 364)
        Me.Text = "Calculator"
        AddLabel(11, 9, "You have 200 seconds. After every 40 seconds you can")
        AddLabel(11, 28, "proceed to the next level if you have enough points. A right")
        AddLabel(11, 47, "answer gives you 2-158 points, depending on the level; with")
        AddLabel(11, 66, "a wrong answer you lose 5 points. Good luck!")
        Dim tp As Integer() = {88, 117, 146, 175, 204, 233, 262, 291}
        Dim txt As String() = {"Addition", "Subtraction", "Multiplication", "Division", "Remainder", "Exponentiation", "Roots", "Logarithms"}
        For i As Integer = 0 To 7
            CB(i) = NewCheckBox(12, tp(i), txt(i))
        Next
        BStart = New Button
        BStart.BackColor = Color.LimeGreen
        BStart.Enabled = False
        BStart.Location = New Point(250, 200)
        BStart.Size = New Size(90, 40)
        BStart.Text = "Go!"
        Me.Controls.Add(BStart)
    End Sub
    Function NewCheckBox(l As Integer, t As Integer, txt As String) As CheckBox
        Dim c As CheckBox = New CheckBox
        AddHandler c.CheckedChanged, AddressOf CBCheck
        c.AutoSize = True
        c.Location = New Point(l, t)
        c.Text = txt
        Me.Controls.Add(c)
        Return c
    End Function
End Class