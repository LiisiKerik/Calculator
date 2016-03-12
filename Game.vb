Public Class Game
    WithEvents LMessage As Label
    Dim AddMax, Cut, ExpMax, Level, LevelMax, MessageTimer, MltMax, Operation, Points, Recent, RightAnswer, TaskLevel, MyTimer As Integer
    Public NumberOfOperations
    Public Range(7) As Integer
    WithEvents B0, BOK, BExit, BSettings, BStartStop, B8, B9, B1, B2, B3, B4, B5, B6, BDelete, B7 As Button
    Dim LLevel, LPoints, LAnswer, LLevelTime, LLevelTime2, LTime, LPointsNeeded1, LPointsNeeded2, LPointsNeeded, LTask, LTiny As Label
    WithEvents Time As Timer = New Timer
    Private Sub B0_Click() Handles B0.Click
        Print(0)
    End Sub
    Private Sub B1_Click() Handles B1.Click
        Print(1)
    End Sub
    Private Sub B2_Click() Handles B2.Click
        Print(2)
    End Sub
    Private Sub B3_Click() Handles B3.Click
        Print(3)
    End Sub
    Private Sub B4_Click() Handles B4.Click
        Print(4)
    End Sub
    Private Sub B5_Click() Handles B5.Click
        Print(5)
    End Sub
    Private Sub B6_Click() Handles B6.Click
        Print(6)
    End Sub
    Private Sub B7_Click() Handles B7.Click
        Print(7)
    End Sub
    Private Sub B8_Click() Handles B8.Click
        Print(8)
    End Sub
    Private Sub B9_Click() Handles B9.Click
        Print(9)
    End Sub
    Private Sub BDelete_Click() Handles BDelete.Click
        LAnswer.Text = ""
    End Sub
    Sub BExit_Click() Handles BExit.Click
        Form1.Close()
    End Sub
    Private Sub BOK_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BOK.Click
        If LAnswer.Text = "" Then
            Wrong()
        Else
            If CInt(LAnswer.Text) = RightAnswer Then
                Points += 39 * TaskLevel - 37
                If NumberOfOperations > 1 Then
                    Cut = Range(Recent) \ 8
                    Do
                        If Cut Mod NumberOfOperations - 1 = 0 Then
                            Exit Do
                        End If
                        Cut -= 1
                    Loop
                    Range(Recent) -= Cut
                    Cut /= NumberOfOperations - 1
                    For i As Integer = 0 To 7
                        If Form1.OperationMin(i) > 0 And i <> Recent Then
                            Range(i) += Cut
                        End If
                    Next
                    SetRange()
                End If
            Else
                Wrong()
            End If
        End If
        LAnswer.Text = ""
        If Math.Abs(Points) = 1 Then
            LPoints.Text = CStr(Points) & " Point"
        Else
            LPoints.Text = CStr(Points) & " Points"
        End If
        TaskGenerator()
    End Sub
    Private Sub BSettings_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BSettings.Click
        Me.Close()
    End Sub
    Private Sub BStartStop_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BStartStop.Click
        If Time.Enabled = True Then
            Finish()
            HideSomeLs()
        Else
            B0.Enabled = True
            B1.Enabled = True
            B2.Enabled = True
            B3.Enabled = True
            B4.Enabled = True
            B5.Enabled = True
            B6.Enabled = True
            B7.Enabled = True
            B8.Enabled = True
            B9.Enabled = True
            BDelete.Enabled = True
            BExit.Visible = False
            BOK.Enabled = True
            BSettings.Visible = False
            BStartStop.BackColor = Color.Crimson
            BStartStop.Text = "Finish"
            Level = 1
            LLevel.Text = "Level 1"
            LLevel.Visible = True
            LLevelTime.Visible = True
            LLevelTime2.Visible = True
            LMessage.Text = ""
            LPoints.Text = "0 Points"
            LPoints.Visible = True
            LPointsNeeded.Visible = True
            LPointsNeeded1.Visible = True
            LPointsNeeded2.Visible = True
            LTime.Visible = True
            Points = 0
            TaskGenerator()
            MyTimer = 0
            Time.Start()
        End If
    End Sub
    Sub Finish()
        B0.Enabled = False
        B1.Enabled = False
        B2.Enabled = False
        B3.Enabled = False
        B4.Enabled = False
        B5.Enabled = False
        B6.Enabled = False
        B7.Enabled = False
        B8.Enabled = False
        B9.Enabled = False
        BDelete.Enabled = False
        BExit.Visible = True
        BOK.Enabled = False
        BSettings.Visible = True
        BStartStop.BackColor = Color.LimeGreen
        BStartStop.Text = "Start"
        LAnswer.Text = ""
        LLevel.Visible = False
        LMessage.BackColor = Color.LimeGreen
        If Math.Abs(Points) = 1 Then
            LMessage.Text = "Your result - " & CStr(Points) & " point."
        Else
            LMessage.Text = "Your result - " & CStr(Points) & " points."
        End If
        LPoints.Visible = False
        LTask.Text = ""
        LTime.Visible = False
        LTiny.Text = ""
        Me.BackColor = SystemColors.Control
        Time.Stop()
    End Sub
    Sub HideSomeLs()
        LLevelTime.Visible = False
        LLevelTime2.Visible = False
        LPointsNeeded.Visible = False
        LPointsNeeded1.Visible = False
        LPointsNeeded2.Visible = False
    End Sub
    Sub Print(ByVal i As Integer)
        If LAnswer.Text.Length < 9 Then
            LAnswer.Text &= CStr(i)
        End If
    End Sub
    Sub LMessage_TextChanged() Handles LMessage.TextChanged
        MessageTimer = MyTimer + 3
    End Sub
    Sub Me_Closed() Handles Me.Closed
        Form1.Show()
    End Sub
    Sub Me_Load() Handles Me.Load
        Me.Font = New Font("Maiandra GD", 15.75!, FontStyle.Bold)
        Me.ClientSize = New Size(584, 564)
        Me.Text = "Calculator"
        Time.Interval = 1000
        B0 = New Button
        B0.BackColor = Color.LightGreen
        B0.Enabled = False
        B0.Location = New Point(30, 428)
        B0.Size = New Size(80, 80)
        B0.Text = "0"
        Me.Controls.Add(B0)
        BOK = New Button
        BOK.BackColor = Color.LightGreen
        BOK.Enabled = False
        BOK.Location = New Point(116, 428)
        BOK.Size = New Size(166, 80)
        BOK.Text = "OK"
        Me.Controls.Add(BOK)
        BExit = New Button
        BExit.BackColor = Color.Crimson
        BExit.Location = New Point(512, 12)
        BExit.Size = New Size(60, 40)
        BExit.Text = "Exit"
        Me.Controls.Add(BExit)
        BSettings = New Button
        BSettings.BackColor = Color.Yellow
        BSettings.Location = New Point(406, 12)
        BSettings.Size = New Size(100, 40)
        BSettings.Text = "Settings"
        Me.Controls.Add(BSettings)
        BStartStop = New Button
        BStartStop.BackColor = Color.LimeGreen
        BStartStop.Location = New Point(320, 12)
        BStartStop.Size = New Size(80, 40)
        BStartStop.Text = "Start"
        Me.Controls.Add(BStartStop)
        B8 = New Button
        B8.BackColor = Color.LightGreen
        B8.Enabled = False
        B8.Location = New Point(116, 170)
        B8.Size = New Size(80, 80)
        B8.Text = "8"
        Me.Controls.Add(B8)
        B9 = New Button
        B9.BackColor = Color.LightGreen
        B9.Enabled = False
        B9.Location = New Point(202, 170)
        B9.Size = New Size(80, 80)
        B9.Text = "9"
        Me.Controls.Add(B9)
        B1 = New Button
        B1.BackColor = Color.LightGreen
        B1.Enabled = False
        B1.Location = New Point(30, 342)
        B1.Size = New Size(80, 80)
        B1.Text = "1"
        Me.Controls.Add(B1)
        B2 = New Button
        B2.BackColor = Color.LightGreen
        B2.Enabled = False
        B2.Location = New Point(116, 342)
        B2.Size = New Size(80, 80)
        B2.Text = "2"
        Me.Controls.Add(B2)
        B3 = New Button
        B3.BackColor = Color.LightGreen
        B3.Enabled = False
        B3.Location = New Point(202, 342)
        B3.Size = New Size(80, 80)
        B3.Text = "3"
        Me.Controls.Add(B3)
        B4 = New Button
        B4.BackColor = Color.LightGreen
        B4.Enabled = False
        B4.Location = New Point(30, 256)
        B4.Size = New Size(80, 80)
        B4.Text = "4"
        Me.Controls.Add(B4)
        B5 = New Button
        B5.BackColor = Color.LightGreen
        B5.Enabled = False
        B5.Location = New Point(116, 256)
        B5.Size = New Size(80, 80)
        B5.Text = "5"
        Me.Controls.Add(B5)
        B6 = New Button
        B6.BackColor = Color.LightGreen
        B6.Enabled = False
        B6.Location = New Point(202, 256)
        B6.Size = New Size(80, 80)
        B6.Text = "6"
        Me.Controls.Add(B6)
        BDelete = New Button
        BDelete.BackColor = Color.LightCoral
        BDelete.Enabled = False
        BDelete.Font = New Font("Maiandra GD", 14.25!, FontStyle.Bold)
        BDelete.Location = New Point(288, 428)
        BDelete.Size = New Size(80, 80)
        BDelete.Text = "Delete"
        Me.Controls.Add(BDelete)
        B7 = New Button
        B7.BackColor = Color.LightGreen
        B7.Enabled = False
        B7.Location = New Point(30, 170)
        B7.Size = New Size(80, 80)
        B7.Text = "7"
        Me.Controls.Add(B7)
        LLevel = New Label
        LLevel.AutoSize = True
        LLevel.Location = New Point(12, 9)
        LLevel.Size = New Size(79, 25)
        LLevel.Text = "Level 1"
        Me.Controls.Add(LLevel)
        LPoints = New Label
        LPoints.AutoSize = True
        LPoints.Location = New Point(120, 9)
        LPoints.Size = New Size(93, 25)
        LPoints.Text = "0 Points"
        Me.Controls.Add(LPoints)
        LMessage = New Label
        LMessage.AutoSize = True
        LMessage.Location = New Point(250, 70)
        LMessage.Size = New Size(0, 25)
        Me.Controls.Add(LMessage)
        LAnswer = New Label
        LAnswer.AutoSize = True
        LAnswer.Location = New Point(32, 100)
        LAnswer.Size = New Size(0, 25)
        Me.Controls.Add(LAnswer)
        LLevelTime = New Label
        LLevelTime.AutoSize = True
        LLevelTime.Location = New Point(451, 225)
        LLevelTime.Name = "LLevelTime"
        LLevelTime.Size = New Size(121, 25)
        LLevelTime.Text = "40 seconds"
        LLevelTime.Visible = False
        Me.Controls.Add(LLevelTime)
        LLevelTime2 = New Label
        LLevelTime2.AutoSize = True
        LLevelTime2.Location = New Point(435, 250)
        LLevelTime2.Size = New Size(137, 25)
        LLevelTime2.Text = "for this level"
        LLevelTime2.Visible = False
        Me.Controls.Add(LLevelTime2)
        LTime = New Label
        LTime.AutoSize = True
        LTime.Location = New Point(438, 170)
        LTime.Size = New Size(134, 25)
        LTime.Text = "200 seconds"
        LTime.Visible = False
        Me.Controls.Add(LTime)
        LPointsNeeded1 = New Label
        LPointsNeeded1.AutoSize = True
        LPointsNeeded1.Location = New Point(378, 311)
        LPointsNeeded1.Size = New Size(194, 25)
        LPointsNeeded1.Text = "To get to the next"
        LPointsNeeded1.Visible = False
        Me.Controls.Add(LPointsNeeded1)
        LPointsNeeded2 = New Label
        LPointsNeeded2.AutoSize = True
        LPointsNeeded2.Location = New Point(414, 336)
        LPointsNeeded2.Size = New Size(158, 25)
        LPointsNeeded2.Text = "level you need"
        LPointsNeeded2.Visible = False
        Me.Controls.Add(LPointsNeeded2)
        LPointsNeeded = New Label
        LPointsNeeded.AutoSize = True
        LPointsNeeded.Location = New Point(440, 361)
        LPointsNeeded.Size = New Size(106, 25)
        LPointsNeeded.Text = "20 points"
        LPointsNeeded.Visible = False
        Me.Controls.Add(LPointsNeeded)
        LTask = New Label
        LTask.AutoSize = True
        LTask.Location = New Point(12, 100)
        LTask.Size = New Size(0, 25)
        Me.Controls.Add(LTask)
        LTiny = New Label
        LTiny.AutoSize = True
        LTiny.BackColor = Color.Transparent
        LTiny.Font = New Font("Maiandra GD", 9.75!, FontStyle.Bold)
        LTiny.Location = New Point(320, 320)
        LTiny.Size = New Size(0, 16)
        Me.Controls.Add(LTiny)
    End Sub
    Sub SetRange()
        Dim Last As Integer
        Last = 0
        For i As Integer = 0 To 7
            If Form1.OperationMin(i) > 0 Then
                Form1.OperationMin(i) = Last + 1
                Last += Range(i)
                Form1.OperationMax(i) = Last
            End If
        Next
    End Sub
    Sub TaskGenerator()
        Dim First, Second As Integer
        LTiny.Text = ""
        Randomize()
        Operation = CInt(Rnd() * (840)) + 1
        Select Case Operation
            Case Form1.OperationMin(0) To Form1.OperationMax(0)
                Recent = 0
                If Level = 1 Then
                    First = CInt(Rnd() * 4)
                    Second = CInt(Rnd() * 4)
                Else
                    First = CInt(Rnd() * (AddMax - 1)) + 1
                    Second = CInt(Rnd() * (AddMax - 1)) + 1
                End If
                RightAnswer = First + Second
                LTask.Text = CStr(First) & " + " & CStr(Second) & " ="
            Case Form1.OperationMin(1) To Form1.OperationMax(1)
                Recent = 1
                If Level = 1 Then
                    RightAnswer = CInt(Rnd() * 4)
                    Second = CInt(Rnd() * 4)
                Else
                    RightAnswer = CInt(Rnd() * (AddMax - 1)) + 1
                    Second = CInt(Rnd() * (AddMax - 1)) + 1
                End If
                First = RightAnswer + Second
                LTask.Text = CStr(First) & " - " & CStr(Second) & " ="
            Case Form1.OperationMin(2) To Form1.OperationMax(2)
                Recent = 2
                If Level = 1 Then
                    First = CInt(Rnd() * MltMax)
                    Second = CInt(Rnd() * MltMax)
                Else
                    First = CInt(Rnd() * (MltMax - 2)) + 2
                    Second = CInt(Rnd() * (MltMax - 2)) + 2
                End If
                RightAnswer = First * Second
                LTask.Text = CStr(First) & " x " & CStr(Second) & " ="
            Case Form1.OperationMin(3) To Form1.OperationMax(3)
                Recent = 3
                If Level = 1 Then
                    RightAnswer = CInt(Rnd() * 2) + 1
                    Second = CInt(Rnd() * 2) + 1
                Else
                    RightAnswer = CInt(Rnd() * (MltMax - 2)) + 2
                    Second = CInt(Rnd() * (MltMax - 2)) + 2
                End If
                First = RightAnswer * Second
                LTask.Text = CStr(First) & " / " & CStr(Second) & " ="
            Case Form1.OperationMin(4) To Form1.OperationMax(4)
                Recent = 4
                If Level = 1 Then
                    First = CInt(Rnd() * 9)
                    Second = CInt(Rnd() * 2) + 1
                Else
                    First = CInt(Rnd() * (LevelMax - 1)) + 1
                    Second = CInt(Rnd() * (MltMax - 2)) + 2
                End If
                RightAnswer = First Mod Second
                LTask.Text = CStr(First) & " mod " & CStr(Second) & " ="
            Case Form1.OperationMin(5) To Form1.OperationMax(5)
                Recent = 5
                LTiny.Top = 100
                If Level = 1 Then
                    First = CInt(Rnd() * 9)
                    If First = 0 Then
                        Second = CInt(Rnd() * 8) + 1
                    ElseIf First = 1 Then
                        Second = CInt(Rnd() * 9)
                    ElseIf First = 2 Then
                        Second = CInt(Rnd() * 3)
                    ElseIf First = 3 Then
                        Second = CInt(Rnd() * 2)
                    Else
                        Second = CInt(Rnd())
                    End If
                Else
                    First = CInt(Rnd() * (ExpMax - 2)) + 2
                    Second = 3
                    Do
                        Second += 1
                        If First ^ Second > ExpMax Then Exit Do
                    Loop
                    Second -= 1
                    Second = CInt(Rnd() * (Second - 3)) + 3
                End If
                LTask.Text = CStr(First) & "     ="
                LTiny.Left = LTask.Right - 50
                LTiny.Text = CStr(Second)
                RightAnswer = First ^ Second
            Case Form1.OperationMin(6) To Form1.OperationMax(6)
                Recent = 6
                LTiny.Left = 12
                LTiny.Top = 100
                If Level = 1 Then
                    RightAnswer = CInt(Rnd() * 3)
                    If RightAnswer < 2 Then
                        First = CInt(Rnd() * 7) + 2
                    ElseIf RightAnswer = 2 Then
                        First = CInt(Rnd() * 2) + 1
                    Else
                        First = 2
                    End If
                Else
                    RightAnswer = CInt(Rnd() * (MltMax - 2)) + 2
                    First = 2
                    Do
                        First += 1
                        If RightAnswer ^ First > MltMax Then Exit Do
                    Loop
                    First -= 1
                    First = CInt(Rnd() * (First - 2)) + 2
                End If
                LTiny.Text = CStr(First)
                Second = RightAnswer ^ First
                LTask.Text = "v" & CStr(Second) & "="
            Case Else
                Recent = 7
                LTiny.Left = 50
                LTiny.Top = 110
                If Level = 1 Then
                    First = CInt(Rnd() * 3) + 2
                    If First = 2 Then
                        RightAnswer = CInt(Rnd() * 4)
                    ElseIf First = 3 Then
                        RightAnswer = CInt(Rnd() * 2)
                    ElseIf First = 4 Then
                        RightAnswer = CInt(Rnd() * 2)
                    ElseIf First = 5 Then
                        RightAnswer = CInt(Rnd() * 2)
                    End If
                Else
                    First = CInt(Rnd() * (MltMax - 2)) + 2
                    RightAnswer = 2
                    Do
                        RightAnswer += 1
                        If First ^ RightAnswer > MltMax Then Exit Do
                    Loop
                    RightAnswer -= 1
                    RightAnswer = CInt(Rnd() * (RightAnswer - 2)) + 2
                End If
                LTiny.Text = CStr(First)
                Second = First ^ RightAnswer
                LTask.Text = "log   " & CStr(Second) & " ="
        End Select
        LAnswer.Left = LTask.Right + 5
        TaskLevel = Level
    End Sub
    Public Function TellingTime(ByVal Seconds As Integer) As String
        If Seconds = 1 Then
            Return CStr(Seconds) + " second"
        Else
            Return CStr(Seconds) + " seconds"
        End If
    End Function
    Sub Time_Tick() Handles Time.Tick
        MyTimer += 1
        If MyTimer = 200 Then
            Finish()
            Exit Sub
        End If
        If MyTimer = MessageTimer Then
            LMessage.BackColor = SystemColors.Control
            LMessage.Text = ""
            Me.BackColor = SystemColors.Control
        End If
        If MyTimer = 40 Or MyTimer = 80 Or MyTimer = 120 Or MyTimer = 160 Then
            Dim OldLevel As Integer
            If MyTimer = 160 Then
                HideSomeLs()
            End If
            OldLevel = Level
            If Points < 20 Then
                Level = 1
            ElseIf Points >= 20 And Points < 389 Then
                Level = 2
            ElseIf Points >= 389 And Points < 949 Then
                Level = 3
            ElseIf Points >= 949 And Points < 1425 Then
                Level = 4
            Else
                Level = 5
            End If
            If Level > 1 Then
                LevelMax = 10 ^ Level - 1
                AddMax = LevelMax \ 2
                ExpMax = Math.Floor(LevelMax ^ (1 / 3))
                MltMax = Math.Floor(LevelMax ^ 0.5)
            End If
            LLevel.Text = "Level " & CStr(Level)
            Select Case Level
                Case 1
                    LPointsNeeded.Text = "20 points"
                Case 2
                    LPointsNeeded.Text = "389 points"
                Case 3
                    LPointsNeeded.Text = "949 points"
                Case 4
                    LPointsNeeded.Text = "1425 points"
            End Select
            If Level <= OldLevel Then
                Me.BackColor = Color.Yellow
                LMessage.Text = "Repeating the level."
            Else
                LMessage.BackColor = Color.SteelBlue
                LMessage.Text = "Well done! Now to the next level..."
            End If
        End If
        LLevelTime.Text = Tellingtime(40 * (MyTimer \ 40 + 1) - MyTimer)
        LTime.Text = Tellingtime(200 - MyTimer)
    End Sub
    Sub Wrong()
        Dim Temp(7) As Integer
        LMessage.Text = "Wrong answer!"
        Me.BackColor = Color.Crimson
        Points -= 5
        If NumberOfOperations > 1 Then
            For i As Integer = 0 To 7
                If Form1.OperationMin(i) > 0 And i <> Recent Then
                    Temp(i) = Range(i) \ (8 * (NumberOfOperations - 1))
                    Range(i) += Temp(i)
                    Range(Recent) += Temp(i)
                End If
            Next
            SetRange()
        End If
    End Sub
End Class