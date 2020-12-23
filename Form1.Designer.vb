<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Form1
    Inherits System.Windows.Forms.Form

    'Form 重写 Dispose，以清理组件列表。
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Windows 窗体设计器所必需的
    Private components As System.ComponentModel.IContainer

    '注意: 以下过程是 Windows 窗体设计器所必需的
    '可以使用 Windows 窗体设计器修改它。  
    '不要使用代码编辑器修改它。
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Me.PBox = New System.Windows.Forms.PictureBox()
        Me.TBox1 = New System.Windows.Forms.TextBox()
        Me.TBox2 = New System.Windows.Forms.TextBox()
        Me.SS1 = New System.Windows.Forms.StatusStrip()
        Me.TSPB1 = New System.Windows.Forms.ToolStripProgressBar()
        Me.TSSL1 = New System.Windows.Forms.ToolStripStatusLabel()
        Me.Tmr1 = New System.Windows.Forms.Timer(Me.components)
        CType(Me.PBox, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SS1.SuspendLayout()
        Me.SuspendLayout()
        '
        'PBox
        '
        Me.PBox.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.PBox.Location = New System.Drawing.Point(12, 12)
        Me.PBox.Name = "PBox"
        Me.PBox.Size = New System.Drawing.Size(800, 600)
        Me.PBox.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom
        Me.PBox.TabIndex = 0
        Me.PBox.TabStop = False
        '
        'TBox1
        '
        Me.TBox1.AcceptsReturn = True
        Me.TBox1.Location = New System.Drawing.Point(829, 12)
        Me.TBox1.Multiline = True
        Me.TBox1.Name = "TBox1"
        Me.TBox1.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.TBox1.Size = New System.Drawing.Size(250, 573)
        Me.TBox1.TabIndex = 4
        '
        'TBox2
        '
        Me.TBox2.Location = New System.Drawing.Point(829, 591)
        Me.TBox2.Name = "TBox2"
        Me.TBox2.Size = New System.Drawing.Size(250, 21)
        Me.TBox2.TabIndex = 3
        '
        'SS1
        '
        Me.SS1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.TSPB1, Me.TSSL1})
        Me.SS1.Location = New System.Drawing.Point(0, 629)
        Me.SS1.Name = "SS1"
        Me.SS1.Size = New System.Drawing.Size(1104, 22)
        Me.SS1.TabIndex = 5
        Me.SS1.Text = "StatusStrip1"
        '
        'TSPB1
        '
        Me.TSPB1.Name = "TSPB1"
        Me.TSPB1.Size = New System.Drawing.Size(200, 16)
        '
        'TSSL1
        '
        Me.TSSL1.Name = "TSSL1"
        Me.TSSL1.Size = New System.Drawing.Size(13, 17)
        Me.TSSL1.Text = "-"
        '
        'Tmr1
        '
        Me.Tmr1.Enabled = True
        Me.Tmr1.Interval = 500
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 12.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(1104, 651)
        Me.Controls.Add(Me.SS1)
        Me.Controls.Add(Me.TBox2)
        Me.Controls.Add(Me.TBox1)
        Me.Controls.Add(Me.PBox)
        Me.Name = "Form1"
        Me.Text = "Form1"
        CType(Me.PBox, System.ComponentModel.ISupportInitialize).EndInit()
        Me.SS1.ResumeLayout(False)
        Me.SS1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents PBox As PictureBox
    Friend WithEvents TBox1 As TextBox
    Friend WithEvents TBox2 As TextBox
    Friend WithEvents SS1 As StatusStrip
    Friend WithEvents TSPB1 As ToolStripProgressBar
    Friend WithEvents Tmr1 As Timer
    Friend WithEvents TSSL1 As ToolStripStatusLabel
End Class
