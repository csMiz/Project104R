Imports System.Numerics
Imports System.Text.RegularExpressions

Public Class Form1

    Private TimerStartAt As Date

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        ' test code
        'LightRepository.Add(New PointLight() With {
        '                    .Position = New Vector3(1, 3.5, -1),
        '                    .Color = New Vector3(1.0F),
        '                    .Strength = 20.0F})
        'LightRepository.Add(New PointLight() With {
        '            .Position = New Vector3(50, 30, 50),
        '            .Color = New Vector3(200, 147, 196),
        '            .Strength = 50000.0F})

        LightRepository.Add(New DirectionalLight() With {
                    .LightDirection = Vector3.Normalize(New Vector3(-1, 0, -1)),
                    .Color = New Vector3(200, 147, 196),
                    .Strength = 1.0F})

        ObjLoader.MatRepo.Add("s_light1", New ModelMaterial With {.Emission = New Vector3(1.0F), .EmissionStrength = 6.67F})
        ObjLoader.MatRepo.Add("s_light2", New ModelMaterial With {.Emission = New Vector3(0.9, 0.9, 0.5), .EmissionStrength = 50.0F})

        'LightRepository.Add(New SphereLight() With {
        '            .Position = New Vector3(0, 0, 0),
        '            .Radius = 200.0F,
        '            .LightMaterial = "s_light1"})
        'LightRepository.Add(New SphereLight() With {
        '    .Position = New Vector3(50, 30, 50),
        '    .Radius = 10.0F,
        '    .Strength = 50000.0F,
        '    .LightMaterial = "s_light2"})

        'Dim lightPos As SharpDX.Vector3 = New SharpDX.Vector3(200, 30, 200)
        'Dim targetPos As SharpDX.Vector3 = New SharpDX.Vector3(0, 30, 0)
        'Dim lightWVP As SharpDX.Matrix = RasterizerCamera.CalculateWVP_LookAt(lightPos, targetPos)
        'Dim inputMat As SharpDX.Matrix = New SharpDX.Matrix(0)
        'With inputMat
        '    .M11 = 0
        '    .M21 = 30
        '    .M31 = 0
        '    .M41 = 1
        'End With
        'Dim resultMat As SharpDX.Matrix = lightWVP * inputMat
        'Console.WriteLine(resultMat.M11 & "  " & resultMat.M21 & "  " & resultMat.M31 & "  " & resultMat.M41)

        'Dim lastv As New Vector3(0, 0.174, 16.674)
        'Dim vec As New Vector3(1.84, 0, 0)
        'Dim mat As Matrix4x4 = Matrix4x4.CreateFromYawPitchRoll(1.57, 0, 0)
        'Matrix4x4.Invert(mat, mat)
        'Dim r As Vector3 = Vector3.Transform(vec, mat)
        'r += lastv
        'MsgBox(r.ToString)

    End Sub

    Private Sub TBox2_KeyDown(sender As Object, e As KeyEventArgs) Handles TBox2.KeyDown
        If e.KeyCode = Keys.Enter Then
            e.Handled = True
            TBox2.Text = TBox2.Text.Replace(vbCrLf, vbNullString)
            RunCmd(TBox2.Text)
            TBox2.Text = vbNullString
        End If
    End Sub

    Public Async Sub RunCmd(line As String)
        PostMsg(line)
        Dim args As String() = Regex.Split(line, " ")
        Dim cmd As String = args(0).ToLower
        Dim argCount As Integer = args.Length
        If argCount = 1 Then
            Select Case cmd
                Case "render"
                    TimerStartAt = DateTime.Now
                    PBox.Image = Nothing
                    If AABBRepository IsNot Nothing OrElse BVHRepository IsNot Nothing Then
                        BeginRender(True)
                    Else
                        BeginRender()
                    End If
                Case "load"
                    Dim ofd As New OpenFileDialog With {
                        .Filter = "Obj File|*.obj",
                        .RestoreDirectory = True}
                    If (ofd.ShowDialog = DialogResult.OK) Then
                        ModelRepository.AddRange(ObjLoader.ReadObject(ofd.FileName, 1.0F))
                    End If
                    RunCmd("aabb")
                Case "loadma"
                    Dim ofd As New OpenFileDialog With {
                        .Filter = "Morph Animation File|*.xml",
                        .RestoreDirectory = True}
                    If (ofd.ShowDialog = DialogResult.OK) Then
                        Dim ma As New MorphAnimation
                        ma.LoadAnimation(ofd.FileName)
                        MorphAnimationRepository(ma.AnimationName) = ma
                    End If
                Case "smd"
                    Dim ofd As New OpenFileDialog With {
                        .Filter = "SMD File|*.smd",
                        .RestoreDirectory = True}
                    If (ofd.ShowDialog = DialogResult.OK) Then
                        TimerStartAt = DateTime.Now
                        ModelSMD = New SMDLoader
                        ModelSMD.LoadSMD(ofd.FileName)
                        Dim tEnd As Date = DateTime.Now
                        PostMsg("共用时: " & (tEnd - TimerStartAt).TotalSeconds & "秒")
                        RunCmd("resetsa")
                    End If
                Case "smdlink"
                    TimerStartAt = DateTime.Now
                    LinkSMDSkinToObj()
                    Dim tEnd As Date = DateTime.Now
                    PostMsg("共用时: " & (tEnd - TimerStartAt).TotalSeconds & "秒")
                    RunCmd("savelink")
                Case "savelink"
                    SaveLinkInfo("C:\Users\asdfg\Desktop\rtTest\link.txt")
                Case "loadlink"
                    LoadLinkInfo("C:\Users\asdfg\Desktop\rtTest\link.txt")
                Case "printmat"
                    For Each matName As String In ObjLoader.MatRepo.Keys
                        PostMsg(matName)
                    Next
                Case "printobj"
                    For Each obj As Model In ModelRepository
                        PostMsg(obj.Name)
                    Next
                Case "printbone"
                    DisplayBones()
                Case "ra"
                    If Spectator Is Nothing Then
                        InitializeRasterizer()
                    End If
                    RasterizerUpdateModels()
                    TimerStartAt = DateTime.Now
                    Spectator.PaintImage()
                    Dim tEnd As Date = DateTime.Now
                    PostMsg("共用时: " & (tEnd - TimerStartAt).TotalSeconds & "秒")
                Case "rasa"
                    If Spectator Is Nothing Then
                        InitializeRasterizer()
                    End If
                    RunCmd("applysa")
                    RasterizerUpdateModels_SA()
                    TimerStartAt = DateTime.Now
                    Spectator.PaintImage()
                    Dim tEnd As Date = DateTime.Now
                    PostMsg("共用时: " & (tEnd - TimerStartAt).TotalSeconds & "秒")
                Case "cls"
                    TBox1.Text = ""
                Case "reset"

                Case "resetma"
                    ObjLoader_ma_apply = ObjLoader_ma_ref.Clone()
                Case "resetsa"
                    ObjLoader_sa_apply = ObjLoader.Clone()
                    ObjLoader_sa_middle = ObjLoader.Clone()
                Case "prema"
                    ObjLoader_ma_ref.ReadObject(ObjLoader.ObjFileName, 1.0)
                    ObjLoader_ma_apply = ObjLoader_ma_ref.Clone()
                Case "applysa"
                    ApplySkinMiddle()
                Case "aabb"
                    GenerateAABB()
                Case "caabb"
                    AABBRepository = Nothing
                    BVHRepository = Nothing
                Case "pt"
                    RunCmd("pt 100")
                Case "disparity"
                    GenerateDisparityMA()
                Case "disparity_uv_scale"
                    GenerateDisparityMA_UV_Scale()
                Case "test"
                    For i = 0 To 100
                        ObjLoader_ma_apply = ObjLoader_ma_ref.Clone()
                        MorphAnimationRepository.Values(0).CurrentFrame = i
                        MorphAnimationRepository.Values(0).ApplyAnimation()
                        RunCmd("ra")
                        Await Task.Delay(TimeSpan.FromMilliseconds(33))
                        Application.DoEvents()
                    Next

                    'Dim rot As Matrix4x4 = ModelSMD.GetBoneRotation(31)
                    'Console.WriteLine(rot.ToString)
                    'Matrix4x4.Invert(rot, rot)
                    'Console.WriteLine(rot.ToString)

                Case "script"
                    LoadMotionScript()

            End Select
        ElseIf argCount = 2 Then
            Select Case cmd
                Case "select"

                Case "campos"
                    Dim segs As String() = args(1).Split(",")
                    CameraPosition = New Vector3(CSng(segs(0)), CSng(segs(1)), CSng(segs(2)))
                Case "camrot"
                    Dim segs As String() = args(1).Split(",")
                    CameraRotation = New Vector3(CSng(segs(0)), CSng(segs(1)), CSng(segs(2)))
                Case "aabb"
                    DisplayAABB(CInt(args(1)))
                Case "pt"
                    If ImageBuffer IsNot Nothing Then ImageBuffer.Dispose()
                    ReDim PTBuffer(ImageWidth - 1, ImageHeight - 1)
                    For i = 0 To ImageWidth - 1
                        For j = 0 To ImageHeight - 1
                            PTBuffer(i, j) = Vector3.Zero
                        Next
                    Next
                    Dim maxSample As Integer = CInt(args(1))
                    For i = 0 To maxSample - 1
                        Dim oldImage As Image = PBox.Image
                        PTRenderSample(i)
                        PBox.Image = New Bitmap(ImageBuffer)
                        If oldImage IsNot Nothing Then oldImage.Dispose()
                        TSSL1.Text = "采样: " & (i + 1).ToString & " / " & (maxSample).ToString
                        TSPB1.Value = Math.Floor((i + 1) * 100.0 / maxSample)
                        Application.DoEvents()
                    Next
                Case "ma_clip_left"
                    Dim anim As MorphAnimation = MorphAnimationRepository(args(1))
                    Dim anim2 As MorphAnimation = anim.Left
                    anim2.SaveAnimation("C:\Users\asdfg\Desktop\rtTest\leftMA.xml")
                Case "ma_clip_right"
                    Dim anim As MorphAnimation = MorphAnimationRepository(args(1))
                    Dim anim2 As MorphAnimation = anim.Right
                    anim2.SaveAnimation("C:\Users\asdfg\Desktop\rtTest\rightMA.xml")
                Case "ma_clip_left2"
                    Dim anim As MorphAnimation = MorphAnimationRepository(args(1))
                    Dim anim2 As MorphAnimation = anim.Left2
                    anim2.SaveAnimation("C:\Users\asdfg\Desktop\rtTest\leftMA.xml")
                Case "ma_clip_right2"
                    Dim anim As MorphAnimation = MorphAnimationRepository(args(1))
                    Dim anim2 As MorphAnimation = anim.Right2
                    anim2.SaveAnimation("C:\Users\asdfg\Desktop\rtTest\rightMA.xml")

            End Select
        ElseIf argCount = 3 Then
            Select Case cmd
                Case "size"
                    ImageWidth = CInt(args(1))
                    ImageHeight = CInt(args(2))
                Case "setma"
                    MorphAnimationRepository(args(1)).CurrentFrame = CInt(args(2))
                    MorphAnimationRepository(args(1)).ApplyAnimation()
                Case "bonerot"
                    Dim boneIdx As Integer = CInt(args(1))   '32
                    Dim segs As String() = args(2).Split(",")
                    Dim eulerDelta As Vector3 = New Vector3(CSng(segs(0)), CSng(segs(1)), CSng(segs(2)))
                    SetBoneRotate(boneIdx, eulerDelta)

            End Select
        ElseIf argCount = 4 Then
            Select Case cmd
                Case "setmat"
                    Dim matIdx As String = args(1)
                    Dim matAttr As String = args(2)
                    Dim val As String = args(3)
                    Dim tar As ModelMaterial = ObjLoader.MatRepo(matIdx)
                    If matAttr = "emitv" Then
                        tar.EmissionStrength = CSng(val)
                    ElseIf matAttr = "emitc" Then
                        If val = "d" Then
                            tar.Emission = tar.Diffuse
                            tar.EmissionTexture = tar.DiffuseTexture
                        Else
                            Dim rgb As String() = val.Split(",")
                            tar.Emission = New Vector3(CSng(rgb(0)), CSng(rgb(1)), CSng(rgb(2)))
                            tar.EmissionTexture = Nothing
                        End If
                    ElseIf matAttr = "dc" Then
                        Dim rgb As String() = val.Split(",")
                        tar.Diffuse = New Vector3(CSng(rgb(0)), CSng(rgb(1)), CSng(rgb(2)))
                        tar.DiffuseTexture = Nothing
                    End If

            End Select
        ElseIf argCount = 5 Then

        End If

    End Sub

    Public Sub PostMsg(txt As String)
        TBox1.Text = TBox1.Text & txt & vbCrLf
    End Sub

    Private Sub PrintSingleBVH(BVH As BVH, graph As Graphics, depth As Integer, currentDepth As Integer)
        Const ZOOM As Single = 10.0F
        If currentDepth = depth Then
            graph.DrawRectangle(Pens.Black, New Rectangle(400 + BVH.Region(0).X * ZOOM, 300 - BVH.Region(1).Y * ZOOM, (BVH.Region(1).X - BVH.Region(0).X) * ZOOM, (BVH.Region(1).Y - BVH.Region(0).Y) * ZOOM))
        Else
            If BVH.Content Is Nothing Then
                PrintSingleBVH(BVH.Children(0), graph, depth, currentDepth + 1)
                PrintSingleBVH(BVH.Children(1), graph, depth, currentDepth + 1)
            Else
                graph.DrawRectangle(Pens.Red, New Rectangle(400 + BVH.Region(0).X * ZOOM, 300 - BVH.Region(1).Y * ZOOM, (BVH.Region(1).X - BVH.Region(0).X) * ZOOM, (BVH.Region(1).Y - BVH.Region(0).Y) * ZOOM))
            End If
        End If
    End Sub

    Public Sub DisplayAABB(depth As Integer)
        Dim bitmap As New Bitmap(800, 600)
        Dim g As Graphics = Graphics.FromImage(bitmap)
        g.Clear(Color.White)
        PrintSingleBVH(BVHRepository, g, depth, 0)
        g.Dispose()
        PBox.Image = bitmap

    End Sub

    Public Sub DisplayBones()
        Dim bitmap As New Bitmap(800, 600)
        Dim g As Graphics = Graphics.FromImage(bitmap)
        g.Clear(Color.White)

        For Each i In ModelSMD.Nodes.Keys
            If i >= 0 Then
                Dim bonePos As Vector3 = ModelSMD.GetBonePosition(i)
                Dim boneRot As Matrix4x4 = ModelSMD.GetBoneRotation(i)
                Dim ori As New Vector3(1, 0, 0)
                ori = Vector3.Transform(ori, boneRot)

                g.DrawEllipse(Pens.Black, New Rectangle(bonePos.X * 10 + 400 - 5, -bonePos.Y * 10 + 300 - 5, 10, 10))
                g.DrawLine(Pens.Black, New PointF(bonePos.X * 10 + 400, -bonePos.Y * 10 + 300), New PointF((bonePos.X + ori.X) * 10 + 400, -(bonePos.Y + ori.Y) * 10 + 300))

                g.DrawEllipse(Pens.Red, New Rectangle(bonePos.Z * 10 + 600 - 5, -bonePos.Y * 10 + 300 - 5, 10, 10))
                g.DrawLine(Pens.Red, New PointF(bonePos.Z * 10 + 600, -bonePos.Y * 10 + 300), New PointF((bonePos.Z + ori.Z) * 10 + 600, -(bonePos.Y + ori.Y) * 10 + 300))
            End If
        Next

        g.Dispose()
        PBox.Image = bitmap

    End Sub

    Private Sub Tmr1_Tick(sender As Object, e As EventArgs) Handles Tmr1.Tick
        Dim prog As Single = ImageBufferFinishCount * 1.0F / (ImageWidth * ImageHeight)
        TSPB1.Value = prog * 100
        If prog = 1.0F Then
            PBox.Image = ImageBuffer
            ImageBufferFinishCount = 0

            Dim tEnd As Date = DateTime.Now
            PostMsg("共用时: " & (tEnd - TimerStartAt).TotalSeconds & "秒")
        End If
    End Sub

    Private Sub PBox_Click(sender As Object, e As EventArgs) Handles PBox.Click

    End Sub

    Private Sub PBox_MouseDown(sender As Object, e As MouseEventArgs) Handles PBox.MouseDown
        Dim pos_str As String = (e.X * ImageWidth / PBox.Width).ToString & ", " & (e.Y * ImageHeight / PBox.Height).ToString
        PostMsg(pos_str)

    End Sub

End Class
