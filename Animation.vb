Imports System.IO
Imports System.Numerics
Imports System.Xml

Module Animation

    Public MorphAnimationRepository As New Dictionary(Of String, MorphAnimation)

    Public ObjLoader_ma_ref As New ObjFileManager
    Public ObjLoader_ma_apply As ObjFileManager = Nothing

    Public ModelSkin_Vtx As Dictionary(Of Integer, Dictionary(Of Integer, Single)) = Nothing
    Public ModelSkin_Bone As Dictionary(Of Integer, Dictionary(Of Integer, Single)) = Nothing
    Public ModelSMD As SMDLoader = Nothing

    Public ObjLoader_sa_middle As ObjFileManager = Nothing
    Public ObjLoader_sa_apply As ObjFileManager = Nothing

    Public Sub SetBoneRotate(boneIdx As Integer, yawPitchRoll As Vector3)
        Dim rotMat As Matrix4x4 = Matrix4x4.CreateFromYawPitchRoll(yawPitchRoll.X, yawPitchRoll.Y, yawPitchRoll.Z)
        ' 更新直接绑定的顶点
        Dim lastBonePosRepo As Dictionary(Of Integer, Vector3) = ModelSMD.AppliedBonePosition
        Dim lastBonePos As Vector3 = lastBonePosRepo(boneIdx)
        Dim binding As Dictionary(Of Integer, Single) = ModelSkin_Bone(boneIdx)
        For Each bind_kvp As KeyValuePair(Of Integer, Single) In binding
            Dim vtxIdx As Integer = bind_kvp.Key
            Dim weight As Single = bind_kvp.Value
            Dim lastVtxPos As Vector3 = ObjLoader_sa_apply.VtxRepo(vtxIdx)
            Dim offset As Vector3 = lastVtxPos - lastBonePos
            offset = Vector3.Transform(offset, rotMat)
            Dim fullVtxPos As Vector3 = lastBonePos + offset

            Dim contibute As Vector3 = weight * (fullVtxPos - lastVtxPos)
            ObjLoader_sa_middle.VtxRepo(vtxIdx) += contibute
        Next
        ' 更新子骨骼
        Dim childBones As List(Of Integer) = ModelSMD.GetChildBone(boneIdx)
        ' 子骨骼节点移动
        'TODO
        ' 子骨骼绑定顶点移动
        'TODO

    End Sub

    Public Sub ApplySkinMiddle()
        ObjLoader_sa_apply = ObjLoader_sa_middle
        ObjLoader_sa_middle = ObjLoader_sa_middle.Clone
    End Sub

    Public Sub LinkSMDSkinToObj()
        ModelSkin_Vtx = New Dictionary(Of Integer, Dictionary(Of Integer, Single))
        Dim prog As Integer = 0
        For Each tri As SMD_ReferenceTriangle In ModelSMD.Triangles.Values
            For i = 0 To 2
                Dim vtx As SMD_BonePosNormUVLink = tri.Vertices(i)
                Dim vtxPos As Vector3 = vtx.Position
                Dim objVtxIdx As Integer = FindNearestVertex(vtxPos, ModelSMD.Nodes(vtx.ParentBoneIndex).BoneName)
                ModelSkin_Vtx(objVtxIdx) = vtx.Links
            Next
            prog += 1
            If prog Mod 2500 = 0 Then
                Form1.PostMsg("已处理: " & (prog * 100.0 / ModelSMD.TriangleCount).ToString("00.00") & "%")
                Application.DoEvents()
            End If
        Next
        CacheVerticesSkinToBones()
        Form1.PostMsg("已完成: " & ModelSMD.TriangleCount)
    End Sub

    Public Sub CacheVerticesSkinToBones()
        ModelSkin_Bone = New Dictionary(Of Integer, Dictionary(Of Integer, Single))
        For Each vtx_kvp As KeyValuePair(Of Integer, Dictionary(Of Integer, Single)) In ModelSkin_Vtx
            Dim vtxIdx As Integer = vtx_kvp.Key
            For Each skin_kvp As KeyValuePair(Of Integer, Single) In vtx_kvp.Value
                Dim boneIdx As Integer = skin_kvp.Key
                Dim weight As Single = skin_kvp.Value
                If Not ModelSkin_Bone.ContainsKey(boneIdx) Then
                    ModelSkin_Bone(boneIdx) = New Dictionary(Of Integer, Single)
                End If
                ModelSkin_Bone(boneIdx)(vtxIdx) = weight
            Next
        Next
    End Sub

    Public Sub SaveLinkInfo(path As String)
        Dim file As New FileStream(path, FileMode.Create)
        Using sw As New StreamWriter(file)
            sw.WriteLine("vtx_link")
            For Each vtx_kvp As KeyValuePair(Of Integer, Dictionary(Of Integer, Single)) In ModelSkin_Vtx
                Dim vtxIdx As Integer = vtx_kvp.Key
                Dim skin_count As Integer = vtx_kvp.Value.Count
                Dim str As String = vtxIdx.ToString & " " & skin_count & " "
                For Each skin_kvp As KeyValuePair(Of Integer, Single) In vtx_kvp.Value
                    Dim boneIdx As Integer = skin_kvp.Key
                    Dim weight As Single = skin_kvp.Value
                    str &= (boneIdx & " " & weight & " ")
                Next
                sw.WriteLine(str.TrimEnd)
            Next
            sw.WriteLine("end")
            sw.WriteLine("bone_link")
            For Each bone_kvp As KeyValuePair(Of Integer, Dictionary(Of Integer, Single)) In ModelSkin_Bone
                Dim boneIdx As Integer = bone_kvp.Key
                Dim skin_count As Integer = bone_kvp.Value.Count
                Dim str As String = boneIdx.ToString & " " & skin_count & " "
                For Each skin_kvp As KeyValuePair(Of Integer, Single) In bone_kvp.Value
                    Dim vtxIdx As Integer = skin_kvp.Key
                    Dim weight As Single = skin_kvp.Value
                    str &= (vtxIdx & " " & weight & " ")
                Next
                sw.WriteLine(str.TrimEnd)
            Next
            sw.WriteLine("end")
        End Using
        file.Close()
        file.Dispose()
    End Sub

    Public Sub LoadLinkInfo(path As String)
        ModelSkin_Vtx = New Dictionary(Of Integer, Dictionary(Of Integer, Single))
        ModelSkin_Bone = New Dictionary(Of Integer, Dictionary(Of Integer, Single))
        Dim mode As Integer = 0    ' 0-none 1-vtx 2-bone

        Dim file As New FileStream(path, FileMode.Open)
        Using sr As New StreamReader(file)
            While Not sr.EndOfStream
                Dim line As String = sr.ReadLine.Trim
                If line.Length = 0 Then Continue While
                Dim segs As String() = line.Split
                Dim head As String = segs(0)
                If mode = 0 Then
                    If head = "vtx_link" Then
                        mode = 1
                    ElseIf head = "bone_link" Then
                        mode = 2
                    End If
                ElseIf mode = 1 Then
                    If head = "end" Then
                        mode = 0
                    Else
                        Dim vtxIdx As Integer = CInt(head)
                        ModelSkin_Vtx(vtxIdx) = New Dictionary(Of Integer, Single)
                        Dim skinCount As Integer = CInt(segs(1))
                        If skinCount > 0 Then
                            For i = 0 To skinCount - 1
                                Dim boneIdx As Integer = CInt(segs(2 + 2 * i))
                                Dim weight As Single = CSng(segs(3 + 2 * i))
                                ModelSkin_Vtx(vtxIdx)(boneIdx) = weight
                            Next
                        End If
                    End If
                ElseIf mode = 2 Then
                    If head = "end" Then
                        mode = 0
                    Else
                        Dim boneIdx As Integer = CInt(head)
                        ModelSkin_Bone(boneIdx) = New Dictionary(Of Integer, Single)
                        Dim skinCount As Integer = CInt(segs(1))
                        If skinCount > 0 Then
                            For i = 0 To skinCount - 1
                                Dim vtxIdx As Integer = CInt(segs(2 + 2 * i))
                                Dim weight As Single = CSng(segs(3 + 2 * i))
                                ModelSkin_Vtx(boneIdx)(vtxIdx) = weight
                            Next
                        End If
                    End If
                End If
            End While
        End Using
        file.Close()
        file.Dispose()
    End Sub

    Public Sub GenerateDisparityMA()
        Dim anim As New MorphAnimation
        anim.AnimationName = "Animation1"

        Dim oldRepo As ObjFileManager = ObjLoader
        ObjLoader = New ObjFileManager
        Dim ofd As New OpenFileDialog With {
                        .Filter = "Obj File|*.obj",
                        .RestoreDirectory = True}
        If (ofd.ShowDialog = DialogResult.OK) Then
            Dim secondModel As New List(Of Model)
            secondModel.AddRange(ObjLoader.ReadObject(ofd.FileName, 1.0F))

            For k = 0 To ModelRepository.Count - 1
                Dim targetObj_old As Model = ModelRepository(k)
                'Dim objFilter As String() = {"headPoly_head_f_", "head_f_eyeR_eyelashR_", "head_f_eyeR_eyelidR_", "head_f_eyeL_eyelashL_", "head_f_eyeL_eyelidL_"}
                Dim objFilter As String() = {"head_f_IrisR_eyeR_", "head_f_IrisL_eyeL_"}
                If Not objFilter.Contains(targetObj_old.Name) Then Continue For

                Dim targetObj_new As Model = secondModel(k)
                For Each i In targetObj_old.Mesh.Keys
                    Dim poly_old As ModelPoly = targetObj_old.Mesh(i)
                    Dim poly_new As ModelPoly = targetObj_new.Mesh(i)
                    For j = 0 To 2
                        Dim vtx_old As Vector3 = oldRepo.VtxRepo(poly_old.VtxIdx(j))
                        Dim vtx_new As Vector3 = ObjLoader.VtxRepo(poly_new.VtxIdx(j))
                        Dim vtx_offset As Vector3 = vtx_new - vtx_old

                        If vtx_offset.Length > 0 Then
                            Dim kf0 As New KeyFrameVertex
                            With kf0
                                .Frame = 0
                                .Index = poly_old.VtxIdx(j)
                                .IndexType = 0
                                .Position = Vector3.Zero
                                .Tag = targetObj_old.Name
                            End With
                            anim.AddKeyFrame(kf0)

                            Dim kf1 As New KeyFrameVertex
                            With kf1
                                .Frame = 100
                                .Index = poly_old.VtxIdx(j)
                                .IndexType = 0
                                .Position = vtx_offset
                                .Tag = targetObj_old.Name
                            End With
                            anim.AddKeyFrame(kf1)
                        End If

                        Dim nor_old As Vector3 = oldRepo.NormalRepo(poly_old.NormalIdx(j))
                        Dim nor_new As Vector3 = ObjLoader.NormalRepo(poly_new.NormalIdx(j))
                        'Dim nor_old_q As Quaternion = Quaternion.CreateFromYawPitchRoll()    '正确的插值不应该使用Lerp（线性插值）,而应该使用Slerp（球面插值），球面插值需要四元数
                        Dim nor_offset As Vector3 = nor_new - nor_old

                        If nor_offset.Length > 0 Then
                            Dim kf0 As New KeyFrameVertex
                            With kf0
                                .Frame = 0
                                .Index = poly_old.NormalIdx(j)
                                .IndexType = 1
                                .Normal = Vector3.Zero
                                .Tag = targetObj_old.Name
                            End With
                            anim.AddKeyFrame(kf0)

                            Dim kf1 As New KeyFrameVertex
                            With kf1
                                .Frame = 100
                                .Index = poly_old.NormalIdx(j)
                                .IndexType = 1
                                .Normal = nor_offset
                                .Tag = targetObj_old.Name
                            End With
                            anim.AddKeyFrame(kf1)
                        End If

                        Dim tex_old As Vector2 = oldRepo.TexCoordRepo(poly_old.TexCoordIdx(j))
                        Dim tex_new As Vector2 = ObjLoader.TexCoordRepo(poly_new.TexCoordIdx(j))
                        Dim tex_offset As Vector2 = tex_new - tex_old

                        If tex_offset.Length > 0 Then
                            Dim kf0 As New KeyFrameVertex
                            With kf0
                                .Frame = 0
                                .Index = poly_old.TexCoordIdx(j)
                                .IndexType = 2
                                .TexCoord = Vector2.Zero
                                .Tag = targetObj_old.Name
                            End With
                            anim.AddKeyFrame(kf0)

                            Dim kf1 As New KeyFrameVertex
                            With kf1
                                .Frame = 100
                                .Index = poly_old.TexCoordIdx(j)
                                .IndexType = 2
                                .TexCoord = tex_offset
                                .Tag = targetObj_old.Name
                            End With
                            anim.AddKeyFrame(kf1)
                        End If
                    Next
                Next

            Next
            anim.SaveAnimation("C:\Users\asdfg\Desktop\rtTest\testMA.xml")
        End If

    End Sub

    Public Sub GenerateDisparityMA_UV_Scale()
        Dim anim As New MorphAnimation
        anim.AnimationName = "Animation1"

        Dim oldRepo As ObjFileManager = ObjLoader
        ObjLoader = New ObjFileManager
        Dim ofd As New OpenFileDialog With {
                        .Filter = "Obj File|*.obj",
                        .RestoreDirectory = True}
        If (ofd.ShowDialog = DialogResult.OK) Then
            Dim secondModel As New List(Of Model)
            secondModel.AddRange(ObjLoader.ReadObject(ofd.FileName, 1.0F))

            For k = 0 To ModelRepository.Count - 1
                Dim targetObj_old As Model = ModelRepository(k)
                If Not targetObj_old.Name.Contains("Iris") Then Continue For

                Dim targetObj_new As Model = secondModel(k)

                Dim p1 As Vector2 = oldRepo.TexCoordRepo(targetObj_old.Mesh.Values(0).TexCoordIdx(0))
                Dim p1d As Vector2 = ObjLoader.TexCoordRepo(targetObj_new.Mesh.Values(0).TexCoordIdx(0))
                Dim p2 As Vector2 = oldRepo.TexCoordRepo(targetObj_old.Mesh.Values(4).TexCoordIdx(0))
                Dim p2d As Vector2 = ObjLoader.TexCoordRepo(targetObj_new.Mesh.Values(4).TexCoordIdx(0))

                Dim p1_p1d As Vector2 = p1d - p1
                Dim p2_p2d As Vector2 = p2d - p2

                Dim origin As Vector2 = (p2_p2d * p1 - p1_p1d * p2) / (p2_p2d - p1_p1d)

                For Each i In targetObj_old.Mesh.Keys
                    Dim poly_old As ModelPoly = targetObj_old.Mesh(i)
                    Dim poly_new As ModelPoly = targetObj_new.Mesh(i)
                    For j = 0 To 2
                        Dim tex_old As Vector2 = oldRepo.TexCoordRepo(poly_old.TexCoordIdx(j))
                        Dim tex_new As Vector2 = ObjLoader.TexCoordRepo(poly_new.TexCoordIdx(j))
                        Dim tex_offset As Vector2 = tex_new - tex_old
                        If tex_offset.Length > 0 Then
                            Dim kf0 As New KeyFrameVertex
                            With kf0
                                .Frame = 0
                                .Index = poly_old.TexCoordIdx(j)
                                .IndexType = 3
                                .TexCoordOrigin = origin
                                .TexCoord = tex_old - origin
                                .Tag = targetObj_old.Name
                            End With
                            anim.AddKeyFrame(kf0)

                            Dim kf1 As New KeyFrameVertex
                            With kf1
                                .Frame = 100
                                .Index = poly_old.TexCoordIdx(j)
                                .IndexType = 3
                                .TexCoordOrigin = origin
                                .TexCoord = tex_new - origin
                                .Tag = targetObj_old.Name
                            End With
                            anim.AddKeyFrame(kf1)
                        End If
                    Next
                Next
            Next
            anim.SaveAnimation("C:\Users\asdfg\Desktop\rtTest\testMA.xml")
        End If
    End Sub

End Module

''' <summary>
''' (0,0), anchor1, anchor2, (1,1)
''' </summary>
Public Class LinearTween

    Public Anchor1 As Vector2 = New Vector2(0.1)
    Public Anchor2 As Vector2 = New Vector2(0.9)

    Public Function GetValue(x As Single) As Single
        Return x
    End Function

End Class

Public MustInherit Class KeyFrame
    ''' <summary>
    ''' 时间轴
    ''' </summary>
    Public Frame As Integer

    Public NextKeyFrame As KeyFrame = Nothing
    Public LastKeyFrame As KeyFrame = Nothing

    Public Tween As New LinearTween

    Public Tag As String = ""

    Public MustOverride Sub SaveKeyFrame(xDoc As XmlDocument, root As XmlElement)
    Public MustOverride Sub LoadKeyFrame(node As XmlNode)

End Class

Public Class KeyFrameVertex
    Inherits KeyFrame

    Public Index As Integer
    Public IndexType As Integer     ' 0-vtx 1-norm 2-tex 3-tex_scale

    Public Position As Vector3
    Public Normal As Vector3
    Public TexCoord As Vector2
    Public TexCoordOrigin As Vector2

    Public Overrides Sub SaveKeyFrame(xDoc As XmlDocument, root As XmlElement)

        Dim kfNode As XmlElement = xDoc.CreateElement("KeyFrame")
        With kfNode
            .SetAttribute("Target", Index)
            .SetAttribute("TargetType", IndexType)
            .SetAttribute("Frame", Frame)
            'TODO: tween
            If IndexType = 0 Then
                .SetAttribute("Value", Position.X & "," & Position.Y & "," & Position.Z)
            ElseIf IndexType = 1 Then
                .SetAttribute("Value", Normal.X & "," & Normal.Y & "," & Normal.Z)
            ElseIf Indextype = 2 Then
                .SetAttribute("Value", TexCoord.X & "," & TexCoord.Y)
            ElseIf Indextype = 3 Then
                .SetAttribute("Value", TexCoord.X & "," & TexCoord.Y)
                .SetAttribute("Origin", TexCoordOrigin.X & "," & TexCoordOrigin.Y)
            End If

            .SetAttribute("Tag", Tag)
        End With
        root.AppendChild(kfNode)

        If NextKeyFrame IsNot Nothing Then
            NextKeyFrame.SaveKeyFrame(xDoc, root)
        End If

    End Sub

    Public Overrides Sub LoadKeyFrame(node As XmlNode)
        Me.Index = CInt(node.Attributes("Target").Value)
        Me.IndexType = CInt(node.Attributes("TargetType").Value)
        Me.Frame = CInt(node.Attributes("Frame").Value)
        Dim segs As String() = node.Attributes("Value").Value.Split(",")
        If IndexType = 0 Then
            Me.Position = New Vector3(CSng(segs(0)), CSng(segs(1)), CSng(segs(2)))
        ElseIf IndexType = 1 Then
            Me.Normal = New Vector3(CSng(segs(0)), CSng(segs(1)), CSng(segs(2)))
        ElseIf IndexType = 2 Then
            Me.TexCoord = New Vector2(CSng(segs(0)), CSng(segs(1)))
        ElseIf IndexType = 3 Then
            Me.TexCoord = New Vector2(CSng(segs(0)), CSng(segs(1)))
            segs = node.Attributes("Origin").Value.Split(",")
            Me.TexCoordOrigin = New Vector2(CSng(segs(0)), CSng(segs(1)))
        End If
        Me.Tag = node.Attributes("Tag").Value

    End Sub
End Class


Public Class MorphAnimation

    Public AnimationName As String = ""

    Public KeyFrames As New List(Of KeyFrameVertex)

    Public CurrentFrame As Integer = 0

    Public ParentBone As Integer = -1


    Public Function Left(Optional axis As Single = 0.0F) As MorphAnimation
        Dim result As New MorphAnimation
        result.AnimationName = Me.AnimationName & "_Left"
        Dim candi_norm As New List(Of Integer)
        Dim candi_tex As New List(Of Integer)
        For Each head As KeyFrameVertex In Me.KeyFrames
            If head.IndexType = 0 Then
                If ObjLoader_ma_ref.VtxRepo(head.Index).X < axis Then
                    result.AddKeyFrame(head)

                    Dim link_norm As New List(Of Integer)
                    Dim link_tex As New List(Of Integer)
                    FindVertexInfo(head.Index, ModelRepository, link_norm, link_tex)
                    For Each norm As Integer In link_norm
                        If Not candi_norm.Contains(norm) Then candi_norm.Add(norm)
                    Next
                    For Each tex As Integer In link_tex
                        If Not candi_tex.Contains(tex) Then candi_tex.Add(tex)
                    Next
                End If
            End If
        Next
        For Each head As KeyFrameVertex In Me.KeyFrames
            If head.IndexType = 1 Then
                If candi_norm.Contains(head.Index) Then
                    result.AddKeyFrame(head)
                End If
            ElseIf head.IndexType = 2 Then
                If candi_tex.Contains(head.Index) Then
                    result.AddKeyFrame(head)
                End If
            End If
        Next
        Return result
    End Function
    Public Function Left2() As MorphAnimation
        Dim result As New MorphAnimation
        result.AnimationName = Me.AnimationName & "_Left"
        For Each head As KeyFrameVertex In Me.KeyFrames
            If head.Tag.EndsWith("eyeR_") Then
                result.AddKeyFrame(head)
            End If
        Next
        Return result
    End Function

    Public Function Right(Optional axis As Single = 0.0F) As MorphAnimation
        Dim result As New MorphAnimation
        result.AnimationName = Me.AnimationName & "_Right"
        Dim candi_norm As New List(Of Integer)
        Dim candi_tex As New List(Of Integer)
        For Each head As KeyFrameVertex In Me.KeyFrames
            If head.IndexType = 0 Then
                If ObjLoader_ma_ref.VtxRepo(head.Index).X > axis Then
                    result.AddKeyFrame(head)

                    Dim link_norm As New List(Of Integer)
                    Dim link_tex As New List(Of Integer)
                    FindVertexInfo(head.Index, ModelRepository, link_norm, link_tex)
                    For Each norm As Integer In link_norm
                        If Not candi_norm.Contains(norm) Then candi_norm.Add(norm)
                    Next
                    For Each tex As Integer In link_tex
                        If Not candi_tex.Contains(tex) Then candi_tex.Add(tex)
                    Next
                End If
            End If
        Next
        For Each head As KeyFrameVertex In Me.KeyFrames
            If head.IndexType = 1 Then
                If candi_norm.Contains(head.Index) Then
                    result.AddKeyFrame(head)
                End If
            ElseIf head.IndexType = 2 Then
                If candi_tex.Contains(head.Index) Then
                    result.AddKeyFrame(head)
                End If
            End If
        Next
        Return result
    End Function
    Public Function Right2() As MorphAnimation
        Dim result As New MorphAnimation
        result.AnimationName = Me.AnimationName & "_Right"
        For Each head As KeyFrameVertex In Me.KeyFrames
            If head.Tag.EndsWith("eyeL_") Then
                result.AddKeyFrame(head)
            End If
        Next
        Return result
    End Function

    Public Sub ApplyAnimation()
        For Each head As KeyFrameVertex In Me.KeyFrames
            Dim index As Integer = head.Index
            Dim idxType As Integer = head.IndexType
            Dim nextKf As KeyFrameVertex = head
            Dim tail As KeyFrameVertex = Nothing
            ' find interpolation position
            While (True)
                If nextKf.Frame > CurrentFrame Then
                    Exit While
                End If
                If nextKf.NextKeyFrame IsNot Nothing Then
                    nextKf = nextKf.NextKeyFrame
                Else
                    tail = nextKf
                    nextKf = Nothing
                    Exit While
                End If
            End While
            If nextKf Is Nothing Then
                If idxType = 0 Then
                    Dim pos_offset As Vector3 = tail.Position
                    ObjLoader_ma_apply.VtxRepo(index) += pos_offset
                ElseIf idxType = 1 Then
                    Dim norm_offset As Vector3 = tail.Normal
                    ObjLoader_ma_apply.NormalRepo(index) += norm_offset
                ElseIf idxType = 2 Then
                    Dim tex_offset As Vector2 = tail.TexCoord
                    ObjLoader_ma_apply.TexCoordRepo(index) += tex_offset
                ElseIf idxType = 3 Then
                    Dim tail_zoom As Vector2 = tail.TexCoord / head.TexCoord
                    Dim tex_move As Vector2 = ObjLoader_ma_apply.TexCoordRepo(index) - tail.TexCoordOrigin
                    ObjLoader_ma_apply.TexCoordRepo(index) = tail.TexCoordOrigin + tail_zoom * tex_move
                End If
            Else
                Dim lastKf As KeyFrameVertex = nextKf.LastKeyFrame
                If lastKf Is Nothing Then
                    If idxType = 0 Then
                        Dim pos_offset As Vector3 = nextKf.Position
                        ObjLoader_ma_apply.VtxRepo(index) += pos_offset
                    ElseIf idxType = 1 Then
                        Dim norm_offset As Vector3 = nextKf.Normal
                        ObjLoader_ma_apply.NormalRepo(index) += norm_offset
                    ElseIf idxType = 2 Then
                        Dim tex_offset As Vector2 = nextKf.TexCoord
                        ObjLoader_ma_apply.TexCoordRepo(index) += tex_offset
                    ElseIf idxType = 3 Then
                        Dim next_zoom As Vector2 = nextKf.TexCoord / head.TexCoord
                        Dim tex_move As Vector2 = ObjLoader_ma_apply.TexCoordRepo(index) - nextKf.TexCoordOrigin
                        ObjLoader_ma_apply.TexCoordRepo(index) = nextKf.TexCoordOrigin + next_zoom * tex_move
                    End If
                Else
                    Dim progress As Single = (CurrentFrame - lastKf.Frame) * 1.0 / (nextKf.Frame - lastKf.Frame)
                    If idxType = 0 Then
                        Dim pos_offset As Vector3 = lastKf.Position * (1.0 - progress) + nextKf.Position * progress
                        ObjLoader_ma_apply.VtxRepo(index) += pos_offset
                    ElseIf idxType = 1 Then
                        Dim norm_offset As Vector3 = lastKf.Normal * (1.0 - progress) + nextKf.Normal * progress
                        ObjLoader_ma_apply.NormalRepo(index) += norm_offset
                    ElseIf idxType = 2 Then
                        Dim tex_offset As Vector2 = lastKf.TexCoord * (1.0 - progress) + nextKf.TexCoord * progress
                        ObjLoader_ma_apply.TexCoordRepo(index) += tex_offset
                    ElseIf idxType = 3 Then
                        Dim tex_offset As Vector2 = lastKf.TexCoord * (1.0 - progress) + nextKf.TexCoord * progress
                        Dim tex_zoom As Vector2 = tex_offset / head.TexCoord
                        Dim tex_move As Vector2 = ObjLoader_ma_apply.TexCoordRepo(index) - head.TexCoordOrigin
                        ObjLoader_ma_apply.TexCoordRepo(index) = head.TexCoordOrigin + tex_zoom * tex_move
                    End If
                End If
            End If

            ' fix the normal length
            If idxType = 1 Then
                ObjLoader_ma_apply.NormalRepo(index) = Vector3.Normalize(ObjLoader_ma_apply.NormalRepo(index))
            End If
        Next
    End Sub

    Public Sub AddKeyFrame(kf As KeyFrameVertex)
        '判断是否已经存在行
        Dim row As KeyFrameVertex = Nothing
        For Each tmpKf As KeyFrameVertex In Me.KeyFrames
            If tmpKf.Index = kf.Index AndAlso tmpKf.IndexType = kf.IndexType Then
                row = tmpKf
                Exit For
            End If
        Next
        If row Is Nothing Then
            '新建行
            KeyFrames.Add(kf)
        Else
            '插入相应位置
            Dim kfLeft As KeyFrameVertex = Nothing
            Dim kfRight As KeyFrameVertex = Nothing
            Dim cursor As KeyFrameVertex = row
            While (True)
                If cursor.Frame > kf.Frame Then
                    kfRight = cursor
                    kfLeft = kfRight.LastKeyFrame
                    Exit While
                End If
                If cursor.NextKeyFrame IsNot Nothing Then
                    cursor = cursor.NextKeyFrame
                Else
                    Exit While
                End If
            End While
            If kfRight Is Nothing Then
                kfLeft = cursor
            End If
            If kfLeft Is Nothing Then
                '首位
                Me.KeyFrames.Remove(kfRight)
                kf.NextKeyFrame = kfRight
                Me.KeyFrames.Insert(0, kf)
            ElseIf kfLeft.Frame = kf.Frame Then
                '覆盖
                kfLeft.Position = kf.Position
                kfLeft.Normal = kf.Normal
                kfLeft.TexCoord = kf.TexCoord
            Else
                '其他
                kf.LastKeyFrame = kfLeft
                kf.NextKeyFrame = kfRight
                kfLeft.NextKeyFrame = kf
            End If


        End If
    End Sub

    Public Sub SaveAnimation(path As String)
        Dim xmlPath As String = path    'Application.StartupPath + "\Projects\test.xml"
        Dim xDoc As XmlDocument = New XmlDocument()
        Dim xNode As XmlNode = xDoc.CreateXmlDeclaration("1.0", "UTF-8", Nothing)
        xDoc.AppendChild(xNode)

        Dim root As XmlElement = xDoc.CreateElement("Project104R_MA")
        root.SetAttribute("Name", AnimationName)
        xDoc.AppendChild(root)

        For Each kf As KeyFrameVertex In KeyFrames
            kf.SaveKeyFrame(xDoc, root)
        Next

        xDoc.Save(xmlPath)
    End Sub

    Public Sub LoadAnimation(path As String)
        Dim xml As XmlDocument = New XmlDocument()
        xml.Load(path)
        Dim projectNode As XmlElement = xml.DocumentElement
        Me.AnimationName = projectNode.Attributes("Name").Value

        For Each kfNode As XmlNode In projectNode.ChildNodes
            Dim kf As New KeyFrameVertex
            kf.LoadKeyFrame(kfNode)
            Me.AddKeyFrame(kf)
        Next

    End Sub

End Class
