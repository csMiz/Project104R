Imports System.IO
Imports System.Numerics

Public Class SMDLoader

    Public SMD_Path As String = ""

    Public Nodes As New Dictionary(Of Integer, SMD_Node)

    Public Skeleton As New Dictionary(Of Integer, SMD_SkeletalFrame)
    Public Reference As SMD_SkeletalFrame
    Public ReferenceBones As New SMD_SkeletalFrame
    Public AppliedBonePos As New Dictionary(Of Integer, Vector3)

    Public Triangles As New Dictionary(Of Integer, SMD_ReferenceTriangle)
    Public TriangleCount As Integer = 0

    Public Function GetBonePosition(idx As Integer) As Vector3
        If idx = -1 Then
            Return Vector3.Zero
        End If
        Dim parent As Integer = Nodes(idx).ParentIdx
        Dim parentPos As Vector3 = GetBonePosition(parent)
        Dim offset As Vector3
        If parent = -1 Then
            offset = Reference.Content(idx).Position
        Else
            Dim cumuMat As Matrix4x4 = GetBoneRotation(parent)
            Matrix4x4.Invert(cumuMat, cumuMat)
            Dim matr As New Matrix4x4
            matr.M11 = Reference.Content(idx).Position.X
            matr.M21 = Reference.Content(idx).Position.Y
            matr.M31 = Reference.Content(idx).Position.Z
            matr = cumuMat * matr
            offset = New Vector3(matr.M11, matr.M21, matr.M31)
        End If
        Return parentPos + offset
    End Function

    Public Function GetBoneRotation(idx As Integer) As Matrix4x4
        If idx = -1 Then
            Return Matrix4x4.Identity
        End If
        Dim parent As Integer = Nodes(idx).ParentIdx
        Dim yaw As Matrix4x4 = Matrix4x4.CreateFromAxisAngle(New Vector3(0, 1, 0), Reference.Content(idx).Rotation.Y)
        Dim pitch As Matrix4x4 = Matrix4x4.CreateFromAxisAngle(New Vector3(1, 0, 0), Reference.Content(idx).Rotation.X)
        Dim roll As Matrix4x4 = Matrix4x4.CreateFromAxisAngle(New Vector3(0, 0, 1), Reference.Content(idx).Rotation.Z)
        Dim thisRot As Matrix4x4 = pitch * roll * yaw
        'Dim thisRot = Matrix4x4.CreateFromYawPitchRoll(Reference.Content(idx).Rotation.Y, Reference.Content(idx).Rotation.X, Reference.Content(idx).Rotation.Z)
        Return thisRot * GetBoneRotation(parent)
    End Function

    Public Function GetBonePositionW(idx As Integer) As Vector3
        If idx = -1 Then
            Return Vector3.Zero
        End If
        Return ReferenceBones.Content(idx).Position
    End Function
    Public Function GetBoneRotationW(idx As Integer) As Matrix4x4
        If idx = -1 Then
            Return Matrix4x4.Identity
        End If
        Return Matrix4x4.CreateFromYawPitchRoll(ReferenceBones.Content(idx).Rotation.Y, ReferenceBones.Content(idx).Rotation.X, ReferenceBones.Content(idx).Rotation.Z)
    End Function

    Public Function GetChildBone(parentIdx As Integer) As List(Of Integer)
        Dim res As New List(Of Integer)
        For Each node As KeyValuePair(Of Integer, SMD_Node) In Me.Nodes
            If node.Value.ParentIdx = parentIdx Then
                res.Add(node.Key)
            End If
        Next
        Return res
    End Function

    Public Sub LoadSMD(path As String)
        Dim readMode As Integer = 0    '0-none 1-nodes 2-skeleton 3-triangles
        Dim sk_currentFrame As SMD_SkeletalFrame = Nothing
        Dim tr_currentTriangle As SMD_ReferenceTriangle = Nothing

        SMD_Path = path
        Dim file As FileStream = New FileStream(path, FileMode.Open)
        Using sr As New StreamReader(file)
            While Not sr.EndOfStream
                Dim line As String = sr.ReadLine.Trim
                line = line.Replace("  ", " ")
                If line.Length = 0 Then Continue While
                Dim segs As String() = line.Split(" ")
                Dim head As String = segs(0)
                If readMode = 0 Then
                    If head = "version" Then
                        'pass
                    ElseIf head = "nodes" Then
                        readMode = 1
                    ElseIf head = "skeleton" Then
                        readMode = 2
                    ElseIf head = "triangles" Then
                        readMode = 3

                    End If
                ElseIf readMode = 1 Then
                    'nodes
                    If head = "end" Then
                        readMode = 0
                    Else
                        Dim tmpNode As New SMD_Node
                        With tmpNode
                            .Index = CInt(head)
                            .BoneName = segs(1).Replace("""", "")
                            .ParentIdx = CInt(segs(2))
                        End With
                        Me.Nodes(tmpNode.Index) = tmpNode
                    End If
                ElseIf readMode = 2 Then
                    'skeleton
                    If head = "end" Then
                        readMode = 0
                    ElseIf head = "time" Then
                        sk_currentFrame = New SMD_SkeletalFrame
                        Skeleton(CInt(segs(1))) = sk_currentFrame
                    Else
                        Dim tmpPR As New SMD_PosRot(New Vector3(CSng(segs(1)), CSng(segs(3)), -CSng(segs(2))), New Vector3(CSng(segs(4)), CSng(segs(6)), -CSng(segs(5))))
                        sk_currentFrame.Content(CInt(head)) = tmpPR
                    End If
                ElseIf readMode = 3 Then
                    'triangles
                    If head = "end" Then
                        readMode = 0
                    ElseIf segs.Count = 1 Then
                        tr_currentTriangle = New SMD_ReferenceTriangle
                        tr_currentTriangle.MaterialShortName = head
                        Triangles(TriangleCount) = tr_currentTriangle
                        TriangleCount += 1
                    Else
                        Dim tmpVtxData As New SMD_BonePosNormUVLink
                        With tmpVtxData
                            .ParentBoneIndex = CInt(head)
                            .Position = New Vector3(CSng(segs(1)), CSng(segs(3)), -CSng(segs(2)))
                            .Normal = New Vector3(CSng(segs(4)), CSng(segs(6)), -CSng(segs(5)))
                            .UV = New Vector2(CSng(segs(7)), CSng(segs(8)))
                        End With
                        Dim linkCount As Integer = CInt(segs(9))
                        If linkCount > 0 Then
                            For i = 0 To linkCount - 1
                                Dim boneIdx As Integer = CInt(segs(10 + i * 2))
                                Dim weight As Single = CSng(segs(11 + i * 2))
                                tmpVtxData.Links(boneIdx) = weight
                            Next
                        End If
                        tr_currentTriangle.WriteInVertex(tmpVtxData)
                    End If
                End If

            End While
        End Using
        file.Close()
        file.Dispose()

        Reference = Me.Skeleton(0)
        For Each boneIdx As Integer In Me.Nodes.Keys
            Dim absoluteBonePos As Vector3 = GetBonePosition(boneIdx)
            Me.AppliedBonePos(boneIdx) = New Vector3(absoluteBonePos.X, absoluteBonePos.Y, absoluteBonePos.Z)
        Next
    End Sub

    Public Sub LoadSMDW(path As String)
        Dim readMode As Integer = 0    '0-none 1-nodes 2-skeleton 3-triangles

        Dim file As FileStream = New FileStream(path, FileMode.Open)
        Using sr As New StreamReader(file)
            While Not sr.EndOfStream
                Dim line As String = sr.ReadLine.Trim
                line = line.Replace("  ", " ")
                If line.Length = 0 Then Continue While
                Dim segs As String() = line.Split(" ")
                Dim head As String = segs(0)
                If readMode = 0 Then
                    If head = "version" Then
                        'pass
                    ElseIf head = "nodes" Then
                        readMode = 1
                    ElseIf head = "skeleton" Then
                        readMode = 2
                    ElseIf head = "triangles" Then
                        readMode = 3

                    End If
                ElseIf readMode = 1 Then
                    'nodes
                ElseIf readMode = 2 Then
                    'skeleton
                    If head = "end" Then
                        readMode = 0
                    ElseIf head = "time" Then
                        'pass
                    Else
                        Dim tmpPR As New SMD_PosRot(New Vector3(CSng(segs(1)), CSng(segs(3)), -CSng(segs(2))), New Vector3(CSng(segs(4)), CSng(segs(6)), -CSng(segs(5))))
                        Me.ReferenceBones.Content(head) = tmpPR
                    End If
                ElseIf readMode = 3 Then
                    'triangles
                End If

            End While
        End Using
        file.Close()
        file.Dispose()

        For Each boneIdx As Integer In Me.Nodes.Keys
            Dim absoluteBone As SMD_PosRot = Me.ReferenceBones.Content(boneIdx)
            Me.AppliedBonePos(boneIdx) = New Vector3(absoluteBone.Position.X, absoluteBone.Position.Y, absoluteBone.Position.Z)
        Next
    End Sub

End Class

Public Class SMD_Node

    Public Index As Integer = 0

    Public BoneName As String = ""

    Public ParentIdx As Integer = -1

End Class

Public Class SMD_SkeletalFrame

    Public Content As New Dictionary(Of Integer, SMD_PosRot)

    Public Function Clone() As SMD_SkeletalFrame
        Dim res As New SMD_SkeletalFrame
        With res.Content
            For Each prkvp As KeyValuePair(Of Integer, SMD_PosRot) In Me.Content
                .Add(prkvp.Key, New SMD_PosRot(New Vector3(prkvp.Value.Position.X, prkvp.Value.Position.Y, prkvp.Value.Position.Z),
                    New Vector3(prkvp.Value.Rotation.X, prkvp.Value.Rotation.Y, prkvp.Value.Rotation.Z)))
            Next
        End With
        Return res
    End Function

End Class

Public Class SMD_ReferenceTriangle

    Public MaterialShortName As String = ""

    Public Vertices(2) As SMD_BonePosNormUVLink

    Private VtxWrittenCount As Integer = 0

    Public Sub WriteInVertex(dataIn As SMD_BonePosNormUVLink)
        If VtxWrittenCount >= 3 Then Return
        Me.Vertices(VtxWrittenCount) = dataIn
        VtxWrittenCount += 1
    End Sub

End Class


Public Structure SMD_PosRot

    Public Position As Vector3
    Public Rotation As Vector3    'euler

    Public Sub New(pos As Vector3, rot As Vector3)
        Position = pos
        Rotation = rot
    End Sub

    Public Function Clone() As SMD_PosRot
        Dim r As New SMD_PosRot(New Vector3(Position.X, Position.Y, Position.Z), New Vector3(Rotation.X, Rotation.Y, Rotation.Z))
        Return r
    End Function

End Structure

Public Class SMD_BonePosNormUVLink

    Public ParentBoneIndex As Integer
    Public Position As Vector3
    Public Normal As Vector3
    Public UV As Vector2
    Public Links As New Dictionary(Of Integer, Single)

End Class
