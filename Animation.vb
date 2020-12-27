Imports System.IO
Imports System.Numerics
Imports System.Xml

Module Animation

    Public MorphAnimationRepository As New Dictionary(Of String, MorphAnimation)


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

    Public MustOverride Sub SaveKeyFrame(xDoc As XmlDocument, root As XmlElement)
    Public MustOverride Sub LoadKeyFrame(node As XmlNode)

End Class

Public Class KeyFrameVertex
    Inherits KeyFrame

    Public VtxIdx As Integer

    Public Position As Vector3
    Public Normal As Vector3
    Public TexCoord As Vector2

    Public Overrides Sub SaveKeyFrame(xDoc As XmlDocument, root As XmlElement)

        Dim kfNode As XmlElement = xDoc.CreateElement("KeyFrame")
        With kfNode
            .SetAttribute("Target", VtxIdx)
            .SetAttribute("Frame", Frame)
            'TODO: tween
            .SetAttribute("Position", Position.X & "," & Position.Y & "," & Position.Z)
            .SetAttribute("Normal", Normal.X & "," & Normal.Y & "," & Normal.Z)
            .SetAttribute("TexCoord", TexCoord.X & "," & TexCoord.Y)
        End With
        root.AppendChild(kfNode)

        If NextKeyFrame IsNot Nothing Then
            NextKeyFrame.SaveKeyFrame(xDoc, root)
        End If

    End Sub

    Public Overrides Sub LoadKeyFrame(node As XmlNode)
        Me.VtxIdx = CInt(node.Attributes("Target").Value)
        Me.Frame = CInt(node.Attributes("Frame").Value)
        Dim segs As String()
        segs = node.Attributes("Position").Value.Split(",")
        Me.Position = New Vector3(CSng(segs(0)), CSng(segs(1)), CSng(segs(2)))
        segs = node.Attributes("Normal").Value.Split(",")
        Me.Normal = New Vector3(CSng(segs(0)), CSng(segs(1)), CSng(segs(2)))
        segs = node.Attributes("TexCoord").Value.Split(",")
        Me.TexCoord = New Vector2(CSng(segs(0)), CSng(segs(1)))
    End Sub
End Class


Public Class MorphAnimation

    Public AnimationName As String = ""

    Public KeyFrames As New List(Of KeyFrameVertex)

    Public CurrentFrame As Integer = 0

    Public Function Left(Optional axis As Single = 0.0F) As MorphAnimation
        Dim result As New MorphAnimation
        result.AnimationName = Me.AnimationName & "_Left"
        For Each head As KeyFrameVertex In Me.KeyFrames
            If ObjLoader.VtxRepo(head.VtxIdx).X < 0 Then
                result.AddKeyFrame(head)
            End If
        Next
        Return result
    End Function

    Public Sub ApplyAnimation()
        For Each head As KeyFrameVertex In Me.KeyFrames
            Dim vtxIdx As Integer = head.VtxIdx
            Dim nextKf As KeyFrameVertex = head
            Dim tail As KeyFrameVertex = Nothing
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
                Dim pos_offset As Vector3 = tail.Position
                Dim norm_offset As Vector3 = tail.Normal
                Dim tex_offset As Vector2 = tail.TexCoord
                ObjLoader.VtxRepo(vtxIdx) += pos_offset
                ObjLoader.NormalRepo(vtxIdx) += norm_offset
                ObjLoader.TexCoordRepo(vtxIdx) += tex_offset
            Else
                Dim lastKf As KeyFrameVertex = nextKf.LastKeyFrame
                If lastKf Is Nothing Then
                    Dim pos_offset As Vector3 = nextKf.Position
                    Dim norm_offset As Vector3 = nextKf.Normal
                    Dim tex_offset As Vector2 = nextKf.TexCoord
                    ObjLoader.VtxRepo(vtxIdx) += pos_offset
                    ObjLoader.NormalRepo(vtxIdx) += norm_offset
                    ObjLoader.TexCoordRepo(vtxIdx) += tex_offset
                Else
                    Dim progress As Single = (CurrentFrame - lastKf.Frame) * 1.0 / (nextKf.Frame - lastKf.Frame)
                    Dim pos_offset As Vector3 = lastKf.Position * (1.0 - progress) + nextKf.Position * progress
                    Dim norm_offset As Vector3 = lastKf.Normal * (1.0 - progress) + nextKf.Normal * progress
                    Dim tex_offset As Vector2 = lastKf.TexCoord * (1.0 - progress) + nextKf.TexCoord * progress
                    ObjLoader.VtxRepo(vtxIdx) += pos_offset
                    ObjLoader.NormalRepo(vtxIdx) += norm_offset
                    ObjLoader.TexCoordRepo(vtxIdx) += tex_offset
                End If
            End If
            ObjLoader.NormalRepo(vtxIdx) = Vector3.Normalize(ObjLoader.NormalRepo(vtxIdx))
        Next
    End Sub

    Public Sub AddKeyFrame(kf As KeyFrameVertex)
        '判断是否已经存在行
        Dim row As KeyFrameVertex = Nothing
        For Each tmpKf As KeyFrameVertex In Me.KeyFrames
            If tmpKf.VtxIdx = kf.VtxIdx Then
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
