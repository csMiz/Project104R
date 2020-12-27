Imports System.IO
Imports System.Numerics

Module ObjManager

    Public Class ObjFileManager
        Public ObjFileName As String
        Public MtlFileName As String
        Private CurrentObj As Model
        Private CurrentMat As ModelMaterial
        Public VtxRepo As New Dictionary(Of Integer, Vector3)
        Public NormalRepo As New Dictionary(Of Integer, Vector3)
        Public TexCoordRepo As New Dictionary(Of Integer, Vector2)
        Public MatRepo As New Dictionary(Of String, ModelMaterial)
        Public TextureRepo As New Dictionary(Of String, ModelTexture)

        Public MinX As Single = 9999, MinY As Single = 9999, MinZ As Single = 9999
        Public MaxX As Single = -9999, MaxY As Single = -9999, MaxZ As Single = -9999

        Public Function ReadObject(path As String, scale As Single) As List(Of Model)
            ObjFileName = path
            VtxRepo.Clear()
            NormalRepo.Clear()
            TexCoordRepo.Clear()
            MatRepo.Clear()
            TextureRepo.Clear()
            MtlFileName = ""
            CurrentObj = Nothing
            CurrentMat = Nothing

            Dim objs As New List(Of Model)
            Dim file As New FileStream(path, FileMode.Open)
            Using sr As New StreamReader(file)
                While Not sr.EndOfStream
                    Dim str As String = sr.ReadLine.Replace("  ", " ")
                    If (str.Length() < 1) Then Continue While
                    Dim segs As String() = str.Split()
                    Dim arg As String = segs(0)
                    If (arg = "mtllib") Then        ' read materials
                        MtlFileName = segs(1)
                        Dim tmpMat As New ModelMaterial
                        Dim rootFolderPath As String = path.Substring(0, path.Length - path.Split("\").Last.Length)
                        Dim f2 As New FileStream(rootFolderPath & MtlFileName, FileMode.Open)
                        Using sr2 As New StreamReader(f2)
                            While Not sr2.EndOfStream
                                Dim str2 As String = sr2.ReadLine
                                str2 = str2.Trim
                                str2 = str2.Replace(vbTab, "")
                                If (str2.Length() < 1) Then Continue While
                                Dim segs2 As String() = str2.Split()
                                Dim arg2 As String = segs2(0)
                                If (arg2 = "newmtl") Then
                                    tmpMat = New ModelMaterial
                                    tmpMat.Name = segs2(1)
                                    MatRepo(segs2(1)) = tmpMat
                                ElseIf (arg2 = "Kd") Then
                                    Dim tmpRed As Single = CSng(segs2(1))
                                    Dim tmpGreen As Single = CSng(segs2(2))
                                    Dim tmpBlue As Single = CSng(segs2(3))
                                    Dim tmpC As New Vector3(tmpRed, tmpGreen, tmpBlue)
                                    tmpMat.Diffuse = tmpC
                                ElseIf (arg2 = "Ka") Then
                                    Dim tmpRed As Single = CSng(segs2(1))
                                    Dim tmpGreen As Single = CSng(segs2(2))
                                    Dim tmpBlue As Single = CSng(segs2(3))
                                    Dim tmpC As New Vector3(tmpRed, tmpGreen, tmpBlue)
                                    tmpMat.Ambient = tmpC
                                ElseIf (arg2 = "Ks") Then
                                    Dim tmpRed As Single = CSng(segs2(1))
                                    Dim tmpGreen As Single = CSng(segs2(2))
                                    Dim tmpBlue As Single = CSng(segs2(3))
                                    Dim tmpC As New Vector3(tmpRed, tmpGreen, tmpBlue)
                                    tmpMat.Specular = tmpC
                                ElseIf (arg2 = "Ns") Then
                                    Dim tmpValue As Single = CSng(segs2(1))
                                    tmpMat.SpecularExponent = tmpValue
                                ElseIf (arg2 = "map_Kd") Then
                                    Dim tmpTexPath As String = segs2(1)
                                    If Not tmpTexPath.StartsWith("C") Then
                                        tmpTexPath = rootFolderPath & tmpTexPath
                                    End If
                                    If Not TextureRepo.ContainsKey(tmpTexPath) Then
                                        Dim tmpTex As New ModelTexture
                                        ' read image file
                                        tmpTex.Name = tmpTexPath
                                        tmpTex.Path = tmpTexPath
                                        tmpTex.ReadImage()
                                        TextureRepo(tmpTexPath) = tmpTex
                                        tmpMat.DiffuseTexture = tmpTex
                                    Else
                                        tmpMat.DiffuseTexture = TextureRepo(tmpTexPath)
                                    End If
                                ElseIf (arg2 = "bump") Then
                                    Dim tmpTexPath As String = segs2(1)
                                    If Not TextureRepo.ContainsKey(tmpTexPath) Then
                                        Dim tmpTex As New ModelTexture
                                        ' read image file
                                        tmpTex.Name = tmpTexPath
                                        tmpTex.Path = tmpTexPath
                                        tmpTex.ReadImage()
                                        TextureRepo(tmpTexPath) = tmpTex
                                        tmpMat.Bump = tmpTex
                                    Else
                                        tmpMat.Bump = TextureRepo(tmpTexPath)
                                    End If
                                ElseIf (arg2 = "mz_emissionC") Then
                                    Dim tmpRed As Single = CSng(segs2(1))
                                    Dim tmpGreen As Single = CSng(segs2(2))
                                    Dim tmpBlue As Single = CSng(segs2(3))
                                    Dim tmpC As New Vector3(tmpRed, tmpGreen, tmpBlue)
                                    tmpMat.Emission = tmpC
                                ElseIf (arg2 = "mz_emissionV") Then
                                    Dim tmpValue As Single = CSng(segs2(1))
                                    tmpMat.EmissionStrength = tmpValue
                                ElseIf (arg2 = "mz_isTrans") Then
                                    tmpMat.MarkAsTransparent = True
                                Else
                                    'extension here
                                End If
                            End While
                        End Using
                        f2.Close()
                        f2.Dispose()
                    ElseIf (arg = "o") Then
                        CurrentObj = New Model()
                        objs.Add(CurrentObj)
                        CurrentObj.Name = segs(1)
                    ElseIf (arg = "g") Then
                        CurrentObj = New Model()
                        objs.Add(CurrentObj)
                        For i = 1 To segs.Count - 1
                            CurrentObj.Name = CurrentObj.Name & segs(i) & "_"
                        Next
                    ElseIf (arg = "usemtl") Then
                        Dim tmpMat As ModelMaterial = MatRepo(segs(1))
                        CurrentMat = tmpMat
                    ElseIf (arg = "v") Then
                        Dim tmpX As Single = CSng(segs(1))
                        Dim tmpY As Single = CSng(segs(2))
                        Dim tmpZ As Single = CSng(segs(3))
                        Dim tmpVec As New Vector3(tmpX * scale, tmpY * scale, tmpZ * scale)
                        VtxRepo(VtxRepo.Count) = tmpVec

                        If tmpVec.X < MinX Then MinX = tmpVec.X
                        If tmpVec.Y < MinY Then MinY = tmpVec.Y
                        If tmpVec.Z < MinZ Then MinZ = tmpVec.Z
                        If tmpVec.X > MaxX Then MaxX = tmpVec.X
                        If tmpVec.Y > MaxY Then MaxY = tmpVec.Y
                        If tmpVec.Z > MaxZ Then MaxZ = tmpVec.Z

                    ElseIf (arg = "vn") Then      ' normals
                        Dim tmpX As Single = CSng(segs(1))
                        Dim tmpY As Single = CSng(segs(2))
                        Dim tmpZ As Single = CSng(segs(3))
                        Dim tmpNor As New Vector3(tmpX, tmpY, tmpZ)
                        NormalRepo(NormalRepo.Count) = tmpNor
                    ElseIf (arg = "vt") Then      ' texCoords
                        Dim tmpU As Single = CSng(segs(1))
                        Dim tmpV As Single = CSng(segs(2))
                        Dim tmpTexCoord As New Vector2(tmpU, 1.0 - tmpV)
                        TexCoordRepo(TexCoordRepo.Count) = tmpTexCoord
                    ElseIf (arg = "f") Then
                        Dim f_args As String()() = {segs(1).Split("/"), segs(2).Split("/"), segs(3).Split("/")}
                        Dim pos_idx As Integer() = {CInt(f_args(0)(0)) - 1, CInt(f_args(1)(0)) - 1, CInt(f_args(2)(0)) - 1}
                        Dim nor_idx As Integer() = {CInt(f_args(0)(2)) - 1, CInt(f_args(1)(2)) - 1, CInt(f_args(2)(2)) - 1}

                        'clockwise correction
                        'Dim v1 As Vector3 = VtxRepo(pos_idx(1)) - VtxRepo(pos_idx(0))
                        'Dim v2 As Vector3 = VtxRepo(pos_idx(2)) - VtxRepo(pos_idx(0))

                        Dim tmpTri As ModelPoly = New ModelPoly()
                        With tmpTri
                            .VtxIdx = pos_idx
                            .NormalIdx = nor_idx
                        End With
                        Dim hasTC As String = f_args(0)(1)
                        If (hasTC.Trim.Length > 0) Then
                            Dim tc_idx As Integer() = {CInt(f_args(0)(1)) - 1, CInt(f_args(1)(1)) - 1, CInt(f_args(2)(1)) - 1}
                            tmpTri.TexCoordIdx = tc_idx
                        End If
                        If CurrentMat IsNot Nothing Then
                            tmpTri.MaterialName = CurrentMat.Name
                        End If
                        CurrentObj.Mesh(CurrentObj.Mesh.Count) = tmpTri
                    Else
                        ' extension here
                    End If
                End While
            End Using
            file.Close()
            file.Dispose()

            For i = objs.Count - 1 To 0 Step -1
                Dim tmpObj As Model = objs(i)
                If tmpObj.Mesh.Count = 0 Then
                    objs.RemoveAt(i)
                End If
            Next
            Return objs
        End Function

    End Class


End Module


Public Class Model

    Public Name As String = vbNullString

    Public AABB As Single() = {}

    'Public VtxRepo As New Dictionary(Of Integer, Vector3)

    'Public NormalRepo As New Dictionary(Of Integer, Vector3)

    'Public TextCoordRepo As New Dictionary(Of Integer, Vector2)

    Public Mesh As New Dictionary(Of Integer, ModelPoly)


End Class

Public Structure ModelPoly

    Public VtxIdx As Integer()
    Public NormalIdx As Integer()
    Public TexCoordIdx As Integer()
    Public MaterialName As String

End Structure

Public Structure ModelPolyV

    Public Vtx As Vector3()
    Public Normal As Vector3()
    Public TexCoord As Vector2()
    Public MaterialName As String

End Structure


Public Class ModelMaterial

    Public Name As String = vbNullString

    Public Ambient As New Vector3(0.1F)
    Public AmbientStrength As Single = 1.0F
    Public Diffuse As New Vector3(1.0F)
    Public DiffuseStrength As Single = 1.0F
    Public DiffuseTexture As ModelTexture = Nothing
    Public Specular As New Vector3(1.0F)
    Public SpecularStrength As Single = 1.0F
    Public SpecularExponent As Single = 10.0F

    Public IOR As Single = 1.0F
    Public Bump As ModelTexture = Nothing

    ' =============自定义================
    Public ReflectStrength As Single = 0.5F
    Public RefractStrength As Single = 0.0F
    Public Emission As New Vector3(1.0F)
    Public EmissionStrength As Single = 0.0F
    Public EmissionTexture As ModelTexture = Nothing

    Public MarkAsTransparent As Boolean = False

End Class

Public Class ModelTexture

    Public Name As String
    Public Path As String

    Private TextureBitmap As Bitmap = Nothing
    Private TextureSize As Vector2
    Private TransparentCheckFlag As Boolean = False
    Private TransparentCheckResult As Boolean = False

    Private TextureLock As New Object

    Private Shared PIXEL_OFFSET As Vector2 = New Vector2(0.5)

    <Obsolete("太慢了", False)>
    Public Function HasTransparent() As Boolean
        If Not TransparentCheckFlag Then
            TransparentCheckResult = False
            For j = 0 To TextureSize.Y - 1
                For i = 0 To TextureSize.X - 1
                    Dim color As Color = TextureBitmap.GetPixel(i, j)
                    If color.A < 255 Then
                        TransparentCheckResult = True
                        GoTo check_finish
                    End If
                Next
            Next
check_finish:
            TransparentCheckFlag = True
        End If
        Return TransparentCheckResult
    End Function

    Public Sub ReadImage()
        If TextureBitmap IsNot Nothing Then TextureBitmap.Dispose()
        TextureBitmap = New Bitmap(Path)
        TextureSize = New Vector2(TextureBitmap.Width, TextureBitmap.Height)
    End Sub

    Public Function GetRawPixel(pixelIdx As Vector2) As Vector4
        Dim x As Integer = CInt(pixelIdx.X)
        Dim y As Integer = CInt(pixelIdx.Y)
        While x < 0
            x += TextureSize.X
        End While
        While x >= TextureSize.X
            x -= TextureSize.X
        End While
        While y < 0
            y += TextureSize.Y
        End While
        While y >= TextureSize.Y
            y -= TextureSize.Y
        End While
        Dim pixel As Color
        SyncLock TextureLock
            pixel = TextureBitmap.GetPixel(x, y)
        End SyncLock
        Return New Vector4(pixel.R / 255.0F, pixel.G / 255.0F, pixel.B / 255.0F, pixel.A / 255.0F)
    End Function

    Public Function GetPixel(texCoord As Vector2) As Vector4
        Dim targetPos As Vector2 = texCoord * TextureSize - PIXEL_OFFSET
        ' top-left
        Dim pos_tl As Vector2 = New Vector2(Math.Floor(targetPos.X), Math.Floor(targetPos.Y))
        Dim color_tl As Vector4 = GetRawPixel(pos_tl)
        Dim dx_l As Single = targetPos.X - pos_tl.X
        ' top-right
        Dim pos_tr As Vector2 = New Vector2(Math.Ceiling(targetPos.X), Math.Floor(targetPos.Y))
        Dim color_tr As Vector4 = GetRawPixel(pos_tr)
        ' top
        Dim color_top As Vector4 = color_tl * (1.0F - dx_l) + color_tr * dx_l
        Dim dy_t As Single = targetPos.Y - pos_tl.Y

        ' bottom-left
        Dim pos_bl As Vector2 = New Vector2(Math.Floor(targetPos.X), Math.Ceiling(targetPos.Y))
        Dim color_bl As Vector4 = GetRawPixel(pos_bl)
        ' bottom-right
        Dim pos_br As Vector2 = New Vector2(Math.Ceiling(targetPos.X), Math.Ceiling(targetPos.Y))
        Dim color_br As Vector4 = GetRawPixel(pos_br)
        ' bottom
        Dim color_bottom As Vector4 = color_bl * (1.0F - dx_l) + color_br * dx_l

        Dim color_final As Vector4 = color_top * (1.0F - dy_t) + color_bottom * dy_t
        Return color_final
    End Function


End Class
