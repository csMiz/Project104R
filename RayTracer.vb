Imports System.Numerics

Module RayTracer

    Public Rand As New Random

    Public ObjLoader As New ObjFileManager
    'Public ObjLoader_MA As ObjFileManager

    Public ModelRepository As New List(Of Model)
    Public LightRepository As New List(Of RTLight)
    Public AABBRepository As ModelOctree = Nothing
    Public BVHRepository As BVH = Nothing
    Public ExtrudedBVHRepository As BVH = Nothing

    Public CameraRotation As New Vector3(0, 0, 0)
    Public CameraPosition As New Vector3(0, 30, 12)  '0,2,20

    Public F_ImageWidth As Single = 1.0F
    Public F_ImageHeight As Single = 0.75F
    Public ImageWidth As Integer = 200
    Public ImageHeight As Integer = 150

    Public ACME_THRESHOLD As Single = 0.01
    Public EDGE_WIDTH As Single = 0.02
    Public ReflectDepth As Integer = 2

    Public ImageBuffer As Bitmap = Nothing
    Public ImageBufferFinishCount As Integer = 0
    Public ImageBufferFinishCountLock As New Object
    Public PTBuffer As Vector3(,)
    Private CranelyPettersonRotation As New Dictionary(Of Integer, Single)
    Private Const PATH_SAMPLE_DIMENSION As Integer = 4




    Public Function Matrix3x3Inv(mat As Vector3()) As Vector3()
        Dim det As Single = mat(0).X * (mat(1).Y * mat(2).Z - mat(1).Z * mat(2).Y) - mat(1).X * (mat(0).Y * mat(2).Z - mat(0).Z * mat(2).Y) + mat(2).X * (mat(0).Y * mat(1).Z - mat(0).Z * mat(1).Y)
        If det = 0 Then Return {New Vector3(1, 0, 0), New Vector3(0, 1, 0), New Vector3(0, 0, 1)}
        Dim row1 As New Vector3(mat(1).Y * mat(2).Z - mat(1).Z * mat(2).Y, mat(0).Z * mat(2).Y - mat(0).Y * mat(2).Z, mat(0).Y * mat(1).Z - mat(0).Z * mat(1).Y)
        Dim row2 As New Vector3(mat(1).Z * mat(2).X - mat(1).X * mat(2).Z, mat(0).X * mat(2).Z - mat(0).Z * mat(2).X, mat(1).X * mat(0).Z - mat(0).X * mat(1).Z)
        Dim row3 As New Vector3(mat(1).X * mat(2).Y - mat(1).Y * mat(2).X, mat(0).Y * mat(2).X - mat(0).X * mat(2).Y, mat(0).X * mat(1).Y - mat(1).X * mat(0).Y)
        Return {row1 / det, row2 / det, row3 / det}
    End Function

    Public Function Matrix3x3Mul(mat As Vector3(), vec As Vector3) As Vector3
        'Return New Vector3(Vector3.Dot(mat(0), vec), Vector3.Dot(mat(1), vec), Vector3.Dot(mat(2), vec))
        Dim x As Single = mat(0).X * vec.X + mat(1).X * vec.Y + mat(2).X * vec.Z
        Dim y As Single = mat(0).Y * vec.X + mat(1).Y * vec.Y + mat(2).Y * vec.Z
        Dim z As Single = mat(0).Z * vec.X + mat(1).Z * vec.Y + mat(2).Z * vec.Z
        Return New Vector3(x, y, z)
    End Function

    Public Function CompareVec3(a As Vector3, b As Vector3) As Integer
        Dim result = 0
        If a.X >= b.X Then
            result += &B100
        End If
        If a.Y >= b.Y Then
            result += &B10
        End If
        If a.Z >= b.Z Then
            result += &B1
        End If
        Return result
    End Function

    Public Function ColorPlus(a As Integer(), b As Integer(), Optional chan As Integer = 3) As Integer()
        Dim r(chan - 1) As Integer
        For i = 0 To chan - 1
            r(i) = a(i) + b(i)
        Next
        Return r
    End Function

    Public Function GetRandomDirection() As Vector3
        Dim xi0 As Single = Rand.NextDouble
        Dim xi1 As Single = Rand.NextDouble
        Dim theta As Single = Math.Acos(2.0F * xi0 - 1.0F)
        Dim phi As Single = xi1 * 2.0F * Math.PI
        Dim res_x As Single = Math.Sin(theta) * Math.Cos(phi)
        Dim res_y As Single = Math.Sin(theta) * Math.Sin(phi)
        Dim res_z As Single = Math.Cos(theta)
        Return New Vector3(res_x, res_y, res_z)
    End Function

    Public Function GetRandomDirection(haltonIdx As Integer, haltonDimension As Integer) As Vector3
        If Not CranelyPettersonRotation.ContainsKey(haltonDimension) Then
            CranelyPettersonRotation(haltonDimension) = Rand.NextDouble
        End If
        Dim offset0 As Single = CranelyPettersonRotation(haltonDimension)

        If Not CranelyPettersonRotation.ContainsKey(haltonDimension + 1) Then
            CranelyPettersonRotation(haltonDimension + 1) = Rand.NextDouble
        End If
        Dim offset1 As Single = CranelyPettersonRotation(haltonDimension + 1)

        Dim xi0 As Single = GetHalton(haltonIdx, haltonDimension) + offset0
        Dim xi1 As Single = GetHalton(haltonIdx, haltonDimension + 1) + offset1
        If xi0 >= 1.0F Then xi0 -= 1.0F
        If xi1 >= 1.0F Then xi1 -= 1.0F

        Dim theta As Single = Math.Acos(2.0F * xi0 - 1.0F)
        Dim phi As Single = xi1 * 2.0F * Math.PI
        Dim res_x As Single = Math.Sin(theta) * Math.Cos(phi)
        Dim res_y As Single = Math.Sin(theta) * Math.Sin(phi)
        Dim res_z As Single = Math.Cos(theta)
        Return New Vector3(res_x, res_y, res_z)
    End Function

    Public Function GetPrime(idx As Integer) As Integer
        Dim candi As Integer() = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97}
        If idx < candi.Length - 1 Then
            Return candi(idx)
        End If
        Return 2
    End Function

    Public Function GetHalton(haltonIdx As Integer, haltonDimension As Integer) As Single
        Dim base As Integer = GetPrime(haltonDimension)
        Dim i As Integer = haltonIdx
        Dim f As Single = 1.0F
        Dim r As Single = 0.0F
        While (i > 0)
            f /= base
            r += f * (i Mod base)
            i \= base
        End While
        Return r
    End Function

    Public Function IntersectScene(rayIn As RTRay) As RTIntersectResult
        Dim tmpDepth As Single = 9999.0F
        Dim result As New RTIntersectResult
        result.Hit = False
        For Each tmpModel As Model In ModelRepository
            For Each tmpPoly As ModelPoly In tmpModel.Mesh.Values
                Dim pos0 As Vector3 = ObjLoader.VtxRepo(tmpPoly.VtxIdx(0))
                Dim pos1 As Vector3 = ObjLoader.VtxRepo(tmpPoly.VtxIdx(1))
                Dim pos2 As Vector3 = ObjLoader.VtxRepo(tmpPoly.VtxIdx(2))
                Dim e0 As Vector3 = pos1 - pos0
                Dim e1 As Vector3 = pos2 - pos0
                Dim sp As Vector3 = rayIn.Position - pos0
                Dim de As Vector3() = {-rayIn.Direction, e0, e1}
                Dim tuv As Vector3 = Matrix3x3Mul(Matrix3x3Inv(de), sp)
                If (tuv.Y >= 0.0F AndAlso tuv.Y <= 1.0F AndAlso tuv.Z >= 0.0F AndAlso tuv.Z <= 1.0F AndAlso tuv.Y + tuv.Z <= 1.0F) Then
                    If (tuv.X > 0.01 AndAlso tuv.X < tmpDepth) Then
                        tmpDepth = tuv.X

                        Dim norm0 As Vector3 = ObjLoader.NormalRepo(tmpPoly.NormalIdx(0))
                        Dim norm1 As Vector3 = ObjLoader.NormalRepo(tmpPoly.NormalIdx(1))
                        Dim norm2 As Vector3 = ObjLoader.NormalRepo(tmpPoly.NormalIdx(2))
                        Dim norme0 As Vector3 = norm1 - norm0
                        Dim norme1 As Vector3 = norm2 - norm0

                        result.Hit = True
                        result.TUV = tuv
                        result.MatId = tmpPoly.MaterialName
                        result.Position = pos0 + tuv.Y * e0 + tuv.Z * e1
                        result.Normal = Vector3.Normalize(norm0 + tuv.Y * norme0 + tuv.Z * norme1)
                        result.SourceDirection = rayIn.Direction

                        If tmpPoly.TexCoordIdx IsNot Nothing Then
                            Dim tc0 As Vector2 = ObjLoader.TexCoordRepo(tmpPoly.TexCoordIdx(0))
                            Dim tc1 As Vector2 = ObjLoader.TexCoordRepo(tmpPoly.TexCoordIdx(1))
                            Dim tc2 As Vector2 = ObjLoader.TexCoordRepo(tmpPoly.TexCoordIdx(2))
                            Dim tce0 As Vector2 = tc1 - tc0
                            Dim tce1 As Vector2 = tc2 - tc0
                            result.TextCoord = tc0 + tuv.Y * tce0 + tuv.Z * tce1
                        End If

                    End If
                End If
            Next
        Next
        Return result
    End Function

    Private Function TestSingleAABB(rayIn As RTRay, aabb As ModelOctree) As Single
        If (rayIn.Position.X >= aabb.Region(0).X AndAlso rayIn.Position.X <= aabb.Region(1).X _
            AndAlso rayIn.Position.Y >= aabb.Region(0).Y AndAlso rayIn.Position.Y <= aabb.Region(1).Y _
            AndAlso rayIn.Position.Z >= aabb.Region(0).Z AndAlso rayIn.Position.Z <= aabb.Region(1).Z) Then
            Return 0.001
        End If

        Dim boxIntersect As Boolean = False
        Dim boxIntersectValue As Single = 9999.0F
        ' test front, rear
        If rayIn.Direction.Z <> 0 Then
            For i = 0 To 1
                Dim u1 As Single = aabb.Region(i).Z - rayIn.Position.Z / rayIn.Direction.Z
                If u1 > 0 Then
                    Dim m1 As New Vector3(rayIn.Position.X + u1 * rayIn.Direction.X, rayIn.Position.Y + u1 * rayIn.Direction.Y, 0)
                    If (m1.X >= aabb.Region(0).X AndAlso m1.X <= aabb.Region(1).X AndAlso m1.Y >= aabb.Region(0).Y AndAlso m1.Y <= aabb.Region(1).Y) Then
                        boxIntersect = True
                        If u1 < boxIntersectValue Then
                            boxIntersectValue = u1
                        End If
                    End If
                End If
            Next
        End If
        ' test left, right
        If rayIn.Direction.X <> 0 Then
            For i = 0 To 1
                Dim u1 As Single = aabb.Region(i).X - rayIn.Position.X / rayIn.Direction.X
                If u1 > 0 Then
                    Dim m1 As New Vector3(0, rayIn.Position.Y + u1 * rayIn.Direction.Y, rayIn.Position.Z + u1 * rayIn.Direction.Z)
                    If (m1.Z >= aabb.Region(0).Z AndAlso m1.Z <= aabb.Region(1).Z AndAlso m1.Y >= aabb.Region(0).Y AndAlso m1.Y <= aabb.Region(1).Y) Then
                        boxIntersect = True
                        If u1 < boxIntersectValue Then
                            boxIntersectValue = u1
                        End If
                    End If
                End If
            Next
        End If
        ' test top, bottom
        If rayIn.Direction.Y <> 0 Then
            For i = 0 To 1
                Dim u1 As Single = aabb.Region(i).Y - rayIn.Position.Y / rayIn.Direction.Y
                If u1 > 0 Then
                    Dim m1 As New Vector3(rayIn.Position.X + u1 * rayIn.Direction.X, 0, rayIn.Position.Z + u1 * rayIn.Direction.Z)
                    If (m1.X >= aabb.Region(0).X AndAlso m1.X <= aabb.Region(1).X AndAlso m1.Z >= aabb.Region(0).Z AndAlso m1.Z <= aabb.Region(1).Z) Then
                        boxIntersect = True
                        If u1 < boxIntersectValue Then
                            boxIntersectValue = u1
                        End If
                    End If
                End If
            Next
        End If

        If boxIntersect Then
            Return boxIntersectValue
        End If
        Return -1.0
    End Function

    Private Function TestSingleAABB(rayIn As RTRay, aabb As BVH) As Single

        Dim position As Vector3 = (aabb.Region(0) + aabb.Region(1)) / 2.0F
        Dim radius As Single = Vector3.Distance(aabb.Region(0), position) + 0.01F
        Dim o2p As Vector3 = rayIn.Position - position
        Dim a As Single = Vector3.Dot(rayIn.Direction, rayIn.Direction)
        Dim b As Single = 2.0F * Vector3.Dot(rayIn.Direction, o2p)
        Dim c As Single = Vector3.Dot(o2p, o2p) - radius * radius
        Dim delta As Single = b * b - 4.0F * a * c
        If delta > 0 Then
            Dim t0 As Single = (-b - Math.Sqrt(delta)) / (2.0F * a)
            Dim t1 As Single = (-b + Math.Sqrt(delta)) / (2.0F * a)
            If t0 > 0 OrElse t1 > 0 Then
                Return 1.0F
            End If
        ElseIf delta = 0 Then
            Dim t0 As Single = (-b) / (2.0F * a)
            If t0 > 0 Then
                Return 1.0F
            End If
        End If
        Return -1.0F

        'If (rayIn.Position.X >= aabb.Region(0).X AndAlso rayIn.Position.X <= aabb.Region(1).X _
        '    AndAlso rayIn.Position.Y >= aabb.Region(0).Y AndAlso rayIn.Position.Y <= aabb.Region(1).Y _
        '    AndAlso rayIn.Position.Z >= aabb.Region(0).Z AndAlso rayIn.Position.Z <= aabb.Region(1).Z) Then
        '    Return 0.5
        'End If

        'Dim t As Single
        'If rayIn.Direction.X <> 0 Then
        '    If rayIn.Direction.X > 0 Then
        '        t = aabb.Region(0).X - rayIn.Position.X / rayIn.Direction.X
        '    Else
        '        t = aabb.Region(1).X - rayIn.Position.X / rayIn.Direction.X
        '    End If
        '    If t > 0 Then
        '        Dim pt As Vector3 = rayIn.Position + t * rayIn.Direction
        '        If pt.Y >= aabb.Region(0).Y AndAlso pt.Y <= aabb.Region(1).Y AndAlso pt.Z >= aabb.Region(0).Z AndAlso pt.Z <= aabb.Region(1).Z Then
        '            Return 1.0F
        '        End If
        '    End If
        'End If
        'If rayIn.Direction.Y <> 0 Then
        '    If rayIn.Direction.Y > 0 Then
        '        t = aabb.Region(0).Y - rayIn.Position.Y / rayIn.Direction.Y
        '    Else
        '        t = aabb.Region(1).Y - rayIn.Position.Y / rayIn.Direction.Y
        '    End If
        '    If t > 0 Then
        '        Dim pt As Vector3 = rayIn.Position + t * rayIn.Direction
        '        If pt.X >= aabb.Region(0).X AndAlso pt.X <= aabb.Region(1).X AndAlso pt.Z >= aabb.Region(0).Z AndAlso pt.Z <= aabb.Region(1).Z Then
        '            Return 1.0F
        '        End If
        '    End If
        'End If
        'If rayIn.Direction.Z <> 0 Then
        '    If rayIn.Direction.Z > 0 Then
        '        t = aabb.Region(0).Z - rayIn.Position.Z / rayIn.Direction.Z
        '    Else
        '        t = aabb.Region(1).Z - rayIn.Position.Z / rayIn.Direction.Z
        '    End If
        '    If t > 0 Then
        '        Dim pt As Vector3 = rayIn.Position + t * rayIn.Direction
        '        If pt.Y >= aabb.Region(0).Y AndAlso pt.Y <= aabb.Region(1).Y AndAlso pt.X >= aabb.Region(0).X AndAlso pt.X <= aabb.Region(1).X Then
        '            Return 1.0F
        '        End If
        '    End If
        'End If

        'Return -1.0F
    End Function


    Private Function AABBHitCompare(a As KeyValuePair(Of Integer, Single), b As KeyValuePair(Of Integer, Single)) As Single
        Return a.Value - b.Value
    End Function

    Private Function IntersectNodeAABB(rayIn As RTRay, aabb As ModelOctree) As RTIntersectResult
        If (aabb.Children(0) IsNot Nothing) Then
            Dim tmpDepth As Single = 9999.0F
            Dim result As New RTIntersectResult() With {.Hit = False}

            Dim candidate As New List(Of KeyValuePair(Of Integer, Single))
            For i = 0 To 7
                Dim testChild As Single = TestSingleAABB(rayIn, aabb.Children(i))
                If testChild >= 0 Then
                    Dim kvp As New KeyValuePair(Of Integer, Single)(i, testChild)
                    candidate.Add(kvp)
                End If
            Next
            If candidate.Count > 0 Then
                candidate.Sort(New Comparison(Of KeyValuePair(Of Integer, Single))(AddressOf AABBHitCompare))
                For i = 0 To candidate.Count - 1
                    Dim idx As Integer = candidate(i).Key
                    Dim tmpResult As RTIntersectResult = IntersectNodeAABB(rayIn, aabb.Children(idx))
                    If (tmpResult.Hit AndAlso tmpResult.TUV.X < tmpDepth) Then
                        tmpDepth = tmpResult.TUV.X
                        result = tmpResult
                    End If
                Next
            End If
            Return result
        Else    ' test polygons
            Dim tmpDepth As Single = 9999.0F
            Dim result As New RTIntersectResult() With {.Hit = False}
            For Each tmpPoly As ModelPoly In aabb.Content
                Dim pos0 As Vector3 = ObjLoader.VtxRepo(tmpPoly.VtxIdx(0))
                Dim pos1 As Vector3 = ObjLoader.VtxRepo(tmpPoly.VtxIdx(1))
                Dim pos2 As Vector3 = ObjLoader.VtxRepo(tmpPoly.VtxIdx(2))
                Dim e0 As Vector3 = pos1 - pos0
                Dim e1 As Vector3 = pos2 - pos0
                Dim sp As Vector3 = rayIn.Position - pos0
                Dim de As Vector3() = {-rayIn.Direction, e0, e1}
                Dim tuv As Vector3 = Matrix3x3Mul(Matrix3x3Inv(de), sp)
                If (tuv.Y >= 0.0F AndAlso tuv.Y <= 1.0F AndAlso tuv.Z >= 0.0F AndAlso tuv.Z <= 1.0F AndAlso tuv.Y + tuv.Z <= 1.0F) Then
                    If (tuv.X > 0.001 AndAlso tuv.X < tmpDepth) Then
                        tmpDepth = tuv.X

                        Dim norm0 As Vector3 = ObjLoader.NormalRepo(tmpPoly.NormalIdx(0))
                        Dim norm1 As Vector3 = ObjLoader.NormalRepo(tmpPoly.NormalIdx(1))
                        Dim norm2 As Vector3 = ObjLoader.NormalRepo(tmpPoly.NormalIdx(2))
                        Dim norme0 As Vector3 = norm1 - norm0
                        Dim norme1 As Vector3 = norm2 - norm0

                        With result
                            .Hit = True
                            .TUV = tuv
                            .MatId = tmpPoly.MaterialName
                            .Position = pos0 + tuv.Y * e0 + tuv.Z * e1
                            .Normal = Vector3.Normalize(norm0 + tuv.Y * norme0 + tuv.Z * norme1)
                            .SourceDirection = rayIn.Direction
                        End With

                        If tmpPoly.TexCoordIdx IsNot Nothing Then
                            Dim tc0 As Vector2 = ObjLoader.TexCoordRepo(tmpPoly.TexCoordIdx(0))
                            Dim tc1 As Vector2 = ObjLoader.TexCoordRepo(tmpPoly.TexCoordIdx(1))
                            Dim tc2 As Vector2 = ObjLoader.TexCoordRepo(tmpPoly.TexCoordIdx(2))
                            Dim tce0 As Vector2 = tc1 - tc0
                            Dim tce1 As Vector2 = tc2 - tc0
                            result.TextCoord = tc0 + tuv.Y * tce0 + tuv.Z * tce1
                        End If

                    End If
                End If
            Next
            Return result
        End If
    End Function

    Private Function IntersectNodeAABB_All(rayIn As RTRay, bvh As BVH, Optional backFaceOnly As Boolean = False) As List(Of RTIntersectResult)
        Dim test0 As Single = TestSingleAABB(rayIn, bvh)
        If test0 < 0 Then
            Return New List(Of RTIntersectResult)    ' empty list
        End If

        If (bvh.Content Is Nothing) Then
            Dim result As New List(Of RTIntersectResult)
            For i = 0 To 1
                Dim tmpResult As List(Of RTIntersectResult) = IntersectNodeAABB_All(rayIn, bvh.Children(i), backFaceOnly)
                result.AddRange(tmpResult)
            Next
            Return result
        Else    ' test polygons
            Dim result As New List(Of RTIntersectResult)
            For Each tmpPoly As ModelPolyV In bvh.Content
                Dim tmpResult As New RTIntersectResult With {.Hit = False}

                Dim pos0 As Vector3 = tmpPoly.Vtx(0)
                Dim pos1 As Vector3 = tmpPoly.Vtx(1)
                Dim pos2 As Vector3 = tmpPoly.Vtx(2)
                Dim e0 As Vector3 = pos1 - pos0
                Dim e1 As Vector3 = pos2 - pos0
                Dim sp As Vector3 = rayIn.Position - pos0
                Dim de As Vector3() = {-rayIn.Direction, e0, e1}
                Dim tuv As Vector3 = Matrix3x3Mul(Matrix3x3Inv(de), sp)
                If (tuv.Y >= 0.0F AndAlso tuv.Y <= 1.0F AndAlso tuv.Z >= 0.0F AndAlso tuv.Z <= 1.0F AndAlso tuv.Y + tuv.Z <= 1.0F) Then
                    If (tuv.X > ACME_THRESHOLD AndAlso tuv.X < 9999.0F) Then
                        Dim norm0 As Vector3 = tmpPoly.Normal(0)
                        Dim norm1 As Vector3 = tmpPoly.Normal(1)
                        Dim norm2 As Vector3 = tmpPoly.Normal(2)
                        Dim norme0 As Vector3 = norm1 - norm0
                        Dim norme1 As Vector3 = norm2 - norm0
                        Dim res_normal As Vector3 = Vector3.Normalize(norm0 + tuv.Y * norme0 + tuv.Z * norme1)

                        Dim cullPoly As Boolean = False
                        If backFaceOnly Then
                            If Vector3.Dot(res_normal, -rayIn.Direction) > 0.0 Then
                                cullPoly = True    ' ignore front polys
                            End If
                        End If

                        If Not cullPoly Then
                            With tmpResult
                                .Hit = True
                                .TUV = tuv
                                .MatId = tmpPoly.MaterialName
                                .Position = pos0 + tuv.Y * e0 + tuv.Z * e1
                                .Normal = res_normal
                                .SourceDirection = rayIn.Direction
                            End With

                            If tmpPoly.TexCoord.Length > 0 Then
                                Dim tc0 As Vector2 = tmpPoly.TexCoord(0)
                                Dim tc1 As Vector2 = tmpPoly.TexCoord(1)
                                Dim tc2 As Vector2 = tmpPoly.TexCoord(2)
                                Dim tce0 As Vector2 = tc1 - tc0
                                Dim tce1 As Vector2 = tc2 - tc0
                                tmpResult.TextCoord = tc0 + tuv.Y * tce0 + tuv.Z * tce1
                            End If

                            result.Add(tmpResult)
                        End If

                    End If

                End If
            Next
            Return result
        End If
    End Function


    Private Function IntersectNodeAABB(rayIn As RTRay, bvh As BVH, Optional backFaceOnly As Boolean = False) As RTIntersectResult
        Dim test0 As Single = TestSingleAABB(rayIn, bvh)
        If test0 < 0 Then
            Return New RTIntersectResult() With {.Hit = False}
        End If

        If (bvh.Content Is Nothing) Then
            Dim tmpDepth As Single = 9999.0F
            Dim result As New RTIntersectResult() With {.Hit = False}

            For i = 0 To 1
                Dim tmpResult As RTIntersectResult = IntersectNodeAABB(rayIn, bvh.Children(i), backFaceOnly)
                If (tmpResult.Hit AndAlso tmpResult.TUV.X < tmpDepth) Then
                    tmpDepth = tmpResult.TUV.X
                    result = tmpResult
                End If
            Next
            Return result
        Else    ' test polygons
            Dim tmpDepth As Single = 9999.0F
            Dim result As New RTIntersectResult() With {.Hit = False}
            For Each tmpPoly As ModelPolyV In bvh.Content
                Dim pos0 As Vector3 = tmpPoly.Vtx(0)
                Dim pos1 As Vector3 = tmpPoly.Vtx(1)
                Dim pos2 As Vector3 = tmpPoly.Vtx(2)
                Dim e0 As Vector3 = pos1 - pos0
                Dim e1 As Vector3 = pos2 - pos0
                Dim sp As Vector3 = rayIn.Position - pos0
                Dim de As Vector3() = {-rayIn.Direction, e0, e1}
                Dim tuv As Vector3 = Matrix3x3Mul(Matrix3x3Inv(de), sp)
                If (tuv.Y >= 0.0F AndAlso tuv.Y <= 1.0F AndAlso tuv.Z >= 0.0F AndAlso tuv.Z <= 1.0F AndAlso tuv.Y + tuv.Z <= 1.0F) Then
                    If (tuv.X > ACME_THRESHOLD AndAlso tuv.X < tmpDepth) Then
                        Dim norm0 As Vector3 = tmpPoly.Normal(0)
                        Dim norm1 As Vector3 = tmpPoly.Normal(1)
                        Dim norm2 As Vector3 = tmpPoly.Normal(2)
                        Dim norme0 As Vector3 = norm1 - norm0
                        Dim norme1 As Vector3 = norm2 - norm0
                        Dim res_normal As Vector3 = Vector3.Normalize(norm0 + tuv.Y * norme0 + tuv.Z * norme1)

                        Dim cullPoly As Boolean = False
                        If backFaceOnly Then
                            If Vector3.Dot(res_normal, -rayIn.Direction) > 0.0 Then
                                cullPoly = True    ' ignore front polys
                            End If
                        End If

                        If Not cullPoly Then
                            tmpDepth = tuv.X

                            With result
                                .Hit = True
                                .TUV = tuv
                                .MatId = tmpPoly.MaterialName
                                .Position = pos0 + tuv.Y * e0 + tuv.Z * e1
                                .Normal = res_normal
                                .SourceDirection = rayIn.Direction
                            End With

                            If tmpPoly.TexCoord.Length > 0 Then
                                Dim tc0 As Vector2 = tmpPoly.TexCoord(0)
                                Dim tc1 As Vector2 = tmpPoly.TexCoord(1)
                                Dim tc2 As Vector2 = tmpPoly.TexCoord(2)
                                Dim tce0 As Vector2 = tc1 - tc0
                                Dim tce1 As Vector2 = tc2 - tc0
                                result.TextCoord = tc0 + tuv.Y * tce0 + tuv.Z * tce1
                            End If

                        End If

                    End If

                End If
            Next
            Return result
        End If
    End Function

    Public Function IntersectLights(rayIn As RTRay) As RTIntersectResult
        Dim result As New RTIntersectResult With {.Hit = False}
        Dim minDist As Single = 9999.0F
        For Each light As RTLight In LightRepository
            If light.GetType = GetType(SphereLight) Then
                Dim tmpResult As RTIntersectResult = CType(light, SphereLight).RayHit(rayIn)
                If tmpResult.TUV.X > 0 Then
                    If tmpResult.TUV.X < minDist Then
                        minDist = tmpResult.TUV.X
                        result = tmpResult
                    End If
                End If
            End If
        Next
        Return result
    End Function

    Public Function IntersectSceneAABB_All(rayIn As RTRay) As List(Of RTIntersectResult)
        Dim result As List(Of RTIntersectResult) = IntersectNodeAABB_All(rayIn, BVHRepository)
        Return result
    End Function

    Public Function IntersectSceneAABB(rayIn As RTRay) As RTIntersectResult

        'Dim result As RTIntersectResult = IntersectNodeAABB(rayIn, AABBRepository)
        Dim result As RTIntersectResult = IntersectNodeAABB(rayIn, BVHRepository)
        Dim lightHit As RTIntersectResult = IntersectLights(rayIn)
        If Not result.Hit Then
            Return lightHit
        End If
        If Not lightHit.Hit Then
            Return result
        End If
        If result.TUV.X < lightHit.TUV.X Then
            Return result
        End If
        Return lightHit

    End Function

    Public Function IntersectBackSceneAABB(rayIn As RTRay) As RTIntersectResult
        Dim result As RTIntersectResult = IntersectNodeAABB(rayIn, ExtrudedBVHRepository, True)
        Return result
    End Function

    Public Function GetPhongBRDF(mat As ModelMaterial, normal As Vector3, rayIn As RTRay, rayOut As RTRay, uv As Vector2) As Vector3
        Dim n As Single = mat.SpecularExponent
        Dim r As Vector3 = rayIn.Direction - 2.0F * Vector3.Dot(normal, rayIn.Direction) * normal
        Dim refl As Vector3 = mat.Specular * (n + 2.0F) * Math.Pow(Math.Max(0.0F, Vector3.Dot(r, rayOut.Direction)), n) / (2.0F * Math.PI)

        Dim cos_theta As Single = Math.Max(0.0F, Vector3.Dot(rayOut.Direction, normal))
        Dim rho As Single = 1.0F
        Dim diffuse As Vector3
        If mat.DiffuseTexture Is Nothing Then
            diffuse = mat.Diffuse * rho / Math.PI
        Else
            Dim texColor As Vector4 = mat.DiffuseTexture.GetPixel(uv)
            diffuse = New Vector3(texColor.X, texColor.Y, texColor.Z) * rho / Math.PI
        End If

        Dim combine As Vector3 = refl + diffuse
        Return combine * cos_theta
    End Function


    ''' <summary>
    ''' ray tracing
    ''' </summary>
    ''' <param name="rayIn"></param>
    ''' <param name="useAABB"></param>
    ''' <returns></returns>
    Private Function RTPixel_BlinnPhong(rayIn As RTRay, iterateDepth As Integer, Optional useAABB As Boolean = False) As Vector3
        Dim intersectResult As RTIntersectResult
        If useAABB Then
            intersectResult = IntersectSceneAABB(rayIn)
        Else
            intersectResult = IntersectScene(rayIn)
        End If
        If (intersectResult.Hit) Then
            Dim tmpMat As ModelMaterial = ObjLoader.MatRepo(intersectResult.MatId)
            ' Blinn-Phong
            Dim matDiffuse As Vector3 = tmpMat.Diffuse
            'If tmpMat.DiffuseTexture IsNot Nothing Then
            '    matDiffuse = tmpMat.DiffuseTexture.GetPixel(intersectResult.TextCoord)
            'End If
            Dim matSpecular As Vector3 = tmpMat.Specular
            Dim res_emission As Vector3 = tmpMat.Emission * tmpMat.EmissionStrength
            Dim res_ambient As Vector3 = tmpMat.Ambient * tmpMat.AmbientStrength
            res_ambient = New Vector3(0.01)  'test

            Dim res_diffuse As Vector3 = Vector3.Zero
            Dim res_specular As Vector3 = Vector3.Zero
            For Each tmpLight As RTLight In LightRepository
                Dim shadowTest As Single = tmpLight.CalcShadowTest(intersectResult, useAABB)
                If (shadowTest > 0.0F) Then
                    Dim lightColor As Vector3 = tmpLight.Color
                    Dim diffuseStrength As Single = tmpLight.CalcDiffuseStrength(intersectResult)
                    res_diffuse += (diffuseStrength * lightColor * matDiffuse)
                    Dim specularStrength As Single = tmpLight.CalcSpecularStrength(intersectResult, tmpMat.SpecularExponent)
                    res_specular += (specularStrength * lightColor * matSpecular)
                End If
            Next
            Dim res_color As Vector3 = res_emission + res_ambient + res_diffuse + res_specular

            If iterateDepth < ReflectDepth Then
                Dim subRays As New List(Of RTRay)
                ' reflect
                If tmpMat.ReflectStrength > 0 Then
                    Dim refl_dir As Vector3 = rayIn.Direction - 2.0F * intersectResult.Normal * Vector3.Dot(rayIn.Direction, intersectResult.Normal)
                    Dim refl_ray As New RTRay(intersectResult.Position, refl_dir, tmpMat.ReflectStrength)
                    subRays.Add(refl_ray)
                End If
                ' refract
                'TODO

                For Each nextRay As RTRay In subRays
                    Dim nextResult As Vector3 = RTPixel_BlinnPhong(nextRay, iterateDepth + 1, useAABB)
                    res_color += nextResult
                Next
            End If
            ' 注意，不要让最终颜色限制在1.0内
            Return (res_color * rayIn.Weight)
        End If
        Return Vector3.One
    End Function

    Public Function CmpRtIntersectResult(a As RTIntersectResult, b As RTIntersectResult) As Integer
        If a.TUV.X > b.TUV.X Then
            Return 1
        ElseIf a.TUV.X < b.TUV.X Then
            Return -1
        End If
        Return 0
    End Function

    Private Function RTPixel_NPR(rayIn As RTRay, iterateDepth As Integer, Optional useAABB As Boolean = False) As Vector3
        Dim intersectResult As List(Of RTIntersectResult) = IntersectSceneAABB_All(rayIn)    '改用全部深度结果来允许处理alpha通道
        If intersectResult.Count > 0 Then
            intersectResult.Sort(New Comparison(Of RTIntersectResult)(AddressOf CmpRtIntersectResult))
            Dim res_color As Vector3 = Vector3.Zero
            Dim remain_w As Single = 1.0F
            For Each singleHitResult As RTIntersectResult In intersectResult
                If (singleHitResult.Hit) Then
                    Dim tmpMat As ModelMaterial = ObjLoader.MatRepo(singleHitResult.MatId)

                    Dim matDiffuse As Vector4 = New Vector4(tmpMat.Diffuse.X, tmpMat.Diffuse.Y, tmpMat.Diffuse.Z, 1.0F)
                    If tmpMat.DiffuseTexture IsNot Nothing Then
                        matDiffuse = tmpMat.DiffuseTexture.GetPixel(singleHitResult.TextCoord)
                    End If

                    Dim matEmission As Vector3 = tmpMat.Emission * tmpMat.EmissionStrength
                    Dim result As Vector4 = matDiffuse + New Vector4(matEmission, 0.0F)

                    Dim hasShadow As Boolean = False
                    For Each tmpLight As RTLight In LightRepository
                        Dim shadowTest As Single = tmpLight.CalcShadowTest(singleHitResult, useAABB)
                        If (shadowTest > 0.0F) Then
                            'Dim lightDir As Vector3 = Vector3.Normalize(tmpLight.GetPosition() - singleHitResult.Position)
                            'If tmpLight.GetType = GetType(DirectionalLight) Then
                            '    lightDir = -CType(tmpLight, DirectionalLight).LightDirection
                            'End If
                            'Dim normal As Vector3 = singleHitResult.Normal
                            'Dim theta As Single = Vector3.Dot(lightDir, normal)
                            'If theta < 0.5 Then
                            '    hasShadow = True
                            'End If

                            Dim lightStrength As Single = tmpLight.CalcDiffuseStrength(singleHitResult)
                            lightStrength = {lightStrength, 1.0F}.Min

                            If lightStrength < 0.5 Then
                                hasShadow = True
                            End If
                        Else
                            hasShadow = True
                        End If

                        If hasShadow Then
                            '赛璐璐 正片叠底
                            Dim shadowColor As New Vector4(tmpLight.Color, 255)
                            shadowColor /= 255.0
                            result *= shadowColor
                        End If
                    Next
                    Dim resultV3 As Vector3 = New Vector3(result.X, result.Y, result.Z)
                    res_color += (resultV3 * result.W * remain_w)
                    remain_w -= (result.W * remain_w)

                    If remain_w < 0.01F Then
                        ' 注意，不要让最终颜色限制在1.0内
                        Return (res_color * rayIn.Weight)
                        'Exit For
                    End If
                End If
            Next

            res_color += (Vector3.One * remain_w)
            ' 注意，不要让最终颜色限制在1.0内
            Return (res_color * rayIn.Weight)
        End If
        Return Vector3.One
    End Function


    ''' <summary>
    ''' path tracing
    ''' </summary>
    ''' <param name="rayIn"></param>
    ''' <param name="useAABB"></param>
    ''' <returns></returns>
    Private Function PTPixel(rayIn As RTRay, sampleIdx As Integer, Optional useAABB As Boolean = False) As Vector3
        Dim intersectResult As RTIntersectResult
        If useAABB Then
            intersectResult = IntersectSceneAABB(rayIn)
        Else
            intersectResult = IntersectScene(rayIn)
        End If
        If (intersectResult.Hit) Then
            'Return New Vector3(1.0F)

            Dim finalColor As New Vector3(0.0F)
            Dim weight As New Vector3(1.0F)
            Dim currentHitInfo As RTIntersectResult = intersectResult
            Dim ray0 As RTRay = rayIn
            For rd = 0 To ReflectDepth
                If Not currentHitInfo.Hit Then Exit For
                Dim currentMaterial As ModelMaterial = ObjLoader.MatRepo(currentHitInfo.MatId)
                Dim emit As Vector3
                If currentMaterial.EmissionTexture Is Nothing Then
                    emit = currentMaterial.Emission
                Else
                    Dim emitColor As Vector4 = currentMaterial.EmissionTexture.GetPixel(currentHitInfo.TextCoord)
                    emit = New Vector3(emitColor.X, emitColor.Y, emitColor.Z)
                End If
                finalColor += (weight * currentMaterial.EmissionStrength * emit)

                Dim reflRay As New RTRay
                reflRay.Position = currentHitInfo.Position
                'reflRay.Direction = GetRandomDirection(sampleIdx, PATH_SAMPLE_DIMENSION + 2 * rd)
                reflRay.Direction = GetRandomDirection()
                If Vector3.Dot(reflRay.Direction, currentHitInfo.Normal) * Vector3.Dot(-ray0.Direction, currentHitInfo.Normal) < 0.0F Then
                    reflRay.Direction = -reflRay.Direction
                End If
                weight *= GetPhongBRDF(currentMaterial, currentHitInfo.Normal, ray0, reflRay, currentHitInfo.TextCoord)

                ray0 = reflRay
                If useAABB Then
                    currentHitInfo = IntersectSceneAABB(ray0)
                Else
                    currentHitInfo = IntersectScene(ray0)
                End If
            Next
            ' 注意，不能让最终颜色限制在1.0内，因为超出的部分仍会参与反射
            Return finalColor
        End If
        Return New Vector3(0.0F)
    End Function

    Public Sub PTRenderSample(sampleIndex As Integer)
        If ImageBuffer IsNot Nothing Then
            ImageBuffer.Dispose()
        End If
        ImageBuffer = New Bitmap(ImageWidth, ImageHeight)

        Dim mat_camera_rot As Matrix4x4 = Matrix4x4.CreateFromYawPitchRoll(CameraRotation.X, CameraRotation.Y, CameraRotation.Z)
        For y = 0 To ImageHeight - 1
            Dim f_y As Single = 0.5 * F_ImageHeight - (F_ImageHeight / ImageHeight) * (0.5 + y)
            For x = 0 To ImageWidth - 1
                Dim f_x As Single = -0.5 * F_ImageWidth + (F_ImageWidth / ImageWidth) * (0.5 + x)
                Dim dir0 As Vector3 = Vector3.Normalize(New Vector3(f_x, f_y, -1.0F))
                Dim dir1 As Vector3 = Vector3.Transform(dir0, mat_camera_rot)
                Dim tmpColor As Vector3 = PTPixel(New RTRay(CameraPosition, dir1), sampleIndex, True)

                Dim cumulativeColor As Vector3 = PTBuffer(x, y)

                Dim brightnessFactor As Single = 1.0F
                Dim finalColor As Vector3 = (cumulativeColor * sampleIndex + tmpColor) * brightnessFactor / (sampleIndex + 1.0F)
                PTBuffer(x, y) = finalColor

                Dim displayColor As Vector3 = Vector3.Min(finalColor, New Vector3(1.0F))
                Dim sysColor As Color = Drawing.Color.FromArgb(CInt(displayColor.X * 255), CInt(displayColor.Y * 255), CInt(displayColor.Z * 255))
                ImageBuffer.SetPixel(x, y, sysColor)
            Next
        Next
    End Sub

    Public Sub BeginRender(Optional useAABB As Boolean = False)
        If ImageBuffer IsNot Nothing Then
            ImageBuffer.Dispose()
        End If
        ImageBuffer = New Bitmap(ImageWidth, ImageHeight)
        ImageBufferFinishCount = 0
        Dim mat_camera_rot As Matrix4x4 = Matrix4x4.CreateFromYawPitchRoll(CameraRotation.X, CameraRotation.Y, CameraRotation.Z)

        For y = 0 To ImageHeight - 1
            Dim f_y As Single = 0.5 * F_ImageHeight - (F_ImageHeight / ImageHeight) * (0.5 + y)
            Dim localY As Integer = y + 0

            Dim processRow = Sub()
                                 For x = 0 To ImageWidth - 1

                                     Dim f_x As Single = -0.5 * F_ImageWidth + (F_ImageWidth / ImageWidth) * (0.5 + x)
                                     Dim dir0 As Vector3 = Vector3.Normalize(New Vector3(f_x, f_y, -1.0F))
                                     Dim dir1 As Vector3 = Vector3.Transform(dir0, mat_camera_rot)

                                     Dim localX As Integer = x + 0

                                     Dim rayIn As New RTRay(CameraPosition, dir1)
                                     Dim tmpColor As Vector3 = RTPixel_NPR(rayIn, 0, useAABB)
                                     'depth
                                     Dim depthVal As Single = 9999.0F
                                     Dim tmpDepth As RTIntersectResult = IntersectSceneAABB(rayIn)
                                     If tmpDepth.Hit Then
                                         depthVal = tmpDepth.TUV.X
                                     End If
                                     'edge
                                     Dim edgeTest As RTIntersectResult = IntersectBackSceneAABB(rayIn)    ' back-facing edge render
                                     If (edgeTest.Hit) Then
                                         If edgeTest.TUV.X < depthVal Then
                                             tmpColor *= 0.25
                                         End If
                                     End If
                                     tmpColor = Vector3.Min(tmpColor, New Vector3(1.0F))
                                     Dim sysColor As Color = Drawing.Color.FromArgb(255 * tmpColor.X, 255 * tmpColor.Y, 255 * tmpColor.Z)
                                     SyncLock ImageBufferFinishCountLock
                                         ImageBuffer.SetPixel(localX, localY, sysColor)
                                         ImageBufferFinishCount += 1
                                     End SyncLock
                                 Next
                             End Sub
            Dim tmpTask As New Task(processRow)
            tmpTask.Start()

        Next

    End Sub

    Public Sub GenerateAABB()
        'AABBRepository = New ModelOctree
        'With AABBRepository
        '    .NodeDepth = 0
        '    .Region(0) = New Vector3(ObjLoader.MinX, ObjLoader.MinY, ObjLoader.MinZ)
        '    .Region(1) = New Vector3(ObjLoader.MaxX, ObjLoader.MaxY, ObjLoader.MaxZ)
        'End With
        'For Each tmpModel As Model In ModelRepository
        '    AABBRepository.Content.AddRange(tmpModel.Mesh.Values)
        'Next
        'AABBRepository.Generate()

        Dim polys As New List(Of ModelPoly)
        For Each tmpModel As Model In ModelRepository
            polys.AddRange(tmpModel.Mesh.Values)
        Next
        BVHRepository = BVH.GenerateBVH(polys)

        ExtrudedBVHRepository = BVHRepository.Clone
        ExtrudedBVHRepository.ExtrudePoly(EDGE_WIDTH)

    End Sub



End Module

Public MustInherit Class RTLight

    Public Color As Vector3
    Public Strength As Single = 1.0F

    Public MustOverride Function CalcDiffuseStrength(input As RTIntersectResult) As Single
    Public MustOverride Function CalcSpecularStrength(input As RTIntersectResult, specExp As Single) As Single
    Public MustOverride Function CalcShadowTest(input As RTIntersectResult, Optional useAABB As Boolean = False) As Single
    Public MustOverride Function GetPosition() As Vector3

End Class

Public Class PointLight
    Inherits RTLight

    Public Position As Vector3

    Public Overrides Function CalcDiffuseStrength(input As RTIntersectResult) As Single
        Dim hit2light As Vector3 = Position - input.Position
        Dim lightDir As Vector3 = Vector3.Normalize(hit2light)
        Dim lightDist As Single = hit2light.Length
        Dim lambert As Single = {0.0F, Vector3.Dot(input.Normal, lightDir)}.Max
        Dim diffuseStrength As Single = Strength / (4.0F * Math.PI * lightDist * lightDist)
        Return (lambert * diffuseStrength)
    End Function

    Public Overrides Function CalcSpecularStrength(input As RTIntersectResult, specExp As Single) As Single
        Dim hit2light As Vector3 = Position - input.Position
        Dim lightDir As Vector3 = Vector3.Normalize(hit2light)
        Dim lightDist As Single = hit2light.Length
        Dim half As Vector3 = Vector3.Normalize(lightDir - input.SourceDirection)
        Dim blinnPhong As Single = {0.0F, Vector3.Dot(input.Normal, half)}.Max
        Dim specStrength As Single = Strength / (4.0F * Math.PI * lightDist * lightDist)
        Return (blinnPhong ^ specExp) * specStrength
    End Function

    Public Overrides Function CalcShadowTest(input As RTIntersectResult, Optional useAABB As Boolean = False) As Single
        Dim hit2light As Vector3 = Position - input.Position
        Dim lightDist As Single = hit2light.Length
        Dim lightDir As Vector3 = Vector3.Normalize(hit2light)
        Dim shadowRay As New RTRay(input.Position, lightDir)
        Dim shadowTest As RTIntersectResult
        If useAABB Then
            shadowTest = IntersectSceneAABB(shadowRay)
        Else
            shadowTest = IntersectScene(shadowRay)
        End If

        'HACK: 没有考虑透明的情况
        If (shadowTest.Hit) Then
            If (shadowTest.TUV.X > 0.001 AndAlso shadowTest.TUV.X < lightDist) Then
                Return 0.0F
            End If
        End If
        Return 1.0F
    End Function

    Public Overrides Function GetPosition() As Vector3
        Return Position
    End Function
End Class

Public Class DirectionalLight
    Inherits RTLight

    Public LightDirection As New Vector3(0, 0, -1)

    Public Overrides Function CalcDiffuseStrength(input As RTIntersectResult) As Single
        Dim lambert As Single = {0.0F, Vector3.Dot(input.Normal, -Me.LightDirection)}.Max
        Return (lambert * Me.Strength)
    End Function

    Public Overrides Function CalcSpecularStrength(input As RTIntersectResult, specExp As Single) As Single
        Dim half As Vector3 = Vector3.Normalize(-Me.LightDirection - input.SourceDirection)
        Dim blinnPhong As Single = {0.0F, Vector3.Dot(input.Normal, half)}.Max
        Return (blinnPhong ^ specExp) * Me.Strength
    End Function

    Public Overrides Function CalcShadowTest(input As RTIntersectResult, Optional useAABB As Boolean = False) As Single
        Const lightDist As Single = 9999.0F
        Dim lightDir As Vector3 = -Me.LightDirection
        Dim shadowRay As New RTRay(input.Position, lightDir)
        Dim shadowTest As RTIntersectResult
        If useAABB Then
            shadowTest = IntersectSceneAABB(shadowRay)
        Else
            shadowTest = IntersectScene(shadowRay)
        End If

        'HACK: 没有考虑透明的情况
        If (shadowTest.Hit) Then
            If (shadowTest.TUV.X > 0.001 AndAlso shadowTest.TUV.X < lightDist) Then
                Return 0.0F
            End If
        End If
        Return 1.0F
    End Function

    Public Overrides Function GetPosition() As Vector3
        Return Vector3.Zero
    End Function
End Class

Public Class SphereLight
    Inherits RTLight

    Public Position As Vector3
    Public Radius As Single = 1.0F
    Public LightMaterial As String = ""

    Public Function RayHit(rayIn As RTRay) As RTIntersectResult
        Dim result As New RTIntersectResult
        Dim o2p As Vector3 = rayIn.Position - Position
        Dim a As Single = Vector3.Dot(rayIn.Direction, rayIn.Direction)
        Dim b As Single = 2.0F * Vector3.Dot(rayIn.Direction, o2p)
        Dim c As Single = Vector3.Dot(o2p, o2p) - Radius * Radius
        Dim delta As Single = b * b - 4.0F * a * c
        If delta > 0 Then
            Dim t0 As Single = (-b - Math.Sqrt(delta)) / (2.0F * a)
            Dim t1 As Single = (-b + Math.Sqrt(delta)) / (2.0F * a)

            Dim minDist As Single = 9999.0F
            If t0 > 0.001F Then
                If t0 < minDist Then minDist = t0
            End If
            If t1 > 0.001F Then
                If t1 < minDist Then minDist = t1
            End If
            If minDist < 9998.0F Then
                Dim hitPosition As Vector3 = rayIn.Position + minDist * rayIn.Direction
                Dim normal As Vector3
                If Vector3.Distance(rayIn.Position, Me.Position) < Me.Radius + 0.001 Then
                    normal = Vector3.Normalize(hitPosition - Me.Position)
                Else
                    normal = Vector3.Normalize(hitPosition - Me.Position)
                End If

                With result
                    .Hit = True
                    .Position = hitPosition
                    .Normal = normal
                    .SourceDirection = rayIn.Direction
                    .TUV.X = minDist
                    .MatId = LightMaterial
                End With
                Return result
            End If
        ElseIf delta = 0 Then
            Dim t0 As Single = (-b) / (2.0F * a)
            If t0 > 0.001F Then
                Dim minDist As Single = t0
                Dim hitPosition As Vector3 = rayIn.Position + minDist * rayIn.Direction
                Dim normal As Vector3
                If Vector3.Distance(rayIn.Position, Me.Position) < Me.Radius + 0.001 Then
                    normal = Vector3.Normalize(hitPosition - Me.Position)
                Else
                    normal = Vector3.Normalize(hitPosition - Me.Position)
                End If

                With result
                    .Hit = True
                    .Position = hitPosition
                    .Normal = normal
                    .SourceDirection = rayIn.Direction
                    .TUV.X = minDist
                    .MatId = LightMaterial
                End With
                Return result
            End If
        End If
        Return New RTIntersectResult With {.Hit = False}
    End Function

    Public Overrides Function CalcDiffuseStrength(input As RTIntersectResult) As Single
        Dim hit2light As Vector3 = Me.Position - input.Position
        Dim lightDir As Vector3 = Vector3.Normalize(hit2light)
        Dim lightDist As Single = hit2light.Length
        Dim minLightDist As Single = lightDist - Me.Radius
        Dim lambert As Single = {0.0F, Vector3.Dot(input.Normal, lightDir)}.Max
        Dim diffuseStrength As Single = Strength / (4.0F * Math.PI * minLightDist * minLightDist)
        Return (lambert * diffuseStrength)
    End Function

    Public Overrides Function CalcSpecularStrength(input As RTIntersectResult, specExp As Single) As Single
        Throw New NotImplementedException()
    End Function

    Public Overrides Function CalcShadowTest(input As RTIntersectResult, Optional useAABB As Boolean = False) As Single
        'Dim hit2light As Vector3 = Me.Position - input.Position
        'Dim lightDist As Single = hit2light.Length
        'Dim minLightDist As Single = lightDist - Me.Radius
        'Dim lightDir As Vector3 = Vector3.Normalize(hit2light)
        'Dim maxCos As Single = lightDist / Math.Sqrt(lightDist * lightDist + Me.Radius * Me.Radius)
        'Dim hitTestCount As Integer = 0
        'Dim hitPassCount As Integer = 0
        'While hitTestCount < 100
        '    Dim dirCorrect As Boolean = False
        '    Dim dir As Vector3
        '    While Not dirCorrect
        '        dir = GetRandomDirection()
        '        If Vector3.Dot(dir, lightDir) < maxCos Then
        '            dirCorrect = True
        '        End If
        '    End While
        '    Dim shadowRay As New RTRay(input.Position, dir)
        '    Dim shadowTest As RTIntersectResult = IntersectSceneAABB(shadowRay)
        '    If shadowTest.Hit Then
        '        If (shadowTest.TUV.X > 0.001 AndAlso shadowTest.TUV.X < minLightDist) Then
        '            'hitPassCount += 0
        '        Else
        '            hitPassCount += 1
        '        End If
        '    Else
        '        hitPassCount += 1
        '    End If
        '    hitTestCount += 1
        'End While
        'Return (hitPassCount * 1.0F / hitTestCount)
    End Function

    Public Overrides Function GetPosition() As Vector3
        Return Position
    End Function
End Class

Public Structure RTRay

    Public Position As Vector3
    Public Direction As Vector3
    Public Weight As Single

    Public Sub New(pos As Vector3, dir As Vector3)
        Position = pos
        Direction = dir
        Weight = 1.0F
    End Sub
    Public Sub New(pos As Vector3, dir As Vector3, strength As Single)
        Position = pos
        Direction = dir
        Weight = strength
    End Sub

End Structure

Public Structure RTIntersectResult

    Public Hit As Boolean

    Public TUV As Vector3
    Public MatId As String

    Public Position As Vector3
    Public Normal As Vector3
    Public TextCoord As Vector2
    Public SourceDirection As Vector3

End Structure

Public Class ModelOctree

    Public Content As New List(Of ModelPoly)

    Public NodeDepth As Integer = 0

    Public Children(7) As ModelOctree

    Public Region(1) As Vector3

    Public Sub Generate()
        If (Content.Count > 20 AndAlso NodeDepth < 10) Then
            Dim axis As Vector3 = (Region(0) + Region(1)) * 0.5
            For i = 0 To 7
                Children(i) = New ModelOctree
                Children(i).NodeDepth = NodeDepth + 1
            Next
            Children(0).Region(0) = axis
            Children(0).Region(1) = Region(1)
            Children(1).Region(0) = New Vector3(Region(0).X, axis.Y, axis.Z)
            Children(1).Region(1) = New Vector3(axis.X, Region(1).Y, Region(1).Z)
            Children(2).Region(0) = New Vector3(Region(0).X, Region(0).Y, axis.Z)
            Children(2).Region(1) = New Vector3(axis.X, axis.Y, Region(1).Z)
            Children(3).Region(0) = New Vector3(axis.X, Region(0).Y, axis.Z)
            Children(3).Region(1) = New Vector3(Region(1).X, axis.Y, Region(1).Z)

            Children(4).Region(0) = New Vector3(axis.X, axis.Y, Region(0).Z)
            Children(4).Region(1) = New Vector3(Region(1).X, Region(1).Y, axis.Z)
            Children(5).Region(0) = New Vector3(Region(0).X, axis.Y, Region(0).Z)
            Children(5).Region(1) = New Vector3(axis.X, Region(1).Y, axis.Z)
            Children(6).Region(0) = New Vector3(Region(0).X, Region(0).Y, Region(0).Z)
            Children(6).Region(1) = New Vector3(axis.X, axis.Y, axis.Z)
            Children(7).Region(0) = New Vector3(axis.X, Region(0).Y, Region(0).Z)
            Children(7).Region(1) = New Vector3(Region(1).X, axis.Y, axis.Z)

            For Each poly As ModelPoly In Content
                Dim target As New List(Of Integer)

                '算出三角面的包围盒
                Dim ep1 As New Vector3(9999, 9999, 9999)
                Dim ep2 As New Vector3(-9999, -9999, -9999)
                For i = 0 To 2
                    Dim vtx As Vector3 = ObjLoader.VtxRepo(poly.VtxIdx(i))
                    If vtx.X < ep1.X Then ep1.X = vtx.X
                    If vtx.X > ep2.X Then ep2.X = vtx.X
                    If vtx.Y < ep1.Y Then ep1.Y = vtx.Y
                    If vtx.Y > ep2.Y Then ep2.Y = vtx.Y
                    If vtx.Z < ep1.Z Then ep1.Z = vtx.Z
                    If vtx.Z > ep2.Z Then ep2.Z = vtx.Z
                Next
                Dim eps As Vector3() = {ep1, New Vector3(ep2.X, ep1.Y, ep1.Z),
                                        New Vector3(ep1.X, ep2.Y, ep1.Z), New Vector3(ep2.X, ep2.Y, ep1.Z),
                                        New Vector3(ep1.X, ep1.Y, ep2.Z), New Vector3(ep2.X, ep1.Y, ep2.Z),
                                        New Vector3(ep1.X, ep2.Y, ep2.Z), ep2}
                For Each ep_vtx As Vector3 In eps
                    Dim compareResult = CompareVec3(ep_vtx, axis)
                    If (Not target.Contains(compareResult)) Then
                        target.Add(compareResult)
                    End If
                Next
                For Each tar As Integer In target
                    Select Case tar
                        Case &B111
                            Children(0).Content.Add(poly)
                        Case &B11
                            Children(1).Content.Add(poly)
                        Case &B1
                            Children(2).Content.Add(poly)
                        Case &B101
                            Children(3).Content.Add(poly)
                        Case &B110
                            Children(4).Content.Add(poly)
                        Case &B10
                            Children(5).Content.Add(poly)
                        Case &B0
                            Children(6).Content.Add(poly)
                        Case &B100
                            Children(7).Content.Add(poly)
                    End Select
                Next
            Next

            For i = 0 To 7
                Dim minR As Vector3 = Children(i).Region(0)
                Dim maxR As Vector3 = Children(i).Region(1)
                For Each poly As ModelPoly In Children(i).Content
                    For j = 0 To 2
                        Dim tmpVtx As Vector3 = ObjLoader.VtxRepo(poly.VtxIdx(j))
                        If tmpVtx.X < minR.X Then minR.X = tmpVtx.X
                        If tmpVtx.Y < minR.Y Then minR.Y = tmpVtx.Y
                        If tmpVtx.Z < minR.Z Then minR.Z = tmpVtx.Z
                        If tmpVtx.X > maxR.X Then maxR.X = tmpVtx.X
                        If tmpVtx.Y > maxR.Y Then maxR.Y = tmpVtx.Y
                        If tmpVtx.Z > maxR.Z Then maxR.Z = tmpVtx.Z
                    Next
                Next
                'Children(i).Region = {minR, maxR}
                Children(i).Generate()
            Next
        End If
    End Sub

End Class

Public Class BVH

    Public Content As List(Of ModelPolyV) = Nothing

    Public NodeDepth As Integer = 0

    Public Children(1) As BVH

    Public Region(1) As Vector3

    Private Const AABBOffset As Single = 0.1F

    Public Shared Function GetEmptyLeaf() As BVH
        Dim leaf As New BVH
        With leaf
            .Content = New List(Of ModelPolyV)
        End With
        Return leaf
    End Function

    Public Shared Function BVHDist(a As BVH, b As BVH) As Single
        Dim centA As Vector3 = a.Region(0) + a.Region(1) / 2.0F
        Dim centB As Vector3 = b.Region(0) + b.Region(1) / 2.0F
        Return Vector3.Distance(centA, centB)
    End Function

    Public Shared Function MakeNode(a As BVH, b As BVH) As BVH
        Dim rMin As New Vector3({a.Region(0).X, b.Region(0).X}.Min, {a.Region(0).Y, b.Region(0).Y}.Min, {a.Region(0).Z, b.Region(0).Z}.Min)
        Dim rMax As New Vector3({a.Region(1).X, b.Region(1).X}.Max, {a.Region(1).Y, b.Region(1).Y}.Max, {a.Region(1).Z, b.Region(1).Z}.Max)
        Dim node As New BVH
        With node
            .NodeDepth = {a.NodeDepth, b.NodeDepth}.Max + 1
            .Children(0) = a
            .Children(1) = b
            .Region(0) = rMin - New Vector3(AABBOffset)
            .Region(1) = rMax + New Vector3(AABBOffset)
        End With
        Return node
    End Function

    Public Shared Function GenerateBVH(polyIn As List(Of ModelPoly)) As BVH
        Dim leafCount As Integer = (polyIn.Count \ 100) + 1
        Dim centers As New List(Of Vector3)
        Dim leaves As New List(Of BVH)
        For i = 0 To leafCount - 1
            Dim idx As Integer = Math.Floor(Rand.NextDouble * polyIn.Count)
            Dim tmpPoly As ModelPoly = polyIn(idx)
            Dim tmpCent As Vector3 = ObjLoader.VtxRepo(tmpPoly.VtxIdx(0)) + ObjLoader.VtxRepo(tmpPoly.VtxIdx(1)) + ObjLoader.VtxRepo(tmpPoly.VtxIdx(2)) / 3.0F
            centers.Add(tmpCent)
            leaves.Add(BVH.GetEmptyLeaf())
        Next

        'nearest neighbor
        For Each tmpPoly As ModelPoly In polyIn
            Dim tmpCent As Vector3 = ObjLoader.VtxRepo(tmpPoly.VtxIdx(0)) + ObjLoader.VtxRepo(tmpPoly.VtxIdx(1)) + ObjLoader.VtxRepo(tmpPoly.VtxIdx(2)) / 3.0F
            Dim minDist As Single = 9999.0F
            Dim minDistIdx As Integer = 0
            For i = 0 To leafCount - 1
                Dim tmpDist As Single = Vector3.Distance(tmpCent, centers(i))
                If (tmpDist < minDist) Then
                    minDist = tmpDist
                    minDistIdx = i
                End If
            Next

            Dim convertPoly As New ModelPolyV
            With convertPoly
                .MaterialName = tmpPoly.MaterialName
                ReDim .Vtx(2)
                .Vtx(0) = ObjLoader.VtxRepo(tmpPoly.VtxIdx(0))
                .Vtx(1) = ObjLoader.VtxRepo(tmpPoly.VtxIdx(1))
                .Vtx(2) = ObjLoader.VtxRepo(tmpPoly.VtxIdx(2))
                ReDim .Normal(2)
                .Normal(0) = ObjLoader.NormalRepo(tmpPoly.NormalIdx(0))
                .Normal(1) = ObjLoader.NormalRepo(tmpPoly.NormalIdx(1))
                .Normal(2) = ObjLoader.NormalRepo(tmpPoly.NormalIdx(2))
                If tmpPoly.TexCoordIdx.Length > 0 Then
                    ReDim .TexCoord(2)
                    .TexCoord(0) = ObjLoader.TexCoordRepo(tmpPoly.TexCoordIdx(0))
                    .TexCoord(1) = ObjLoader.TexCoordRepo(tmpPoly.TexCoordIdx(1))
                    .TexCoord(2) = ObjLoader.TexCoordRepo(tmpPoly.TexCoordIdx(2))
                Else
                    .TexCoord = {}
                End If
            End With
            leaves(minDistIdx).Content.Add(convertPoly)
        Next
        For i = 0 To leafCount - 1
            leaves(i).UpdateRegion()
        Next

        While (leaves.Count > 1)
            Dim idx As Integer = 0    'Math.Floor(Rand.NextDouble * leaves.Count)
            Dim target As BVH = leaves(idx)
            leaves.RemoveAt(idx)
            Dim minDist As Single = 9999.0F
            Dim minDistIdx As Integer = 0
            For i = 0 To leaves.Count - 1
                Dim tmpDist As Single = BVHDist(target, leaves(i))
                If tmpDist < minDist Then
                    minDist = tmpDist
                    minDistIdx = i
                End If
            Next
            Dim minDistBVH As BVH = leaves(minDistIdx)
            leaves.RemoveAt(minDistIdx)
            Dim tmpNode As BVH = MakeNode(target, minDistBVH)
            leaves.Add(tmpNode)
        End While

        Return leaves.First
    End Function

    Public Sub UpdateRegion()
        If Me.Children(0) Is Nothing Then
            Dim rMin As New Vector3(9999.0F)
            Dim rMax As New Vector3(-9999.0F)
            For Each poly As ModelPolyV In Me.Content
                For Each vtx As Vector3 In poly.Vtx
                    If vtx.X < rMin.X Then rMin.X = vtx.X
                    If vtx.Y < rMin.Y Then rMin.Y = vtx.Y
                    If vtx.Z < rMin.Z Then rMin.Z = vtx.Z
                    If vtx.X > rMax.X Then rMax.X = vtx.X
                    If vtx.Y > rMax.Y Then rMax.Y = vtx.Y
                    If vtx.Z > rMax.Z Then rMax.Z = vtx.Z
                Next
            Next
            Me.Region(0) = rMin - New Vector3(AABBOffset)
            Me.Region(1) = rMax + New Vector3(AABBOffset)
        Else
            Dim a As BVH = Me.Children(0)
            Dim b As BVH = Me.Children(1)
            Dim rMin As New Vector3({a.Region(0).X, b.Region(0).X}.Min, {a.Region(0).Y, b.Region(0).Y}.Min, {a.Region(0).Z, b.Region(0).Z}.Min)
            Dim rMax As New Vector3({a.Region(1).X, b.Region(1).X}.Max, {a.Region(1).Y, b.Region(1).Y}.Max, {a.Region(1).Z, b.Region(1).Z}.Max)
            Me.Region(0) = rMin - New Vector3(AABBOffset)
            Me.Region(1) = rMax + New Vector3(AABBOffset)
        End If
    End Sub

    Public Function Clone() As BVH
        Dim result As New BVH
        With result
            .NodeDepth = Me.NodeDepth
            .Region(0) = Me.Region(0)
            .Region(1) = Me.Region(1)
        End With

        If Me.Children(0) Is Nothing Then
            result.Content = New List(Of ModelPolyV)
            For Each tmpPolyV As ModelPolyV In Me.Content
                Dim newPoly As New ModelPolyV
                With newPoly
                    .MaterialName = tmpPolyV.MaterialName
                    ReDim .Vtx(2)
                    .Vtx(0) = New Vector3(tmpPolyV.Vtx(0).X, tmpPolyV.Vtx(0).Y, tmpPolyV.Vtx(0).Z)
                    .Vtx(1) = New Vector3(tmpPolyV.Vtx(1).X, tmpPolyV.Vtx(1).Y, tmpPolyV.Vtx(1).Z)
                    .Vtx(2) = New Vector3(tmpPolyV.Vtx(2).X, tmpPolyV.Vtx(2).Y, tmpPolyV.Vtx(2).Z)
                    ReDim .Normal(2)
                    .Normal(0) = New Vector3(tmpPolyV.Normal(0).X, tmpPolyV.Normal(0).Y, tmpPolyV.Normal(0).Z)
                    .Normal(1) = New Vector3(tmpPolyV.Normal(1).X, tmpPolyV.Normal(1).Y, tmpPolyV.Normal(1).Z)
                    .Normal(2) = New Vector3(tmpPolyV.Normal(2).X, tmpPolyV.Normal(2).Y, tmpPolyV.Normal(2).Z)
                    If tmpPolyV.TexCoord.Length > 0 Then
                        ReDim .TexCoord(2)
                        .TexCoord(0) = New Vector2(tmpPolyV.TexCoord(0).X, tmpPolyV.TexCoord(0).Y)
                        .TexCoord(1) = New Vector2(tmpPolyV.TexCoord(1).X, tmpPolyV.TexCoord(1).Y)
                        .TexCoord(2) = New Vector2(tmpPolyV.TexCoord(2).X, tmpPolyV.TexCoord(2).Y)
                    Else
                        .TexCoord = {}
                    End If
                End With
                result.Content.Add(newPoly)
            Next
        Else
            result.Children(0) = Me.Children(0).Clone
            result.Children(1) = Me.Children(1).Clone
        End If
        Return result
    End Function

    Public Sub ExtrudePoly(offset As Single)
        If Me.Children(0) Is Nothing Then
            For i = 0 To Me.Content.Count - 1
                Dim poly As ModelPolyV = Me.Content(i)
                For j = 0 To 2
                    Dim normal As Vector3 = poly.Normal(j)
                    Dim pos As Vector3 = poly.Vtx(j)
                    Me.Content(i).Vtx(j) = pos + normal * offset
                Next

            Next
        Else
            Me.Children(0).ExtrudePoly(offset)
            Me.Children(1).ExtrudePoly(offset)
        End If
        UpdateRegion()
    End Sub

End Class