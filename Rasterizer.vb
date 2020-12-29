Imports SharpDX
Imports SharpDX.Direct3D11
Imports SharpDX.Direct2D1
Imports SharpDX.Mathematics.Interop
Imports SharpDX.D3DCompiler
Imports SharpDX.IO
Imports System.Drawing.Imaging

Module Rasterizer

    Public PBox_ImageCanvas As Bitmap1 = Nothing
    Public Spectator As SpectatorCamera

    Public GameImagingFactory As WIC.ImagingFactory = New WIC.ImagingFactory

    Public CPU_BITMAP_PROPERTY As BitmapProperties1 = New BitmapProperties1() With {
        .PixelFormat = New SharpDX.Direct2D1.PixelFormat(SharpDX.DXGI.Format.B8G8R8A8_UNorm, SharpDX.Direct2D1.AlphaMode.Premultiplied),
        .BitmapOptions = BitmapOptions.CannotDraw Or BitmapOptions.CpuRead}

    Public TARGET_BITMAP_PROPERTY As BitmapProperties1 = New BitmapProperties1() With {
        .PixelFormat = New SharpDX.Direct2D1.PixelFormat(SharpDX.DXGI.Format.B8G8R8A8_UNorm, SharpDX.Direct2D1.AlphaMode.Premultiplied),
        .BitmapOptions = BitmapOptions.Target}

    Public ImageResources As New Dictionary(Of String, RasterizerTextureResource)

    Public DefaultFont8 As New Font(New FontFamily("Microsoft Yahei"), 8)
    Public DefaultFont12 As New Font(New FontFamily("Microsoft Yahei"), 12)

    Public RasterizerRandomTexture_sys As System.Drawing.Bitmap
    Public RasterizerRandomTexture As Texture2D
    Public RasterizerRandomTextureSRV As ShaderResourceView

    Public Sub InitializeRasterizer()
        Spectator = New SpectatorCamera
        'AddHandler Spectator_Workspace.EveryFramePass, AddressOf PlayAnimation
        With Spectator
            .Resolve = New SharpDX.Vector2(800, 600)
            .CameraFocus = New SharpDX.Vector2(.Resolve.X / 2, .Resolve.Y / 2)
        End With
        Spectator.InitializeDirectComponents(Form1.PBox)
        InitializePBoxCanvas()
        Spectator.CurrentRasterizerCamera.CalculateWVP()

        'Dim r = Spectator.CurrentRasterizerCamera.ApplyWVP(New Vector3(0, 11.547, 0))
        'Console.WriteLine(r)
        'Dim r2 = Spectator.CurrentRasterizerCamera.ApplyWVP(New Vector3(0, 23.094, -20))
        'Console.WriteLine(r2)
        'Dim r3 = Spectator.CurrentRasterizerCamera.ApplyWVP(New Vector3(0, 34.641, -40))
        'Console.WriteLine(r3)

        LoadAllImageResources(Spectator.GetDeviceContext, Spectator.GetGlobalDevice)
        GenerateRandomTexture(Spectator.GetGlobalDevice, Spectator.GetDeviceContext)

        If Spectator.PaintingLayers.Count = 0 Then
            Spectator.PaintingLayers.Add(AddressOf Spectator.DrawLink3DImage)
        End If
    End Sub

    Public Sub RasterizerUpdateModels()
        Spectator.CurrentRasterizerCamera.ClearMesh()
        For Each model As Model In ModelRepository
            If ObjLoader_ma_apply Is Nothing Then
                Spectator.CurrentRasterizerCamera.LoadMesh(model, ObjLoader)
            Else
                Spectator.CurrentRasterizerCamera.LoadMesh(model, ObjLoader_ma_apply)
            End If

        Next
    End Sub

    Public Sub InitializePBoxCanvas()
        If PBox_ImageCanvas IsNot Nothing Then
            PBox_ImageCanvas.Dispose()
        End If
        PBox_ImageCanvas = New Bitmap1(Spectator.GetDeviceContext, New SharpDX.Size2(Spectator.Resolve.X, Spectator.Resolve.Y), TARGET_BITMAP_PROPERTY)
    End Sub


    ''' <summary>
    ''' read all images from the directory
    ''' </summary>
    ''' <param name="context">D2D_DeviceContext / RenderTarget</param>
    ''' <param name="device">D3D_Device</param>
    Public Sub LoadAllImageResources(context As Direct2D1.DeviceContext, device As Direct3D11.Device1)
        For Each mat As KeyValuePair(Of String, ModelMaterial) In ObjLoader.MatRepo
            Dim matName As String = mat.Value.Name
            If mat.Value.DiffuseTexture Is Nothing Then
                Continue For
            End If

            Dim filePath As String = mat.Value.DiffuseTexture.Path
            Dim tmpImage As New RasterizerTextureResource With {
                .Name = matName,
                .Path = filePath,
                .D2DImage = LoadBitmapUsingWIC(context, .Path),
                .D3DTexture = LoadTextureUsingWIC(device, .Path),
                .SysImage = New Drawing.Bitmap(.Path)}
            tmpImage.GenerateSRV(device)
            ImageResources(tmpImage.Name) = tmpImage
        Next
    End Sub

    ''' <summary>
    ''' reload all images after device/context changed. need renderingLock = 2
    ''' </summary>
    ''' <param name="context">D2D_DeviceContext / RenderTarget</param>
    ''' <param name="device">D3D_Device</param>
    Public Sub UpdateAllImageResources(context As Direct2D1.DeviceContext, device As Direct3D11.Device1)
        For Each tmpResource As RasterizerTextureResource In ImageResources.Values
            With tmpResource
                If .D2DImage IsNot Nothing Then .D2DImage.Dispose()
                .D2DImage = LoadBitmapUsingWIC(context, .Path)
                If .D3DTexture IsNot Nothing Then .D3DTexture.Dispose()
                .D3DTexture = LoadTextureUsingWIC(device, .Path)
                .GenerateSRV(device)
            End With
        Next
    End Sub

    ''' <summary>
    ''' convert system.drawing.bitmap to d2dbitmap
    ''' </summary>
    ''' <param name="rt">render target == d2dContext</param>
    ''' <param name="drawingBitmap">system.drawing.bitmap</param>
    Public Function LoadBitmapFromSysBitmap(rt As RenderTarget, drawingBitmap As System.Drawing.Bitmap) As Bitmap1
        Dim result As Bitmap1 = Nothing
        Dim drawingBitmapData As BitmapData = drawingBitmap.LockBits(New Drawing.Rectangle(0, 0, drawingBitmap.Width, drawingBitmap.Height),
                                                                     ImageLockMode.ReadOnly, Imaging.PixelFormat.Format32bppPArgb)
        Dim dataStreamxx As DataStream = New DataStream(drawingBitmapData.Scan0, drawingBitmapData.Stride * drawingBitmapData.Height, True, False)
        Dim properties As Direct2D1.BitmapProperties1 = New Direct2D1.BitmapProperties1()
        properties.PixelFormat = New Direct2D1.PixelFormat(DXGI.Format.B8G8R8A8_UNorm, Direct2D1.AlphaMode.Premultiplied)
        result = New Direct2D1.Bitmap1(rt, New Size2(drawingBitmap.Width, drawingBitmap.Height), dataStreamxx, drawingBitmapData.Stride, properties)
        drawingBitmap.UnlockBits(drawingBitmapData)
        dataStreamxx.Dispose()
        Return result
    End Function

    ''' <summary>
    ''' use WIC to load image resources
    ''' </summary>
    ''' <param name="context">d2dContext</param>
    ''' <param name="filePath">image path</param>
    Public Function LoadBitmapUsingWIC(context As Direct2D1.DeviceContext, filePath As String) As Bitmap1
        Dim fileStream As NativeFileStream = New NativeFileStream(filePath, NativeFileMode.Open, NativeFileAccess.Read)
        Dim bitmapDecoder As WIC.BitmapDecoder = New WIC.BitmapDecoder(GameImagingFactory, fileStream, WIC.DecodeOptions.CacheOnDemand)
        Dim frame As WIC.BitmapFrameDecode = bitmapDecoder.GetFrame(0)
        Dim converter As WIC.FormatConverter = New WIC.FormatConverter(GameImagingFactory)
        converter.Initialize(frame, SharpDX.WIC.PixelFormat.Format32bppPRGBA)
        Dim newBitmap As Bitmap1 = SharpDX.Direct2D1.Bitmap1.FromWicBitmap(context, converter)
        fileStream.Dispose()
        bitmapDecoder.Dispose()
        frame.Dispose()
        converter.Dispose()
        Return newBitmap
    End Function

    Public Function LoadTextureUsingWIC(d3dDevice As Direct3D11.Device, filePath As String) As Direct3D11.Texture2D
        Dim fileStream As NativeFileStream = New NativeFileStream(filePath, NativeFileMode.Open, NativeFileAccess.Read)
        Dim bitmapDecoder As WIC.BitmapDecoder = New WIC.BitmapDecoder(GameImagingFactory, fileStream, WIC.DecodeOptions.CacheOnDemand)
        Dim frame As WIC.BitmapFrameDecode = bitmapDecoder.GetFrame(0)
        Dim converter As WIC.FormatConverter = New WIC.FormatConverter(GameImagingFactory)
        converter.Initialize(frame, SharpDX.WIC.PixelFormat.Format32bppPRGBA)
        ' converter is 'BitmapSource'

        Dim result As Direct3D11.Texture2D = Nothing
        Dim stride As Integer = converter.Size.Width * 4
        Dim buffer As DataStream = New DataStream(converter.Size.Height * stride, True, True)
        converter.CopyPixels(stride, buffer)
        Dim description As New Direct3D11.Texture2DDescription() With {
            .Width = converter.Size.Width,
            .Height = converter.Size.Height,
            .ArraySize = 1,
            .BindFlags = SharpDX.Direct3D11.BindFlags.ShaderResource Or Direct3D11.BindFlags.RenderTarget,
            .Usage = SharpDX.Direct3D11.ResourceUsage.Default,
            .CpuAccessFlags = SharpDX.Direct3D11.CpuAccessFlags.None,
            .Format = SharpDX.DXGI.Format.R8G8B8A8_UNorm,
            .MipLevels = 1,
            .OptionFlags = Direct3D11.ResourceOptionFlags.GenerateMipMaps, 'ResourceOptionFlags.GenerateMipMap
            .SampleDescription = New SharpDX.DXGI.SampleDescription(1, 0)}
        result = New Direct3D11.Texture2D(d3dDevice, description, New SharpDX.DataRectangle(buffer.DataPointer, stride))
        buffer.Close()
        buffer.Dispose()
        Return result
    End Function

    ''' <summary>
    ''' use WIC to load image resources
    ''' </summary>
    ''' <param name="context">d2dContext</param>
    ''' <param name="readStream">image stream</param>
    Public Function LoadBitmapUsingWIC(context As Direct2D1.DeviceContext, readStream As System.IO.Stream) As Bitmap1
        Dim bitmapDecoder As WIC.BitmapDecoder = New WIC.BitmapDecoder(GameImagingFactory, readStream, WIC.DecodeOptions.CacheOnDemand)
        Dim frame As WIC.BitmapFrameDecode = bitmapDecoder.GetFrame(0)
        Dim Converter As WIC.FormatConverter = New WIC.FormatConverter(GameImagingFactory)
        Converter.Initialize(frame, SharpDX.WIC.PixelFormat.Format32bppPRGBA)
        Dim newBitmap As Bitmap1 = SharpDX.Direct2D1.Bitmap1.FromWicBitmap(context, Converter)
        bitmapDecoder.Dispose()
        frame.Dispose()
        Converter.Dispose()
        Return newBitmap
    End Function

    ''' <summary>
    ''' convert bitmap1 to system.drawing.bitmap
    ''' </summary>
    ''' <param name="context">d2dContext</param>
    ''' <param name="image">direct2d bitmap1</param>
    Public Function GetSysBitmapFromBitmap1(context As Direct2D1.DeviceContext, image As Bitmap1) As System.Drawing.Bitmap
        Dim tmpH As Integer = CInt(image.Size.Height)
        Dim tmpW As Integer = CInt(image.Size.Width)
        Dim tmpBitmap As Bitmap1 = New Bitmap1(context, New Size2(tmpW, tmpH), CPU_BITMAP_PROPERTY)
        tmpBitmap.CopyFromBitmap(image)
        Dim result As New System.Drawing.Bitmap(tmpW, tmpH)
        Dim offset As Integer = 0

        Dim data As DataRectangle = tmpBitmap.Map(MapOptions.Read)
        Dim pointer As IntPtr = data.DataPointer
        For j = 0 To tmpH - 1
            For i = 0 To tmpW - 1
                Dim B As Byte = Runtime.InteropServices.Marshal.ReadByte(pointer, offset)
                offset += 1
                Dim G As Byte = Runtime.InteropServices.Marshal.ReadByte(pointer, offset)
                offset += 1
                Dim R As Byte = Runtime.InteropServices.Marshal.ReadByte(pointer, offset)
                offset += 1
                Dim A As Byte = Runtime.InteropServices.Marshal.ReadByte(pointer, offset)
                offset += 1
                'Dim BGRA As Integer = Runtime.InteropServices.Marshal.ReadInt32(pointer, offset)
                'offset += 4
                result.SetPixel(i, j, System.Drawing.Color.FromArgb(A, R, G, B))
            Next
        Next

        tmpBitmap.Unmap()
        tmpBitmap.Dispose()
        Return result
    End Function

    Public Function GetTextureFromBitmap1(d3dDevice As Direct3D11.Device, context As Direct2D1.DeviceContext, image As Bitmap1) As Direct3D11.Texture2D
        Dim tmpH As Integer = CInt(image.Size.Height)
        Dim tmpW As Integer = CInt(image.Size.Width)
        Dim tmpBitmap As Bitmap1 = New Bitmap1(context, New Size2(tmpW, tmpH), CPU_BITMAP_PROPERTY)
        tmpBitmap.CopyFromBitmap(image)
        Dim dataRect As DataRectangle = tmpBitmap.Map(MapOptions.Read)

        Dim desc As New Direct3D11.Texture2DDescription() With {
            .Width = tmpW,
            .Height = tmpH,
            .ArraySize = 1,
            .BindFlags = SharpDX.Direct3D11.BindFlags.ShaderResource Or Direct3D11.BindFlags.RenderTarget,
            .Usage = SharpDX.Direct3D11.ResourceUsage.Default,
            .CpuAccessFlags = SharpDX.Direct3D11.CpuAccessFlags.None,
            .Format = SharpDX.DXGI.Format.R8G8B8A8_UNorm,
            .MipLevels = 1,
            .OptionFlags = Direct3D11.ResourceOptionFlags.GenerateMipMaps,
            .SampleDescription = New SharpDX.DXGI.SampleDescription(1, 0)}
        Dim result As Texture2D = New Texture2D(d3dDevice, desc, dataRect)

        tmpBitmap.Unmap()
        tmpBitmap.Dispose()

        Return result
    End Function

    Public Sub GenerateRandomTexture(d3dDevice As Direct3D11.Device, context As Direct2D1.DeviceContext)
        If RasterizerRandomTexture_sys IsNot Nothing Then RasterizerRandomTexture_sys.Dispose()
        RasterizerRandomTexture_sys = New Drawing.Bitmap(ImageWidth, ImageHeight)
        Dim rand As New Random
        For j = 0 To ImageHeight - 1
            For i = 0 To ImageWidth - 1
                Dim r As Integer = Math.Floor(256 * rand.NextDouble)
                Dim g As Integer = Math.Floor(256 * rand.NextDouble)
                Dim b As Integer = Math.Floor(256 * rand.NextDouble)
                RasterizerRandomTexture_sys.SetPixel(i, j, Drawing.Color.FromArgb(255, r, g, b))
            Next
        Next

        Dim d2dBitmap As Bitmap1 = LoadBitmapFromSysBitmap(context, RasterizerRandomTexture_sys)
        RasterizerRandomTexture = GetTextureFromBitmap1(d3dDevice, context, d2dBitmap)
        d2dBitmap.Dispose()

        Dim srvd As New Direct3D11.ShaderResourceViewDescription With {
                .Format = RasterizerRandomTexture.Description.Format,
                .Dimension = SharpDX.Direct3D.ShaderResourceViewDimension.Texture2D}
        srvd.Texture2D.MostDetailedMip = 0
        srvd.Texture2D.MipLevels = -1
        If RasterizerRandomTextureSRV IsNot Nothing Then RasterizerRandomTextureSRV.Dispose()
        RasterizerRandomTextureSRV = New Direct3D11.ShaderResourceView(d3dDevice, RasterizerRandomTexture, srvd)
        d3dDevice.ImmediateContext.GenerateMips(RasterizerRandomTextureSRV)
    End Sub

    Public Function GetDistance(a As Drawing.Point, b As Drawing.Point) As Single
        Return Math.Sqrt((a.X - b.X) ^ 2 + (a.Y - b.Y) ^ 2)
    End Function
    Public Function GetDistance(a As Mathematics.Interop.RawVector2, b As PointF) As Single
        Return Math.Sqrt((a.X - b.X) ^ 2 + (a.Y - b.Y) ^ 2)
    End Function
    Public Function GetDistance(a As Drawing.Color, b As Drawing.Color) As Single
        Return Math.Sqrt((CSng(a.R) - CSng(b.R)) ^ 2 + (CSng(a.G) - CSng(b.G)) ^ 2 + (CSng(a.B) - CSng(b.B)) ^ 2)
    End Function
    Public Function GetDistance(a As Mathematics.Interop.RawVector2, b As Mathematics.Interop.RawVector4) As Single
        Dim disPoint As PointF
        If b.Z - b.X = 0 Then
            disPoint = New PointF(b.X, a.Y)
            If (disPoint.Y >= b.Y And disPoint.Y <= b.W) OrElse (disPoint.Y <= b.Y And disPoint.Y >= b.W) Then
                Return GetDistance(a, disPoint)
            Else
                Dim dis1 As Single = GetDistance(a, New PointF(b.X, b.Y))
                Dim dis2 As Single = GetDistance(a, New PointF(b.Z, b.W))
                Return {dis1, dis2}.Min
            End If
        Else
            Dim k As Single = (b.W - b.Y) / (b.Z - b.X)
            Dim k1 As Single = 1.0 / k
            Dim x As Single = (k * b.X + k1 * a.X + a.Y - b.Y) / (k1 + k)
            Dim y = k * (x - b.X) + b.Y
            disPoint = New PointF(x, y)
            If (disPoint.X >= b.X And disPoint.X <= b.Z) OrElse (disPoint.X <= b.X And disPoint.X >= b.Z) Then
                Return GetDistance(a, disPoint)
            Else
                Dim dis1 As Single = GetDistance(a, New PointF(b.X, b.Y))
                Dim dis2 As Single = GetDistance(a, New PointF(b.Z, b.W))
                Return {dis1, dis2}.Min
            End If
        End If
        Return -1
    End Function



End Module

Public Class RasterizerTextureResource

    Public Name As String = ""
    Public Path As String = ""
    Public D2DImage As Bitmap1 = Nothing
    Public SysImage As System.Drawing.Bitmap = Nothing
    Public D3DTexture As Direct3D11.Texture2D = Nothing

    Public D3DShaderResourceView As Direct3D11.ShaderResourceView = Nothing

    Public Sub GenerateSRV(d3dDevice As Direct3D11.Device)
        If Me.D3DShaderResourceView IsNot Nothing Then
            D3DShaderResourceView.Dispose()
        End If
        Dim srvd As New Direct3D11.ShaderResourceViewDescription With {
            .Format = D3DTexture.Description.Format,
            .Dimension = SharpDX.Direct3D.ShaderResourceViewDimension.Texture2D}
        srvd.Texture2D.MostDetailedMip = 0
        srvd.Texture2D.MipLevels = -1

        D3DShaderResourceView = New Direct3D11.ShaderResourceView(d3dDevice, D3DTexture, srvd)
        d3dDevice.ImmediateContext.GenerateMips(D3DShaderResourceView)
    End Sub

End Class

''' <summary>
''' 2d camera class in 3d space
''' </summary>
Public Class RasterizerCamera

    Public FOV As Single = 60 * Math.PI / 180

    Public Scale As Vector2 = Vector2.One

    Public F_Near As Single = 1.0F

    Public F_Far As Single = 100.0F


    Public View_Position As Matrix
    Public View_RX As Matrix
    Public View_RY As Matrix
    Public View_RZ As Matrix
    Public Mat_View As Matrix
    Public Mat_Projection As Matrix
    ''' <summary>
    ''' 提供给GPU的WVP变换矩阵
    ''' </summary>
    Public WVP As SharpDX.Mathematics.Interop.RawMatrix
    Public WVP_Inv As Matrix

    Public HalfResolve As Vector2

    Public DefaultInputElement As Direct3D11.InputElement()
    Public DefaultInputSignature As ShaderSignature
    Public DefaultInputLayout As InputLayout
    Public DefaultVertexShader As Direct3D11.VertexShader
    Public DefaultPixelShader As Direct3D11.PixelShader
    Public DefaultEffect As Direct3D11.Effect
    Public DefaultConstantBuffer As Direct3D11.Buffer
    Public DefaultSamplerState As Direct3D11.SamplerState

    Public Container_Solid As New List(Of ModelPolyVBundle)
    Public Container_Transparent As New List(Of ModelPolyVBundle)

    Public ContainerSyncLock As New Object

    Public Sub LoadShader(shaderPath As String, d3dDevice As Direct3D11.Device1, context As Direct3D11.DeviceContext1)
        'set input element
        Dim ie() As Direct3D11.InputElement = {
            New Direct3D11.InputElement("POSITION", 0, DXGI.Format.R32G32B32_Float, 0, 0),
            New Direct3D11.InputElement("NORMAL", 0, DXGI.Format.R32G32B32_Float, 12, 0),
            New Direct3D11.InputElement("TEXCOORD", 0, DXGI.Format.R32G32_Float, 24, 0)}
        DefaultInputElement = ie

        'set input signature and load vertex shaders, pixel shader
        Using bytecode As ShaderBytecode = ShaderBytecode.CompileFromFile(shaderPath, "VShader", "vs_4_0", ShaderFlags.None, EffectFlags.None)
            DefaultInputSignature = ShaderSignature.GetInputSignature(bytecode)
            DefaultVertexShader = New VertexShader(d3dDevice, bytecode)
        End Using
        'Using bytecode As ShaderBytecode = ShaderBytecode.CompileFromFile(shaderPath, "PShader", "ps_4_0", ShaderFlags.None, EffectFlags.None)
        '    DefaultPixelShader = New PixelShader(d3dDevice, bytecode)
        'End Using
        Using bytecode As ShaderBytecode = ShaderBytecode.CompileFromFile(shaderPath, "fx_5_0", ShaderFlags.None, EffectFlags.None)
            DefaultEffect = New Direct3D11.Effect(d3dDevice, bytecode)
        End Using

        DefaultInputLayout = New InputLayout(d3dDevice, DefaultInputSignature.Data, DefaultInputElement)

        Dim constantBufferDesc As BufferDescription = New BufferDescription() With {
                    .Usage = ResourceUsage.Dynamic,
                    .SizeInBytes = 128,
                    .BindFlags = BindFlags.ConstantBuffer,
                    .CpuAccessFlags = CpuAccessFlags.Write,
                    .OptionFlags = ResourceOptionFlags.None,
                    .StructureByteStride = 0
                }
        DefaultConstantBuffer = New SharpDX.Direct3D11.Buffer(d3dDevice, constantBufferDesc)

        '----------optional: 可以直接写在shader里-------------------
        Dim samplerStateDesc As SamplerStateDescription = New SamplerStateDescription() With {
                    .Filter = Direct3D11.Filter.MinMagMipLinear,
                    .AddressU = TextureAddressMode.Wrap,
                    .AddressV = TextureAddressMode.Wrap,
                    .AddressW = TextureAddressMode.Wrap,
                    .MipLodBias = 0,
                    .MaximumAnisotropy = 1,
                    .ComparisonFunction = Comparison.Always,
                    .BorderColor = New Color4(0, 0, 0, 0),
                    .MinimumLod = 0,
                    .MaximumLod = Single.MaxValue}
        DefaultSamplerState = New SamplerState(d3dDevice, samplerStateDesc)


    End Sub

    Public Sub CalculateViewP()
        View_Position = Matrix.Identity
        With View_Position
            .M14 = -CameraPosition.X
            .M24 = -CameraPosition.Y
            .M34 = -CameraPosition.Z
        End With
    End Sub
    Public Sub CalculateViewRX()
        View_RX = Matrix.Identity
        With View_RX
            .M22 = Math.Cos(CameraRotation.X)
            .M32 = Math.Sin(CameraRotation.X)
            .M23 = - .M32
            .M33 = .M22
        End With
    End Sub
    Public Sub CalculateViewRY()
        View_RY = Matrix.Identity
        With View_RY
            .M11 = Math.Cos(CameraRotation.Y)
            .M13 = Math.Sin(CameraRotation.Y)
            .M31 = - .M13
            .M33 = .M11
        End With
    End Sub
    Public Sub CalculateViewRZ()
        View_RZ = Matrix.Identity
        With View_RZ
            .M11 = Math.Cos(CameraRotation.Z)
            .M21 = Math.Sin(CameraRotation.Z)
            .M12 = - .M21
            .M22 = .M11
        End With
    End Sub
    Public Sub RefreshProjection()
        Mat_Projection = Matrix.Identity
        With Mat_Projection
            .M11 = 1 / Math.Tan(FOV / 2)
            .M22 = 1 * (ImageWidth * 1.0 / ImageHeight) / Math.Tan(FOV / 2)
            .M33 = -((F_Near + F_Far) / (F_Far - F_Near))
            .M44 = 0
            .M43 = -1
            .M34 = -(2 * F_Far * F_Near / (F_Far - F_Near))
        End With
    End Sub
    Public Sub CalculateWVP()
        CalculateViewP()
        CalculateViewRX()
        CalculateViewRY()
        CalculateViewRZ()
        RefreshProjection()
        Mat_View = View_RZ * (View_RY * (View_RX * View_Position))
        WVP = Mat_Projection * Mat_View
    End Sub

    Public Shared Function CalculateWVP_LookAt(from As Vector3, target As Vector3) As Matrix
        Dim VRP As Vector3 = from
        Dim TP As Vector3 = target
        Dim VPN As Vector3 = -(TP - VRP)
        Dim VUV As Vector3 = Vector3.Up
        Dim n As Vector3 = Vector3.Normalize(VPN)
        Dim u As Vector3 = -Vector3.Normalize(Vector3.Cross(n, VUV))
        Dim v As Vector3 = -Vector3.Cross(u, n)
        Dim t As Vector3 = -New Vector3(Vector3.Dot(VRP, u), Vector3.Dot(VRP, v), Vector3.Dot(VRP, n))
        Dim viewMat As Matrix = New Matrix(u.X, v.X, n.X, 0,
                                              u.Y, v.Y, n.Y, 0,
                                              u.Z, v.Z, n.Z, 0,
                                              t.X, t.Y, t.Z, 1)
        viewMat.Transpose()
        Dim projMat As Matrix = Matrix.PerspectiveFovRH(Math.PI * 0.333, 1.333, 1, 50)
        Return projMat * viewMat
    End Function

    Public Function ApplyWVP(input As Vector3) As Vector2
        Dim inputMat As Matrix = New Matrix(0)
        With inputMat
            .M11 = input.X
            .M21 = input.Y
            .M31 = input.Z
            .M41 = 1
        End With
        Dim resultMat As Matrix = WVP * inputMat

        Return New Vector2((1.0 + resultMat.M11) * HalfResolve.X, (1.0 - resultMat.M21) * HalfResolve.Y)
    End Function
    Public Function ApplyWVP(input As Vector2) As Vector2
        Dim inputMat As Matrix = New Matrix(0)
        With inputMat
            .M11 = input.X
            .M21 = input.Y
            .M31 = 1
            .M41 = 1
        End With
        Dim resultMat As Matrix = WVP * inputMat

        Return New Vector2((1.0 + resultMat.M11) * HalfResolve.X, (1.0 - resultMat.M21) * HalfResolve.Y)
    End Function
    Public Function ApplyWVPInverse(input As RawVector2) As RawVector2
        Dim inputMat As Matrix = New Matrix(0)
        With inputMat
            .M11 = input.X
            .M21 = input.Y
            .M31 = 1
            .M41 = 1
        End With
        Dim resultMat As Matrix = WVP_Inv * inputMat
        Return New RawVector2(resultMat.M11 / HalfResolve.X, resultMat.M21 / HalfResolve.Y)
    End Function

    Public Sub LoadMesh(modelIn As Model, loader As ObjFileManager)
        SyncLock ContainerSyncLock
            Dim bundle As New ModelPolyVBundle
            bundle.TextureIndex = modelIn.Mesh(0).MaterialName
            For Each tmpPoly As ModelPoly In modelIn.Mesh.Values
                Dim convertPoly As New ModelPolyV
                With convertPoly
                    .MaterialName = tmpPoly.MaterialName
                    ReDim .Vtx(2)
                    .Vtx(0) = loader.VtxRepo(tmpPoly.VtxIdx(0))
                    .Vtx(1) = loader.VtxRepo(tmpPoly.VtxIdx(1))
                    .Vtx(2) = loader.VtxRepo(tmpPoly.VtxIdx(2))
                    ReDim .Normal(2)
                    .Normal(0) = loader.NormalRepo(tmpPoly.NormalIdx(0))
                    .Normal(1) = loader.NormalRepo(tmpPoly.NormalIdx(1))
                    .Normal(2) = loader.NormalRepo(tmpPoly.NormalIdx(2))
                    If tmpPoly.TexCoordIdx.Length > 0 Then
                        ReDim .TexCoord(2)
                        .TexCoord(0) = loader.TexCoordRepo(tmpPoly.TexCoordIdx(0))
                        .TexCoord(1) = loader.TexCoordRepo(tmpPoly.TexCoordIdx(1))
                        .TexCoord(2) = loader.TexCoordRepo(tmpPoly.TexCoordIdx(2))
                    Else
                        .TexCoord = {Numerics.Vector2.Zero, Numerics.Vector2.Zero, Numerics.Vector2.Zero}
                    End If
                End With
                bundle.Faces.Add(convertPoly)
            Next
            bundle.RefreshBuffer()

            If loader.MatRepo(modelIn.Mesh(0).MaterialName).MarkAsTransparent Then
                Me.Container_Transparent.Add(bundle)
            Else
                Me.Container_Solid.Add(bundle)
            End If
        End SyncLock
    End Sub

    Public Sub LoadMesh(polyIn As ModelPolyV)
        SyncLock ContainerSyncLock
            ' TODO

        End SyncLock
    End Sub

    Public Sub ClearMesh()
        SyncLock ContainerSyncLock
            For Each bundle As ModelPolyVBundle In Me.Container_Solid
                bundle.Dispose()
            Next
            Me.Container_Solid.Clear()
            For Each bundle As ModelPolyVBundle In Me.Container_Transparent
                bundle.Dispose()
            Next
            Me.Container_Transparent.Clear()
        End SyncLock
    End Sub

    Public Sub DrawSSAO1(d3dDevice As Direct3D11.Device1, context As Direct3D11.DeviceContext1)
        context.OutputMerger.SetTargets(Spectator.GetDepthBufffer, Spectator.GetRenderTargetView)
        SyncLock ContainerSyncLock
            Dim solid_trans As List(Of ModelPolyVBundle)() = {Container_Solid, Container_Transparent}
            For Each render_order As List(Of ModelPolyVBundle) In solid_trans
                For Each targetBundle As ModelPolyVBundle In render_order
                    Dim textureIndex As String = targetBundle.TextureIndex
                    Dim vertexBuffer As New Buffer(d3dDevice, targetBundle.Buffer, targetBundle.Buffer.Length, ResourceUsage.Default, BindFlags.VertexBuffer, CpuAccessFlags.None, ResourceOptionFlags.None, 0)

                    With context
                        .InputAssembler.InputLayout = DefaultInputLayout
                        .InputAssembler.PrimitiveTopology = Direct3D.PrimitiveTopology.TriangleList
                        .InputAssembler.SetVertexBuffers(0, New VertexBufferBinding(vertexBuffer, 32, 0))
                        With DefaultEffect
                            .GetVariableByName("MatWVP").AsMatrix().SetMatrix(Me.WVP)
                            .GetTechniqueByIndex(0).GetPassByIndex(0).Apply(context)
                        End With
                        'render
                        .Draw(targetBundle.Faces.Count * 3, 0)
                    End With

                    vertexBuffer.Dispose()
                Next
            Next
        End SyncLock
        context.OutputMerger.ResetTargets()
    End Sub
    Public Sub DrawSSAO2(d3dDevice As Direct3D11.Device1, context As Direct3D11.DeviceContext1)
        Dim lightPos As Vector3 = New Vector3(10, 40, 10)

        context.OutputMerger.SetTargets(Spectator.GetDepthBufffer, Spectator.GetRenderTargetView)
        SyncLock ContainerSyncLock
            Dim solid_trans As List(Of ModelPolyVBundle)() = {Container_Solid, Container_Transparent}
            ' ssao2
            For Each render_order As List(Of ModelPolyVBundle) In solid_trans
                For Each targetBundle As ModelPolyVBundle In render_order
                    Dim textureIndex As String = targetBundle.TextureIndex
                    Dim vertexBuffer As New Buffer(d3dDevice, targetBundle.Buffer, targetBundle.Buffer.Length, ResourceUsage.Default, BindFlags.VertexBuffer, CpuAccessFlags.None, ResourceOptionFlags.None, 0)

                    With context
                        .InputAssembler.InputLayout = DefaultInputLayout
                        .InputAssembler.PrimitiveTopology = Direct3D.PrimitiveTopology.TriangleList
                        .InputAssembler.SetVertexBuffers(0, New VertexBufferBinding(vertexBuffer, 32, 0))
                        With DefaultEffect
                            .GetVariableByName("MatWVP").AsMatrix().SetMatrix(Me.WVP)
                            .GetVariableByName("ShaderTexture").AsShaderResource.SetResource(ImageResources(textureIndex).D3DShaderResourceView)
                            .GetVariableByName("DepthTexture").AsShaderResource.SetResource(Spectator.D3DSSAOFirstRoundSRV)
                            .GetVariableByName("RandomTexture").AsShaderResource.SetResource(RasterizerRandomTextureSRV)
                            .GetVariableByName("LightPos").AsVector().Set(lightPos)

                            .GetTechniqueByIndex(0).GetPassByIndex(1).Apply(context)
                        End With
                        'render
                        .Draw(targetBundle.Faces.Count * 3, 0)
                    End With

                    vertexBuffer.Dispose()
                Next
            Next
            ' edge
            For Each render_order As List(Of ModelPolyVBundle) In solid_trans
                For Each targetBundle As ModelPolyVBundle In render_order
                    Dim textureIndex As String = targetBundle.TextureIndex
                    Dim vertexBuffer As New Buffer(d3dDevice, targetBundle.Buffer, targetBundle.Buffer.Length, ResourceUsage.Default, BindFlags.VertexBuffer, CpuAccessFlags.None, ResourceOptionFlags.None, 0)

                    With context
                        .InputAssembler.InputLayout = DefaultInputLayout
                        .InputAssembler.PrimitiveTopology = Direct3D.PrimitiveTopology.TriangleList
                        .InputAssembler.SetVertexBuffers(0, New VertexBufferBinding(vertexBuffer, 32, 0))
                        With DefaultEffect
                            .GetVariableByName("MatWVP").AsMatrix().SetMatrix(Me.WVP)
                            .GetVariableByName("ShaderTexture").AsShaderResource.SetResource(ImageResources(textureIndex).D3DShaderResourceView)
                            .GetVariableByName("CameraPos").AsVector().Set(New RawVector3(CameraPosition.X, CameraPosition.Y, CameraPosition.Z))

                            .GetTechniqueByIndex(0).GetPassByIndex(2).Apply(context)
                        End With
                        'render
                        .Draw(targetBundle.Faces.Count * 3, 0)
                    End With

                    vertexBuffer.Dispose()
                Next
            Next
        End SyncLock
        context.OutputMerger.ResetTargets()
    End Sub

    Public Sub DrawShadowMap(d3dDevice As Direct3D11.Device1, context As Direct3D11.DeviceContext1)
        Dim lightPos As Vector3 = New Vector3(10, 40, 10)
        Dim targetPos As Vector3 = New Vector3(0, 30, 0)
        Dim lightWVP As Matrix = CalculateWVP_LookAt(lightPos, targetPos)

        SyncLock ContainerSyncLock

            Dim solid_trans As List(Of ModelPolyVBundle)() = {Container_Solid, Container_Transparent}
            For Each render_order As List(Of ModelPolyVBundle) In solid_trans
                For Each targetBundle As ModelPolyVBundle In render_order
                    Dim textureIndex As String = targetBundle.TextureIndex
                    Dim vertexBuffer As New Buffer(d3dDevice, targetBundle.Buffer, targetBundle.Buffer.Length, ResourceUsage.Default, BindFlags.VertexBuffer, CpuAccessFlags.None, ResourceOptionFlags.None, 0)

                    With context
                        .InputAssembler.InputLayout = DefaultInputLayout
                        .InputAssembler.PrimitiveTopology = Direct3D.PrimitiveTopology.TriangleList
                        .InputAssembler.SetVertexBuffers(0, New VertexBufferBinding(vertexBuffer, 32, 0))
                        With DefaultEffect
                            .GetVariableByName("LightWVP").AsMatrix().SetMatrix(lightWVP)
                            .GetTechniqueByIndex(0).GetPassByIndex(0).Apply(context)
                        End With
                        'render
                        .Draw(targetBundle.Faces.Count * 3, 0)
                    End With

                    vertexBuffer.Dispose()
                Next
            Next

        End SyncLock

        context.OutputMerger.ResetTargets()    ' unbind the depth buffer and the canvas

    End Sub

    ''' <summary>
    ''' 绘制容器内的三维面，使用D3D11
    ''' </summary>
    Public Sub DrawContainer3D(d3dDevice As Direct3D11.Device1, context As Direct3D11.DeviceContext1)

        Dim lightPos As Vector3 = New Vector3(10, 40, 10)
        Dim targetPos As Vector3 = New Vector3(0, 30, 0)
        Dim lightWVP As Matrix = CalculateWVP_LookAt(lightPos, targetPos)

        context.OutputMerger.SetTargets(Spectator.GetDepthBufffer, Spectator.GetRenderTargetView)

        SyncLock ContainerSyncLock

            'Draw d3d11
            Dim solid_trans As List(Of ModelPolyVBundle)() = {Container_Solid, Container_Transparent}
            For Each render_order As List(Of ModelPolyVBundle) In solid_trans
                For Each targetBundle As ModelPolyVBundle In render_order
                    Dim textureIndex As String = targetBundle.TextureIndex
                    Dim vertexBuffer As New Buffer(d3dDevice, targetBundle.Buffer, targetBundle.Buffer.Length, ResourceUsage.Default, BindFlags.VertexBuffer, CpuAccessFlags.None, ResourceOptionFlags.None, 0)

                    With context

                        'set input format
                        .InputAssembler.InputLayout = DefaultInputLayout
                        .InputAssembler.PrimitiveTopology = Direct3D.PrimitiveTopology.TriangleList
                        .InputAssembler.SetVertexBuffers(0, New VertexBufferBinding(vertexBuffer, 32, 0))
                        With DefaultEffect
                            .GetVariableByName("MatWVP").AsMatrix().SetMatrix(Me.WVP)
                            .GetVariableByName("CameraPos").AsVector().Set(New RawVector3(CameraPosition.X, CameraPosition.Y, CameraPosition.Z))
                            .GetVariableByName("ShaderTexture").AsShaderResource.SetResource(ImageResources(textureIndex).D3DShaderResourceView)
                            .GetVariableByName("ShadowTexture").AsShaderResource.SetResource(Spectator.D3DShadowMapSRV)
                            .GetVariableByName("LightPos").AsVector().Set(lightPos)
                            .GetVariableByName("LightWVP").AsMatrix().SetMatrix(lightWVP)
                            '-------- write const input here ------------------

                            .GetTechniqueByIndex(0).GetPassByIndex(1).Apply(context)
                        End With
                        'render
                        .Draw(targetBundle.Faces.Count * 3, 0)

                        '===================================
                        .InputAssembler.SetVertexBuffers(0, New VertexBufferBinding(vertexBuffer, 32, 0))
                        With DefaultEffect
                            .GetVariableByName("MatWVP").AsMatrix().SetMatrix(Me.WVP)
                            .GetVariableByName("CameraPos").AsVector().Set(New RawVector3(CameraPosition.X, CameraPosition.Y, CameraPosition.Z))
                            .GetVariableByName("ShaderTexture").AsShaderResource.SetResource(ImageResources(textureIndex).D3DShaderResourceView)
                            .GetTechniqueByIndex(0).GetPassByIndex(2).Apply(context)
                        End With
                        .Draw(targetBundle.Faces.Count * 3, 0)
                    End With

                    vertexBuffer.Dispose()

                Next
            Next



        End SyncLock

    End Sub

End Class

Public Class ModelPolyVBundle

    Public TextureIndex As String = ""

    Public Faces As New List(Of ModelPolyV)

    Public Buffer As SharpDX.DataStream = Nothing

    Public Sub RefreshBuffer()
        If Me.Buffer IsNot Nothing Then Me.Buffer.Dispose()

        Dim structLength As Integer = 32

        Me.Buffer = New DataStream(structLength * 3 * Me.Faces.Count, True, True)
        With Me.Buffer
            For Each tmpFace As ModelPolyV In Me.Faces
                .Write(tmpFace.Vtx(0))
                .Write(tmpFace.Normal(0))
                .Write(tmpFace.TexCoord(0))

                .Write(tmpFace.Vtx(1))
                .Write(tmpFace.Normal(1))
                .Write(tmpFace.TexCoord(1))

                .Write(tmpFace.Vtx(2))
                .Write(tmpFace.Normal(2))
                .Write(tmpFace.TexCoord(2))
            Next
            .Position = 0
        End With

    End Sub

    Public Sub Dispose()
        If Buffer IsNot Nothing Then Buffer.Dispose()
    End Sub

End Class


''' <summary>
''' spectator
''' </summary>
Public Class SpectatorCamera
    Implements IDisposable
    ''' <summary>
    ''' centre of camera
    ''' </summary>
    Public CameraFocus As Vector2
    Public ReadOnly Property CameraTopLeft As Vector2
        Get
            Return New Vector2(CameraFocus.X - 0.5 * Resolve.X / Zoom, CameraFocus.Y - 0.5 * Resolve.Y / Zoom)
        End Get
    End Property
    ''' <summary>
    ''' scale. from 0 to 1
    ''' </summary>
    Public Zoom As Single = 1.0F
    ''' <summary>
    ''' camera resolution
    ''' </summary>
    Public Resolve As Vector2 = Nothing
    Public ReadOnly Property ResolveRectangle As Mathematics.Interop.RawRectangleF
        Get
            Return New Mathematics.Interop.RawRectangleF(0, 0, Me.Resolve.X, Me.Resolve.Y)
        End Get
    End Property
    ''' <summary>
    ''' 定义绘图委托，仅用于D2D
    ''' </summary>
    Public Delegate Sub DDraw(context As SharpDX.Direct2D1.DeviceContext, spectator As SpectatorCamera, canvasBitmap As Bitmap1)
    ''' <summary>
    ''' 分层绘图，不考虑小窗口穿透的模式，仅用于D2D
    ''' </summary>
    Public PaintingLayers As New List(Of DDraw)

    ''' <summary>
    ''' 当前鼠标位置
    ''' </summary>
    Public CurrentCursorX As Integer = 0
    Public CurrentCursorY As Integer = 0

    Private GlobalDevice As SharpDX.Direct3D11.Device1
    Private D3DContext As SharpDX.Direct3D11.DeviceContext1
    Private D3DRenderTargetView As SharpDX.Direct3D11.RenderTargetView
    Private D3DDepthStencilView As Direct3D11.DepthStencilView
    Private GlobalSwapChain As DXGI.SwapChain1
    Public D3DDepthStencilResource As Texture2D
    Public D3DDepthStencilSRV As ShaderResourceView

    ''' <summary>
    ''' Direct2D 1.1 画布对象
    ''' </summary>
    Private D2DContext As SharpDX.Direct2D1.DeviceContext
    ''' <summary>
    ''' 用于Direct2d显示的画布
    ''' </summary>
    Private D2DTarget As SharpDX.Direct2D1.Bitmap1
    ''' <summary>
    ''' D2D空白背景画布
    ''' </summary>
    Private BitmapBackgroundCanvas As Bitmap1
    ''' <summary>
    ''' D3D图像
    ''' </summary>
    Public D3DRenderImage As Bitmap1

    ''' <summary>
    ''' 光栅化渲染时，从光源位置的图像，用来做阴影贴图
    ''' </summary>
    Public D3DShadowMapImage As Bitmap1
    Public D3DShadowMapSRV As ShaderResourceView
    Public D3DSSAOFirstRoundImage As Bitmap1
    Public D3DSSAOFirstRoundSRV As ShaderResourceView

    Public CurrentRasterizerCamera As New RasterizerCamera

    Public Camera3DPixelInfo(0) As Integer

    Public RenderingSyncLock As New Object
    Public Event EveryFramePass()


    Public Sub InitializeDirectComponents(canvas As Control)
        Dim handle As IntPtr = canvas.Handle
        '-------setup d3d11--------
        'create device and swapchain
        Dim tmpGlobalDevice As Direct3D11.Device
        Dim tmpGlobalSwapChain As DXGI.SwapChain
        Dim sc_description As New SharpDX.DXGI.SwapChainDescription
        With sc_description
            .BufferCount = 1
            .ModeDescription = New DXGI.ModeDescription(Resolve.X, Resolve.Y, New DXGI.Rational(60, 1), DXGI.Format.B8G8R8A8_UNorm)
            .SampleDescription = New DXGI.SampleDescription(1, 0)
            .Usage = SharpDX.DXGI.Usage.BackBuffer Or DXGI.Usage.RenderTargetOutput
            .Flags = DXGI.SwapChainFlags.AllowModeSwitch
            .IsWindowed = True
            .OutputHandle = handle
            .SwapEffect = DXGI.SwapEffect.Discard
        End With
        Direct3D11.Device.CreateWithSwapChain(SharpDX.Direct3D.DriverType.Hardware, Direct3D11.DeviceCreationFlags.BgraSupport, {SharpDX.Direct3D.FeatureLevel.Level_11_1}, sc_description, tmpGlobalDevice, tmpGlobalSwapChain)
        If GlobalDevice IsNot Nothing Then GlobalDevice.Dispose()
        GlobalDevice = tmpGlobalDevice.QueryInterface(Of SharpDX.Direct3D11.Device1)()
        If GlobalSwapChain IsNot Nothing Then
            GlobalSwapChain.GetBackBuffer(Of DXGI.Surface)(0).Dispose()
            GlobalSwapChain.Dispose()
        End If
        GlobalSwapChain = tmpGlobalSwapChain.QueryInterface(Of DXGI.SwapChain1)
        'create render target
        If D3DRenderTargetView IsNot Nothing Then
            D3DRenderTargetView.Resource.Dispose()
            D3DRenderTargetView.Dispose()
        End If
        Using resource = Direct3D11.Resource.FromSwapChain(Of Direct3D11.Texture2D)(GlobalSwapChain, 0)
            D3DRenderTargetView = New Direct3D11.RenderTargetView(GlobalDevice, resource)
        End Using
        'get device context
        If D3DContext IsNot Nothing Then
            D3DContext.Rasterizer.State.Dispose()
            D3DContext.OutputMerger.DepthStencilState.Dispose()
            D3DContext.OutputMerger.BlendState.Dispose()
            D3DContext.OutputMerger.ResetTargets()
            D3DContext.OutputMerger.Dispose()
            D3DContext.Dispose()
        End If
        D3DContext = GlobalDevice.ImmediateContext.QueryInterface(Of SharpDX.Direct3D11.DeviceContext1)()
        'create rasterizer
        Dim rsd = New Direct3D11.RasterizerStateDescription With {
            .CullMode = Direct3D11.CullMode.None,
            .FillMode = Direct3D11.FillMode.Solid}
        D3DContext.Rasterizer.State = New Direct3D11.RasterizerState(GlobalDevice, rsd)
        'create depth stencil
        If D3DDepthStencilView IsNot Nothing Then
            D3DDepthStencilView.Resource.Dispose()
            D3DDepthStencilView.Dispose()
        End If
        Dim depthBuffer_description As New Direct3D11.Texture2DDescription With {
                .ArraySize = 1,
                .BindFlags = Direct3D11.BindFlags.DepthStencil Or BindFlags.ShaderResource,
                .Format = SharpDX.DXGI.Format.R32_Typeless,
                .Width = Resolve.X,
                .Height = Resolve.Y,
                .MipLevels = 1,
                .SampleDescription = New DXGI.SampleDescription(1, 0),
                .Usage = ResourceUsage.Default,
                .CpuAccessFlags = CpuAccessFlags.None,
                .OptionFlags = ResourceOptionFlags.None
            }
        D3DDepthStencilResource = New Direct3D11.Texture2D(GlobalDevice, depthBuffer_description)
        Dim depthStencilView_description As DepthStencilViewDescription = New DepthStencilViewDescription With {
            .Format = DXGI.Format.D32_Float,
            .Dimension = DepthStencilViewDimension.Texture2D,
            .Texture2D = New DepthStencilViewDescription.Texture2DResource With {.MipSlice = 0}
        }
        D3DDepthStencilView = New Direct3D11.DepthStencilView(GlobalDevice, D3DDepthStencilResource, depthStencilView_description)
        Dim depthSRV_description As New ShaderResourceViewDescription With {
            .Format = DXGI.Format.R32_Float,
            .Dimension = Direct3D.ShaderResourceViewDimension.Texture2D,
            .Texture2D = New ShaderResourceViewDescription.Texture2DResource() With {.MipLevels = 1, .MostDetailedMip = 0}
        }
        D3DDepthStencilSRV = New ShaderResourceView(GlobalDevice, D3DDepthStencilResource, depthSRV_description)

        D3DContext.OutputMerger.SetTargets(D3DDepthStencilView, D3DRenderTargetView)

        Dim dss_desc As New Direct3D11.DepthStencilStateDescription With {
            .IsDepthEnabled = True,
            .DepthComparison = Comparison.LessEqual,
            .DepthWriteMask = DepthWriteMask.All}
        Dim dss As Direct3D11.DepthStencilState = New Direct3D11.DepthStencilState(GlobalDevice, dss_desc)
        D3DContext.OutputMerger.SetDepthStencilState(dss)

        '-------optional: disable depth buffer---------
        'Dim dss_desc As New Direct3D11.DepthStencilStateDescription With {
        '    .IsDepthEnabled = False,
        '    .IsStencilEnabled = False}
        'Dim dss As Direct3D11.DepthStencilState = New Direct3D11.DepthStencilState(GlobalDevice, dss_desc)
        'D3DContext.OutputMerger.SetDepthStencilState(dss)
        '--------------
        'set viewport
        Dim Viewport = New Mathematics.Interop.RawViewportF
        With Viewport
            .X = 0
            .Y = 0
            .Width = Resolve.X
            .Height = Resolve.Y
            .MaxDepth = 1.0F
            .MinDepth = 0.0F
        End With
        '----------alpha blend----------
        Dim tmpBSD As New Direct3D11.BlendStateDescription
        With tmpBSD.RenderTarget(0)
            .IsBlendEnabled = True
            .SourceBlend = Direct3D11.BlendOption.SourceAlpha
            .DestinationBlend = Direct3D11.BlendOption.InverseSourceAlpha
            .BlendOperation = Direct2D1.BlendOperation.Add
            .SourceAlphaBlend = Direct3D11.BlendOption.SourceAlpha
            .DestinationAlphaBlend = Direct3D11.BlendOption.InverseSourceAlpha
            .AlphaBlendOperation = Direct2D1.BlendOperation.Add
            .RenderTargetWriteMask = Direct3D11.ColorWriteMaskFlags.All
        End With
        Dim tmpBS As Direct3D11.BlendState = New Direct3D11.BlendState(GlobalDevice, tmpBSD)
        D3DContext.OutputMerger.SetBlendState(tmpBS, New RawColor4(0, 0, 0, 0), -1)
        '----------------
        D3DContext.Rasterizer.SetViewport(Viewport)

        '-------link to d2d1.1-----------
        Dim backBuffer As DXGI.Surface = GlobalSwapChain.GetBackBuffer(Of DXGI.Surface)(0)

        'Dim D3D11Device1 As Direct3D11.Device1 = GlobalDevice.QueryInterface(Of SharpDX.Direct3D11.Device1)()
        Dim DXGIDevice2 As SharpDX.DXGI.Device2 = GlobalDevice.QueryInterface(Of SharpDX.DXGI.Device2)()
        Dim d2dDevice As SharpDX.Direct2D1.Device = New SharpDX.Direct2D1.Device(DXGIDevice2)
        If D2DContext IsNot Nothing Then
            D2DContext.Device.Dispose()
            D2DContext.Dispose()
        End If
        D2DContext = New SharpDX.Direct2D1.DeviceContext(d2dDevice, SharpDX.Direct2D1.DeviceContextOptions.EnableMultithreadedOptimizations)

        Dim dpiX As Single, dpiY As Single
        Using myGraphics As System.Drawing.Graphics = Form1.CreateGraphics()    'use Graphics class to get the Dpi args
            dpiX = myGraphics.DpiX
            dpiY = myGraphics.DpiY
        End Using
        Dim Properties As BitmapProperties1 = New BitmapProperties1(New Direct2D1.PixelFormat(SharpDX.DXGI.Format.B8G8R8A8_UNorm, SharpDX.Direct2D1.AlphaMode.Premultiplied), dpiX, dpiY, BitmapOptions.Target Or BitmapOptions.CannotDraw)
        If D2DTarget IsNot Nothing Then
            D2DTarget.Dispose()
            D3DRenderImage.Dispose()
            BitmapBackgroundCanvas.Dispose()
            D3DShadowMapImage.Dispose()
            D3DSSAOFirstRoundImage.dispose()
        End If
        D2DTarget = New Bitmap1(D2DContext, backBuffer, Properties)
        '-------image output---------
        Dim normalProp As New BitmapProperties1() With {
                      .PixelFormat = New SharpDX.Direct2D1.PixelFormat(SharpDX.DXGI.Format.B8G8R8A8_UNorm, SharpDX.Direct2D1.AlphaMode.Premultiplied),
                      .BitmapOptions = BitmapOptions.Target}
        D3DRenderImage = New Bitmap1(D2DContext, New Size2(Resolve.X, Resolve.Y), normalProp)
        BitmapBackgroundCanvas = New Bitmap1(D2DContext, New Size2(Resolve.X, Resolve.Y), normalProp)
        D3DShadowMapImage = New Bitmap1(D2DContext, New Size2(Resolve.X, Resolve.Y), normalProp)
        D3DSSAOFirstRoundImage = New Bitmap1(D2DContext, New Size2(Resolve.X, Resolve.Y), normalProp)

        '-------load shaders-------
        Me.CurrentRasterizerCamera.HalfResolve = New Vector2(Resolve.X * 0.5, Resolve.Y * 0.5)
        Me.CurrentRasterizerCamera.LoadShader("C:\Users\asdfg\Desktop\rtTest\SSAO.hlsl", GlobalDevice, D3DContext)



    End Sub

    ''' <summary>
    ''' 在当前D2D图层直接绘制D3D生成的图像
    ''' </summary>
    Public Sub DrawLink3DImage(context As SharpDX.Direct2D1.DeviceContext, spectator As SpectatorCamera, canvasBitmap As Bitmap1)
        context.DrawImage(D3DRenderImage)
    End Sub

    Public Sub PaintImage()

        SyncLock RenderingSyncLock

            ''-----------optional: draw shadow map--------------------
            '' clear canvas and depth buffer
            'D3DContext.ClearRenderTargetView(D3DRenderTargetView, New Mathematics.Interop.RawColor4(0, 0, 0, 1))    ' rgba
            'D3DContext.ClearDepthStencilView(D3DDepthStencilView, Direct3D11.DepthStencilClearFlags.Depth, 1, 0)

            'Me.CurrentRasterizerCamera.DrawShadowMap(GlobalDevice, D3DContext)
            'D3DShadowMapImage.CopyFromBitmap(D2DTarget)
            '' convert depth bitmap to shader resource view
            'Dim shadowMapTexture As Texture2D = GetTextureFromBitmap1(GlobalDevice, D2DContext, D3DShadowMapImage)
            'Dim srvd As New Direct3D11.ShaderResourceViewDescription With {
            '    .Format = shadowMapTexture.Description.Format,
            '    .Dimension = SharpDX.Direct3D.ShaderResourceViewDimension.Texture2D}
            'srvd.Texture2D.MostDetailedMip = 0
            'srvd.Texture2D.MipLevels = -1
            'If D3DShadowMapSRV IsNot Nothing Then D3DShadowMapSRV.Dispose()
            'D3DShadowMapSRV = New Direct3D11.ShaderResourceView(GlobalDevice, shadowMapTexture, srvd)
            'GlobalDevice.ImmediateContext.GenerateMips(D3DShadowMapSRV)
            'shadowMapTexture.Dispose()
            ''--------------------------------------------------------

            '----------draw SSAO--------------------
            'TODO
            ' clear canvas and depth buffer
            D3DContext.ClearRenderTargetView(D3DRenderTargetView, New Mathematics.Interop.RawColor4(1, 1, 1, 1))    ' rgba
            D3DContext.ClearDepthStencilView(D3DDepthStencilView, Direct3D11.DepthStencilClearFlags.Depth, 1, 0)

            Me.CurrentRasterizerCamera.DrawSSAO1(GlobalDevice, D3DContext)

            D3DSSAOFirstRoundImage.CopyFromBitmap(D2DTarget)
            Dim ssaoTexture As Texture2D = GetTextureFromBitmap1(GlobalDevice, D2DContext, D3DSSAOFirstRoundImage)
            Dim srvd As New Direct3D11.ShaderResourceViewDescription With {
                .Format = ssaoTexture.Description.Format,
                .Dimension = SharpDX.Direct3D.ShaderResourceViewDimension.Texture2D}
            srvd.Texture2D.MostDetailedMip = 0
            srvd.Texture2D.MipLevels = -1
            If D3DSSAOFirstRoundSRV IsNot Nothing Then D3DSSAOFirstRoundSRV.Dispose()
            D3DSSAOFirstRoundSRV = New Direct3D11.ShaderResourceView(GlobalDevice, ssaoTexture, srvd)
            GlobalDevice.ImmediateContext.GenerateMips(D3DSSAOFirstRoundSRV)
            ssaoTexture.Dispose()

            D3DContext.ClearRenderTargetView(D3DRenderTargetView, New Mathematics.Interop.RawColor4(1, 1, 1, 1))    ' rgba
            D3DContext.ClearDepthStencilView(D3DDepthStencilView, Direct3D11.DepthStencilClearFlags.Depth, 1, 0)

            Me.CurrentRasterizerCamera.DrawSSAO2(GlobalDevice, D3DContext)

            '--------------------------------------------------------

            '' clear canvas and depth buffer
            'D3DContext.ClearRenderTargetView(D3DRenderTargetView, New Mathematics.Interop.RawColor4(1, 1, 1, 1))
            'D3DContext.ClearDepthStencilView(D3DDepthStencilView, Direct3D11.DepthStencilClearFlags.Depth, 1, 0)

            'Me.CurrentRasterizerCamera.DrawContainer3D(GlobalDevice, D3DContext)
            'copy the 3d image to memory in order to add further 2d graphics
            D3DRenderImage.CopyFromBitmap(D2DTarget)

            'draw d2d
            With D2DContext
                .Target = BitmapBackgroundCanvas
                .BeginDraw()
                .Clear(New RawColor4(1, 1, 1, 1))

                '.DrawImage(D3DRenderImage)  ' 3d layer
                For i = 0 To PaintingLayers.Count - 1  ' 2d layers
                    PaintingLayers(i).Invoke(D2DContext, Me, BitmapBackgroundCanvas)
                Next

                .EndDraw()

                .Target = D2DTarget
                .BeginDraw()
                .Clear(New RawColor4(1, 1, 1, 1))
                .DrawImage(BitmapBackgroundCanvas)
                '.DrawImage(ImageResources.Item("1"), New RawVector2(0, 0))
                'testdraw(D2DContext)
                '.DrawImage(ImageResources.Item("2"), New RawVector2(750, 400))

                .EndDraw()
            End With
            'display
            GlobalSwapChain.Present(0, DXGI.PresentFlags.None)
            'event
            RaiseEvent EveryFramePass()

        End SyncLock

    End Sub

    Public Sub Resize(width As Integer, height As Integer, canvas As Control)

        SyncLock RenderingSyncLock

            Me.Resolve = New Vector2(width, height)
            With CurrentRasterizerCamera
                .HalfResolve = Me.Resolve / 2
                .Scale = New Vector2(Zoom / Resolve.X, Zoom / Resolve.Y)
                .CalculateWVP()
            End With
            InitializeDirectComponents(canvas)
            InitializePBoxCanvas()
            UpdateAllImageResources(D2DContext, GlobalDevice)

        End SyncLock

    End Sub

    ''' <summary>
    ''' 获取D2DDeviceContext
    ''' </summary>
    Public Function GetDeviceContext() As Direct2D1.DeviceContext
        Return D2DContext
    End Function

    Public Function GetGlobalDevice() As Direct3D11.Device1
        Return Me.GlobalDevice
    End Function

    Public Function GetDepthBufffer() As Direct3D11.DepthStencilView
        Return Me.D3DDepthStencilView
    End Function

    Public Function GetRenderTargetView() As Direct3D11.RenderTargetView
        Return Me.D3DRenderTargetView
    End Function


    ''' <summary>
    ''' 获取一半的Resolve值，即屏幕中心
    ''' </summary>
    Public Function GetCenter() As Vector2
        Return New Vector2(CInt(Me.Resolve.X / 2), CInt(Me.Resolve.Y / 2))
    End Function

    ''' <summary>
    ''' dispose all directx resources
    ''' </summary>
    Public Sub Dispose() Implements IDisposable.Dispose
        SyncLock RenderingSyncLock
            GlobalDevice.Dispose()
            GlobalSwapChain.GetBackBuffer(Of DXGI.Surface)(0).Dispose()
            GlobalSwapChain.Dispose()
            D3DRenderTargetView.Resource.Dispose()
            D3DRenderTargetView.Dispose()
            D3DContext.Rasterizer.State.Dispose()
            D3DContext.OutputMerger.DepthStencilState.Dispose()
            D3DContext.OutputMerger.BlendState.Dispose()
            D3DContext.OutputMerger.ResetTargets()
            D3DContext.OutputMerger.Dispose()
            D3DContext.Dispose()
            D3DDepthStencilSRV.Dispose()
            D3DDepthStencilResource.Dispose()
            D3DDepthStencilView.Resource.Dispose()
            D3DDepthStencilView.Dispose()
            D2DContext.Device.Dispose()
            D2DContext.Dispose()
            D2DTarget.Dispose()
            D3DRenderImage.Dispose()
            BitmapBackgroundCanvas.Dispose()

            End
        End SyncLock

    End Sub


End Class



