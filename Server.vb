Imports System.Net
Imports System.Net.Sockets
Imports System.Text
Imports System.Threading

Module Server

    Private TCPListener As New System.Net.Sockets.Socket(Net.Sockets.AddressFamily.InterNetwork, Net.Sockets.SocketType.Stream, Net.Sockets.ProtocolType.Tcp)
    Public ServerShutdownFlag As Boolean = False

    Public ReceivedFrameData As New List(Of Single())
    Public ReceivedFrame_MaxCount As Integer = 10

    Public LowPass_k As Integer = 8
    Public LowPass_sigma As Single = 5.0F

    Public RenderQueue As New List(Of Single())
    Public RenderQueueLock As New Object

    Public Sub RenderQueueLoop()

        While True
            Dim listenStartAt As Date = DateTime.Now

            SyncLock RenderQueueLock
                If RenderQueue.Count > 0 Then
                    Dim val As Single() = RenderQueue.First
                    RenderQueue.RemoveAt(0)
                    ApplyMotionScript(val)
                End If
            End SyncLock

            Dim listenEndAt As Date = DateTime.Now
            While (listenEndAt - listenStartAt).Milliseconds < 33
                listenEndAt = DateTime.Now
                Application.DoEvents()
            End While
        End While

    End Sub

    Private Function ApplyLowPassFilter(k As Integer, sigma As Single) As Single()
        ' low pass filter
        Dim res(10) As Single
        For j = 0 To 10
            res(j) = 0.0F
        Next
        Dim weight_sum As Single = 0.0F
        For i = 0 To ReceivedFrameData.Count - 1
            Dim x As Single = ReceivedFrameData.Count - i - 0.5
            Dim val As Single() = ReceivedFrameData(i)
            Dim weight As Single = Math.Exp(-x * x / (2.0 * sigma * sigma)) / (Math.Sqrt(6.2832) * sigma)
            weight_sum += weight
            For j = 0 To 10
                res(j) += weight * val(j)
            Next
        Next
        For j = 0 To 10
            res(j) /= weight_sum
        Next
        Return res
    End Function

    '接收
    Public Sub TryReceive()

        While Not ServerShutdownFlag
            Dim listenStartAt As Date = DateTime.Now
            Dim bytes() As Byte
            Dim handler As System.Net.Sockets.Socket = TCPListener.Accept()     '建立连接请求

            Dim poseData As New List(Of Single)

            bytes = New Byte(1024) {}
            Dim bytesRec As Integer = handler.Receive(bytes)     '接收数据
            If bytesRec > 0 Then
                'data = System.Text.Encoding.Unicode.GetString(bytes, 0, bytesRec)
                'Console.WriteLine(data)

                poseData.Add(BitConverter.ToSingle(bytes, 0))    ' has face
                poseData.Add(BitConverter.ToSingle(bytes, 4))    ' face pos x
                poseData.Add(BitConverter.ToSingle(bytes, 8))    ' face pos y
                poseData.Add(BitConverter.ToSingle(bytes, 12))    ' face rect r
                poseData.Add(BitConverter.ToSingle(bytes, 16))    ' head rx
                poseData.Add(BitConverter.ToSingle(bytes, 20))    ' head ry
                poseData.Add(BitConverter.ToSingle(bytes, 24))    ' head rz
                poseData.Add(BitConverter.ToSingle(bytes, 28))    ' blink
                poseData.Add(BitConverter.ToSingle(bytes, 32))    ' smile
                poseData.Add(BitConverter.ToSingle(bytes, 36))    ' mouth_a
                poseData.Add(BitConverter.ToSingle(bytes, 40))    ' serious


                ReceivedFrameData.Add(poseData.ToArray)
                If (ReceivedFrameData.Count > ReceivedFrame_MaxCount) Then
                    ReceivedFrameData.RemoveAt(0)
                End If
                Dim lp_data As Single() = ApplyLowPassFilter(LowPass_k, LowPass_sigma)
                SyncLock RenderQueueLock
                    RenderQueue.Add(lp_data)
                End SyncLock

            Else

            End If
            handler.Shutdown(Net.Sockets.SocketShutdown.Both)
            handler.Close()
            handler.Dispose()

            'ApplyMotionScript(lp_data)

            Dim listenEndAt As Date = DateTime.Now
            While (listenEndAt - listenStartAt).Milliseconds < 42
                listenEndAt = DateTime.Now
                Application.DoEvents()
            End While
        End While
        Form1.PostMsg("Server shutdown")

    End Sub

    Public Sub StartListener()
        '指定ip和端口
        Dim localEndPoint As New System.Net.IPEndPoint(IPAddress.Parse("0.0.0.0"), 9979)
        TCPListener.Bind(localEndPoint)
        TCPListener.Listen(10)

        Form1.PostMsg("Server started at 9979")

        'TryReceive()
        Dim t As New Task(AddressOf TryReceive)
        t.Start()


    End Sub

    Public Sub SendTCPMessage()
        Dim sender1 As New System.Net.Sockets.Socket(Net.Sockets.AddressFamily.InterNetwork, Net.Sockets.SocketType.Stream, Net.Sockets.ProtocolType.Tcp)
        '初始化socket
        Dim msg As Byte() = System.Text.Encoding.Unicode.GetBytes("helloworld")
        '对发送的数据进行编码
        '***************************
        '指定ip和端口
        'Dim ipHostInfo As System.Net.IPHostEntry = System.Net.Dns.GetHostEntry("192.168.0.107")
        'Dim ipAddress As System.Net.IPAddress = ipHostInfo.AddressList(0)
        Dim ipe As New System.Net.IPEndPoint(IPAddress.Parse("192.168.0.128"), 9979)
        '**********************
        sender1.Connect(ipe) '建立连接
        Dim bytesSent As Integer = sender1.Send(msg) '发送数据
        '关闭socket
        sender1.Shutdown(Net.Sockets.SocketShutdown.Both)
        sender1.Close()
        sender1.Dispose()
    End Sub


End Module
