Imports System.Net
Imports System.Net.Sockets
Imports System.Text
Imports System.Threading

Module Server

    Dim listener As New System.Net.Sockets.Socket(Net.Sockets.AddressFamily.InterNetwork, Net.Sockets.SocketType.Stream, Net.Sockets.ProtocolType.Tcp)
    '初始socket

    '接收
    Public Async Sub TryReceive()
        While True
            Dim bytes() As Byte
            Dim handler As System.Net.Sockets.Socket = listener.Accept()     '建立连接请求

            Dim data As String = Nothing
            bytes = New Byte(1024) {}
            Dim bytesRec As Integer = handler.Receive(bytes)     '接收数据
            If bytesRec > 0 Then
                data = System.Text.Encoding.Unicode.GetString(bytes, 0, bytesRec)
                Console.WriteLine(data)
            Else
                Exit Sub
            End If
            handler.Shutdown(Net.Sockets.SocketShutdown.Both)
            handler.Close()
            handler.Dispose()
            Await Task.Delay(100)
        End While

    End Sub

    Public Sub StartListener()
        '指定ip和端口
        'Dim ipHostInfo As System.Net.IPHostEntry = System.Net.Dns.GetHostEntry(System.Net.Dns.GetHostName())
        'Dim ipAddress As System.Net.IPAddress = ipHostInfo.AddressList(0)
        'Dim localEndPoint As New System.Net.IPEndPoint(IPAddress.Parse("192.168.1.128"), 9979)
        Dim localEndPoint As New System.Net.IPEndPoint(IPAddress.Parse("0.0.0.0"), 9979)
        listener.Bind(localEndPoint)
        listener.Listen(10)

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
        Dim ipe As New System.Net.IPEndPoint(IPAddress.Parse("192.168.1.128"), 9979)
        '**********************
        sender1.Connect(ipe) '建立连接
        Dim bytesSent As Integer = sender1.Send(msg) '发送数据
        '关闭socket
        sender1.Shutdown(Net.Sockets.SocketShutdown.Both)
        sender1.Close()
        sender1.Dispose()
    End Sub


End Module
