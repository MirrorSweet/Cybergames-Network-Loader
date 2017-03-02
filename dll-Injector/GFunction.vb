Imports System.IO
Imports System.ComponentModel
Imports System.Runtime.InteropServices
Imports System.Windows.Forms

Public Class GFunction
    Public Structure Memory
        Dim qlvkoelbfkla23i928awefvfsf As Byte

        Private Declare Function WriteProcessMemory Lib "kernel32" (ByVal Handle As Integer, ByVal address As Integer, ByRef Value As Int32, ByVal Size As Integer, ByRef lpNumberOfBytesWritten As Long) As Long
        Private Declare Function WriteFloatMemory Lib "kernel32" Alias "WriteProcessMemory" (ByVal hProcess As Integer, ByVal lpBaseAddress As Integer, ByRef lpBuffer As Single, ByVal nSize As Integer, ByRef lpNumberOfBytesWritten As Integer) As Integer
        Private Declare Function ReadProcessMemory Lib "kernel32" (ByVal Handle As Integer, ByVal address As Integer, ByRef Value As Int32, ByVal Size As Integer, ByRef lpNumberOfBytesWritten As Long) As Long
        Private Declare Function ReadFloatMemory Lib "kernel32" Alias "ReadProcessMemory" (ByVal hProcess As Integer, ByVal lpBaseAddress As Integer, ByRef lpBuffer As Single, ByVal nSize As Integer, ByRef lpNumberOfBytesWritten As Integer) As Integer
        Public Declare Function VirtualProtectEx Lib "KERNEL32.dll" (ByVal hProcess As IntPtr, ByVal lpAddress As IntPtr, ByVal dwSize As IntPtr, ByVal newProtect As MemroyProtection, ByRef oldProtect As Integer) As Boolean
        'Public Declare Function VirtualProtectEx Lib "KERNEL32.dll" (ByVal hProcess As IntPtr, ByVal lpAddress As IntPtr, ByVal dwSize As IntPtr, ByVal newProtect As Integer, ByRef oldProtect As Integer) As Boolean
        Public Declare Function OpenProcess Lib "KERNEL32" (ByVal DesiredAccess As ProcessAccess, ByVal InheritHandle As Boolean, ByVal ProcessId As Int32) As Int32
        Public Shared pHandle As Integer
        Public Shared process_id As Int32

        Dim FlagValue As Integer

        <Flags()> _
        Public Enum ProcessAccess As Integer
            PROCESS_ALL_ACCESS = &H1F0FFF&
            PROCESS_CREATE_PROCESS = &H80
            PROCESS_CREATE_THREAD = &H2
            PROCESS_DUP_HANDLE = &H40
            PROCESS_HEAP_ENTRY_BUSY = &H4
            PROCESS_HEAP_ENTRY_DDESHARE = &H20
            PROCESS_HEAP_ENTRY_MOVEABLE = &H10
            PROCESS_HEAP_REGION = &H1
            PROCESS_HEAP_UNCOMMITTED_RANGE = &H2
            PROCESS_QUERY_INFORMATION = &H400
            PROCESS_SET_INFORMATION = &H200
            PROCESS_SET_QUOTA = &H100
            PROCESS_TERMINATE = &H1
            PROCESS_VM_OPERATION = &H8
            PROCESS_VM_READ = &H10
            PROCESS_VM_WRITE = &H20
            PROCESS_Synchronize = &H100000
            PROCESS_SUSPEND_RESUME = 2048
        End Enum
        <Flags()> _
        Public Enum MemroyProtection As Integer
            PAGE_NOACCESS = 1
            PAGE_READONLY = 2
            PAGE_READWRITE = 4
            PAGE_WRITECOPY = 8
            PAGE_EXECUTE = 16
            PAGE_EXECUTE_READ = 32
            PAGE_EXECUTE_READWRITE = 64
            PAGE_EXECUTE_WRITECOPY = 128
            PAGE_GUARD = 256
            PAGE_NOCACHE = 512
        End Enum
        Public Function GetProcessHandle(ByVal ProcessName As String) As Boolean 'Checks to see if the game is running (returns True or False) and sets the pHandle *REQUIRED TO USE*
            On Error Resume Next
            For Each p As Process In Process.GetProcessesByName(ProcessName)
                process_id = p.Id
                pHandle = OpenProcess(ProcessAccess.PROCESS_ALL_ACCESS, False, p.Id)
                Return True
            Next
            Return False
        End Function
        Public Sub RemoveProtection(ByVal AddressOfStart As Integer) 'Changes the protection of the page with the specified starting address to PAGE_EXECUTE_READWRITE
            On Error Resume Next
            Dim oldProtect As Integer
            If Not VirtualProtectEx(pHandle, New IntPtr(AddressOfStart), New IntPtr(2048), MemroyProtection.PAGE_EXECUTE_READWRITE, oldProtect) Then Throw New Win32Exception
        End Sub
        Public Function GetBaseAddress(ByVal ModuleName As String) As Integer
            Dim base As Integer
            For Each PM As ProcessModule In Process.GetProcessById(process_id).Modules
                If ModuleName.ToLower = PM.ModuleName.ToLower Then base = PM.BaseAddress
            Next
            Return base
        End Function
#Region "GConvert"
        Public Shared Function GetStringToArrayBytes(ByVal str As String) As Byte()
            Dim encoding As New System.Text.UTF8Encoding()
            Return encoding.GetBytes(str)
        End Function
        Public Shared Function GetArrayBytes(ByVal Value As String) As Byte()
            Dim strArray As String() = Value.Split(New Char() {" "})
            Dim buffer As Byte() = New Byte(strArray.Length - 1) {}
            Dim i As Integer
            For i = 0 To strArray.Length - 1
                buffer(i) = Convert.ToByte(strArray(i), &H10)
            Next i
            Return buffer
        End Function
#End Region
#Region "Write"

        Public Shared Function WriteString(ByVal Address As Integer, ByVal Text As String) As Boolean
            Dim sString As Byte() = GFunction.Memory.GetArrayBytes(Text)
            For i As Integer = LBound(sString) To UBound(sString)
                WriteByte(Address + i, sString(i))
            Next
        End Function
        Public Shared Sub WriteByte(ByVal Address As Integer, ByVal Value As Byte) 'Writes a single byte value
            WriteProcessMemory(pHandle, Address, Value, 1, 0)
        End Sub
        Public Sub WriteASM(ByVal Address As Int32, ByVal Value As Byte()) 'Writes assembly using bytes
            For i As Integer = LBound(Value) To UBound(Value)
                WriteByte(Address + i, Value(i))
            Next
        End Sub

        Public Sub WriteInteger(ByVal Address As Integer, ByVal Value As Integer) 'Writes a single byte value
            WriteProcessMemory(pHandle, Address, Value, 4, 0)
        End Sub
        Public Sub WriteFloat(ByVal Address As Integer, ByVal Value As Single) 'Writes a 2 bytes value
            WriteFloatMemory(pHandle, Address, Value, 4, 0)
        End Sub
        Public Sub WriteFloath4x(ByVal Address As Integer, ByVal Value As Long) 'Writes a 2 bytes value
            WriteFloatMemory(pHandle, Address, Value, 4, Nothing)
        End Sub
        Public Function WritePointer(ByVal Pointer As Int32, ByVal Buffer As Int32, ByVal OffSet As Int32()) 'Writes to a pointer
            For Each I As Integer In OffSet
                ReadProcessMemory(pHandle, Pointer, Pointer, 4, 0)
                Pointer += I
            Next
            WriteProcessMemory(pHandle, Pointer, Buffer, 4, 0)
            Return 0
        End Function
        Public Function WriteAddPointer(ByVal Pointer As Int32, ByVal Buffer As Int32, ByVal OffSet() As Int32) 'Adds a value to a pointer
            For Each I As Integer In OffSet
                ReadProcessMemory(pHandle, Pointer, Pointer, 4, 0)
                Pointer += I
            Next
            WriteProcessMemory(pHandle, Pointer, ReadInteger(Pointer) + Buffer, 4, 0)
            Return 0
        End Function
#End Region
#Region "Read"
        Public Shared Function ReadByte(ByVal Address As Int32) As Byte
            Dim value As Integer
            ReadProcessMemory(pHandle, Address, value, 1, 0)
            Return value
        End Function
        Public Function ReadInteger(ByVal Address As Int32) As Int32
            Dim value As Integer
            ReadProcessMemory(pHandle, Address, value, 4, 0)
            Return value
        End Function
        Public Function ReadFloat(ByVal Address As Int32) As Double
            Dim value As Single
            ReadFloatMemory(pHandle, Address, value, 4, 0)
            Return value
        End Function
        Public Function ReadFloath4x(ByVal Address As Int32) As Int32
            Dim value As Single
            ReadFloatMemory(pHandle, Address, value, 4, 0)
            Return value
        End Function
        Public Function ReadPointer(ByVal Pointer As Int32, ByRef Buffer As Int32, ByVal OffSet() As Int32) 'Reads a pointer value and returns it
            For Each I As Integer In OffSet
                ReadProcessMemory(pHandle, Pointer, Pointer, 4, 0)
                Pointer += I
            Next
            ReadProcessMemory(pHandle, Pointer, Buffer, 4, 0)
            Return 0
        End Function


#End Region
#Region "Other"
        Private Declare Function VirtualAllocEx Lib "kernel32" (ByVal hProcess As Integer, ByVal lpAddress As Integer, ByVal dwSize As Integer, ByVal flAllocationType As Integer, ByVal flProtect As Integer) As Integer
        Private Declare Function WriteProcessMemory Lib "kernel32" (ByVal hProcess As Integer, ByVal lpBaseAddress As Integer, ByVal lpBuffer() As Byte, ByVal nSize As Integer, ByVal lpNumberOfBytesWritten As UInteger) As Boolean
        Private Declare Function GetProcAddress Lib "kernel32" (ByVal hModule As Integer, ByVal lpProcName As String) As Integer
        Private Declare Function GetModuleHandle Lib "kernel32" Alias "GetModuleHandleA" (ByVal lpModuleName As String) As Integer
        Private Declare Function CreateRemoteThread Lib "kernel32" (ByVal hProcess As Integer, ByVal lpThreadAttributes As Integer, ByVal dwStackSize As Integer, ByVal lpStartAddress As Integer, ByVal lpParameter As Integer, ByVal dwCreationFlags As Integer, ByVal lpThreadId As Integer) As Integer
        Private Declare Function WaitForSingleObject Lib "kernel32" (ByVal hHandle As Integer, ByVal dwMilliseconds As Integer) As Integer
        Private Declare Function CloseHandle Lib "kernel32" (ByVal hObject As Integer) As Integer

        Private Function Die(Optional ByVal hProc As Integer = Nothing, Optional ByVal libThread As Integer = Nothing) As Boolean
            If Not hProc = Nothing Then CloseHandle(hProc)
            If Not libThread = Nothing Then CloseHandle(libThread)
            Return False
        End Function
        Public Function InjectDll(ByVal dllLocation As String) As Boolean
            If IO.File.Exists(dllLocation) = False Then Return False 'if the dll doesn't exist, no point in continuing. So we return false.
            If IntPtr.Size = 8 Then Return False 'If the size of an IntPtr is 8, then is program was compiled as x64. x64 processes can't access x86 processes properly, so we just return false. You need to compile this program as x86.
            Dim hProcess As Integer = OpenProcess(&H1F0FFF, 1, process_id) 'We'll open the process specified by the input process ID. With PROCESS_ALL_ACCESS access, seeing as we only need to write.
            If hProcess = 0 Then Return Die() 'If we didn't get the handle, we exit and return false. No cleanup so no params for die()
            Dim dllBytes As Byte() = System.Text.Encoding.ASCII.GetBytes(dllLocation + ControlChars.NullChar) 'As I mentioned earlier, we have to write the dll location as bytes to the process memory, so we take the bytes of the string using the standard encoding, adding a null byte at the end.
            Dim pathLocation As Integer = VirtualAllocEx(hProcess, 0, dllBytes.Length, &H1000, &H4) 'Allocate memory the size of the string we need to write to memory. pathLocation now holds the address of where the memory was allocated.
            If pathLocation = Nothing Then Return Die(hProcess) 'VirtualAllocEx returns Nothing when it fails, so we check for that and return false if we find it. We've opened a process handle so we have to pass that to Die to clean it up.
            Dim wpm As Integer = WriteProcessMemory(hProcess, pathLocation, dllBytes, dllBytes.Length, 0) 'write the contents of dllBytes to the memory allocated at pathLocation.
            If wpm = 0 Then Return Die(hProcess) ' WriteProcessMemory returns 0 if it fails.
            Dim kernelMod As Integer = GetModuleHandle("kernel32.dll") 'Remember what I was saying about kernel32 being loaded into the same address space for every normal process? This means we don't have to do any fancy crap to find its location in our target process, we can get the location in our own process and safely assume it will be the same for all process. This means we can use GetModuleHandle, which only works internally.
            Dim loadLibAddr As Integer = GetProcAddress(kernelMod, "LoadLibraryA") ' GetProcAddress gives us the address of the specified function within the module.
            If loadLibAddr = 0 Then Return Die(hProcess) 'If GetProcAddress failed it'll return 0.
            Dim procThread As Integer = CreateRemoteThread(hProcess, 0, 0, loadLibAddr, pathLocation, 0, 0) 'Okay, this is the thread creation. We pass our process handle to tell what process to create the thread on. the third param is the handle of the function to call. In this case we choose the LoadLibrary function. The next param is the arguments to pass to the function (omg remember we wrote that to memory? NOW WE PASS THE ADDRESS BACK!)
            If procThread = 0 Then Return Die(hProcess) 'unable to create the thread. Return false
            Dim waitVal As Integer = WaitForSingleObject(procThread, 5000) 'allow the LoadLibrary function 5 seconds to process.
            If Not waitVal = &H0UI Then Return Die(hProcess, procThread) 'Function didn't signal completion. Fuck that shit abort ABORT
            CloseHandle(procThread) 'close the handle to the LoadLibrary function
            CloseHandle(hProcess) 'close the handle to the process
            Return True 'made it, yay.
        End Function

#End Region
    End Structure


    Public Structure Window

        Dim fklgoriow, cmbkjdiwpqkajxkjls As Byte
        ''' <summary>
        ''' <para>The DestroyWindow function destroys the specified window. The function sends WM_DESTROY and WM_NCDESTROY messages to the window to deactivate it and remove the keyboard focus from it. The function also destroys the window's menu, flushes the thread message queue, destroys timers, removes clipboard ownership, and breaks the clipboard viewer chain (if the window is at the top of the viewer chain).</para>
        ''' <para>If the specified window is a parent or owner window, DestroyWindow automatically destroys the associated child or owned windows when it destroys the parent or owner window. The function first destroys child or owned windows, and then it destroys the parent or owner window.</para>
        ''' <para>DestroyWindow also destroys modeless dialog boxes created by the CreateDialog function.</para>
        ''' </summary>
        ''' <param name="hwnd">Handle to the window to be destroyed.</param>
        ''' <returns>If the function succeeds, the return value is nonzero. If the function fails, the return value is zero. To get extended error information, call GetLastError.</returns>
        <DllImport("user32.dll", CharSet:=CharSet.Unicode, SetLastError:=True)> _
        Private Shared Function DestroyWindow(ByVal hwnd As IntPtr) As <MarshalAs(UnmanagedType.Bool)> Boolean
        End Function
        <DllImport("user32.dll")> _
        Private Shared Function WindowFromPoint(ByVal Point As Drawing.Point) As IntPtr
        End Function

        <DllImport("user32.dll", SetLastError:=True)> _
        Public Shared Function CloseWindow(ByVal hWnd As IntPtr) As Integer
        End Function
        <DllImport("user32.dll", SetLastError:=True)> _
        Public Shared Function SetWindowPos(ByVal hWnd As IntPtr, ByVal hWndInsertAfter As IntPtr, ByVal X As Integer, ByVal Y As Integer, ByVal cx As Integer, ByVal cy As Integer, ByVal uFlags As UInteger) As Boolean
        End Function
        <DllImport("user32.dll", SetLastError:=True, CharSet:=CharSet.Auto)> _
        Public Shared Function ShowWindow(ByVal hwnd As IntPtr, ByVal nCmdShow As WindowShowStyle) As Boolean
        End Function
        Public Declare Function DestroyWindow Lib "user32" Alias "DestroyWindow" (ByVal hwnd As Long) As Long
        Public Declare Function PostMessage Lib "user32" Alias "PostMessageA" (ByVal hWnd As Int32, ByVal uMsg As UInt32, ByVal wParam As Int32, ByVal lParam As Int32) As Int32
        Public Declare Function GetWindow Lib "user32" Alias "GetWindow" (ByVal hWnd As Int32, ByVal uCmd As uCmd) As Int32
        Public Declare Function FindWindow Lib "user32" Alias "FindWindowA" (ByVal lpClassName As String, ByVal lpWindowName As String) As Int32
        Public Declare Function FindWindowEx Lib "user32" Alias "FindWindowExA" (ByVal hWnd1 As Int32, ByVal hWnd2 As Int32, ByVal ClassName As String, ByVal WindowTitle As String) As Int32
        Public Declare Function SendMessage Lib "user32" Alias "SendMessageA" (ByVal hWnd As Int32, ByVal wMsg As Int32, ByVal wParam As Int32, ByVal lParam As String) As Int32
#Region "Flags"
        <Flags()> _
        Public Enum uCmd As UInteger
            uCmd_HWNDFIRST = 0
            uCmd_HWNDLAST = 1
            uCmd_HWNDNEXT = 2
            uCmd_HWNDPREV = 3
            uCmd_OWNER = 4
            uCmd_CHILD = 5
            uCmd_ENABLEDPOPUP = 6
        End Enum
        ''' <summary>
        ''' Window Styles.
        ''' The following styles can be specified wherever a window style is required. After the control has been created, these styles cannot be modified, except as noted.
        ''' </summary>
        <Flags()> Public Enum WindowStyles As UInteger
            ''' <summary>The window has a thin-line border.</summary>
            WS_BORDER = &H800000

            ''' <summary>The window has a title bar (includes the WS_BORDER style).</summary>
            WS_CAPTION = &HC00000

            ''' <summary>The window is a child window. A window with this style cannot have a menu bar. This style cannot be used with the WS_POPUP style.</summary>
            WS_CHILD = &H40000000

            ''' <summary>Excludes the area occupied by child windows when drawing occurs within the parent window. This style is used when creating the parent window.</summary>
            WS_CLIPCHILDREN = &H2000000

            ''' <summary>
            ''' Clips child windows relative to each other; that is, when a particular child window receives a WM_PAINT message, the WS_CLIPSIBLINGS style clips all other overlapping child windows out of the region of the child window to be updated.
            ''' If WS_CLIPSIBLINGS is not specified and child windows overlap, it is possible, when drawing within the client area of a child window, to draw within the client area of a neighboring child window.
            ''' </summary>
            WS_CLIPSIBLINGS = &H4000000

            ''' <summary>The window is initially disabled. A disabled window cannot receive input from the user. To change this after a window has been created, use the EnableWindow function.</summary>
            WS_DISABLED = &H8000000

            ''' <summary>The window has a border of a style typically used with dialog boxes. A window with this style cannot have a title bar.</summary>
            WS_DLGFRAME = &H400000

            ''' <summary>
            ''' The window is the first control of a group of controls. The group consists of this first control and all controls defined after it, up to the next control with the WS_GROUP style.
            ''' The first control in each group usually has the WS_TABSTOP style so that the user can move from group to group. The user can subsequently change the keyboard focus from one control in the group to the next control in the group by using the direction keys.
            ''' You can turn this style on and off to change dialog box navigation. To change this style after a window has been created, use the SetWindowLong function.
            ''' </summary>
            WS_GROUP = &H20000

            ''' <summary>The window has a horizontal scroll bar.</summary>
            WS_HSCROLL = &H100000

            ''' <summary>The window is initially maximized.</summary> 
            WS_MAXIMIZE = &H1000000

            ''' <summary>The window has a maximize button. Cannot be combined with the WS_EX_CONTEXTHELP style. The WS_SYSMENU style must also be specified.</summary> 
            WS_MAXIMIZEBOX = &H10000

            ''' <summary>The window is initially minimized.</summary>
            WS_MINIMIZE = &H20000000

            ''' <summary>The window has a minimize button. Cannot be combined with the WS_EX_CONTEXTHELP style. The WS_SYSMENU style must also be specified.</summary>
            WS_MINIMIZEBOX = &H20000

            ''' <summary>The window is an overlapped window. An overlapped window has a title bar and a border.</summary>
            WS_OVERLAPPED = &H0

            ''' <summary>The window is an overlapped window.</summary>
            WS_OVERLAPPEDWINDOW = WS_OVERLAPPED Or WS_CAPTION Or WS_SYSMENU Or WS_SIZEFRAME Or WS_MINIMIZEBOX Or WS_MAXIMIZEBOX

            ''' <summary>The window is a pop-up window. This style cannot be used with the WS_CHILD style.</summary>
            WS_POPUP = &H80000000UI

            ''' <summary>The window is a pop-up window. The WS_CAPTION and WS_POPUPWINDOW styles must be combined to make the window menu visible.</summary>
            WS_POPUPWINDOW = WS_POPUP Or WS_BORDER Or WS_SYSMENU

            ''' <summary>The window has a sizing border.</summary>
            WS_SIZEFRAME = &H40000

            ''' <summary>The window has a window menu on its title bar. The WS_CAPTION style must also be specified.</summary>
            WS_SYSMENU = &H80000

            ''' <summary>
            ''' The window is a control that can receive the keyboard focus when the user presses the TAB key.
            ''' Pressing the TAB key changes the keyboard focus to the next control with the WS_TABSTOP style.  
            ''' You can turn this style on and off to change dialog box navigation. To change this style after a window has been created, use the SetWindowLong function.
            ''' For user-created windows and modeless dialogs to work with tab stops, alter the message loop to call the IsDialogMessage function.
            ''' </summary>
            WS_TABSTOP = &H10000

            ''' <summary>The window is initially visible. This style can be turned on and off by using the ShowWindow or SetWindowPos function.</summary>
            WS_VISIBLE = &H10000000

            ''' <summary>The window has a vertical scroll bar.</summary>
            WS_VSCROLL = &H200000
        End Enum
        <Flags()> _
        Public Enum WindowStylesEx As UInteger
            ''' <summary>
            ''' Specifies that a window created with this style accepts drag-drop files.
            ''' </summary>
            WS_EX_ACCEPTFILES = &H10
            ''' <summary>
            ''' Forces a top-level window onto the taskbar when the window is visible.
            ''' </summary>
            WS_EX_APPWINDOW = &H40000
            ''' <summary>
            ''' Specifies that a window has a border with a sunken edge.
            ''' </summary>
            WS_EX_CLIENTEDGE = &H200
            ''' <summary>
            ''' Windows XP: Paints all descendants of a window in bottom-to-top painting order using double-buffering. For more information, see Remarks. This cannot be used if the window has a class style of either CS_OWNDC or CS_CLASSDC. 
            ''' </summary>
            WS_EX_COMPOSITED = &H2000000
            ''' <summary>
            ''' Includes a question mark in the title bar of the window. When the user clicks the question mark, the cursor changes to a question mark with a pointer. If the user then clicks a child window, the child receives a WM_HELP message. The child window should pass the message to the parent window procedure, which should call the WinHelp function using the HELP_WM_HELP command. The Help application displays a pop-up window that typically contains help for the child window.
            ''' WS_EX_CONTEXTHELP cannot be used with the WS_MAXIMIZEBOX or WS_MINIMIZEBOX styles.
            ''' </summary>
            WS_EX_CONTEXTHELP = &H400
            ''' <summary>
            ''' The window itself contains child windows that should take part in dialog box navigation. If this style is specified, the dialog manager recurses into children of this window when performing navigation operations such as handling the TAB key, an arrow key, or a keyboard mnemonic.
            ''' </summary>
            WS_EX_CONTROLPARENT = &H10000
            ''' <summary>
            ''' Creates a window that has a double border; the window can, optionally, be created with a title bar by specifying the WS_CAPTION style in the dwStyle parameter.
            ''' </summary>
            WS_EX_DLGMODALFRAME = &H1
            ''' <summary>
            ''' Windows 2000/XP: Creates a layered window. Note that this cannot be used for child windows. Also, this cannot be used if the window has a class style of either CS_OWNDC or CS_CLASSDC. 
            ''' </summary>
            WS_EX_LAYERED = &H80000
            ''' <summary>
            ''' Arabic and Hebrew versions of Windows 98/Me, Windows 2000/XP: Creates a window whose horizontal origin is on the right edge. Increasing horizontal values advance to the left. 
            ''' </summary>
            WS_EX_LAYOUTRTL = &H400000
            ''' <summary>
            ''' Creates a window that has generic left-aligned properties. This is the default.
            ''' </summary>
            WS_EX_LEFT = &H0
            ''' <summary>
            ''' If the shell language is Hebrew, Arabic, or another language that supports reading order alignment, the vertical scroll bar (if present) is to the left of the client area. For other languages, the style is ignored.
            ''' </summary>
            WS_EX_LEFTSCROLLBAR = &H4000
            ''' <summary>
            ''' The window text is displayed using left-to-right reading-order properties. This is the default.
            ''' </summary>
            WS_EX_LTRREADING = &H0
            ''' <summary>
            ''' Creates a multiple-document interface (MDI) child window.
            ''' </summary>
            WS_EX_MDICHILD = &H40
            ''' <summary>
            ''' Windows 2000/XP: A top-level window created with this style does not become the foreground window when the user clicks it. The system does not bring this window to the foreground when the user minimizes or closes the foreground window. 
            ''' To activate the window, use the SetActiveWindow or SetForegroundWindow function.
            ''' The window does not appear on the taskbar by default. To force the window to appear on the taskbar, use the WS_EX_APPWINDOW style.
            ''' </summary>
            WS_EX_NOACTIVATE = &H8000000
            ''' <summary>
            ''' Windows 2000/XP: A window created with this style does not pass its window layout to its child windows.
            ''' </summary>
            WS_EX_NOINHERITLAYOUT = &H100000
            ''' <summary>
            ''' Specifies that a child window created with this style does not send the WM_PARENTNOTIFY message to its parent window when it is created or destroyed.
            ''' </summary>
            WS_EX_NOPARENTNOTIFY = &H4
            ''' <summary>
            ''' Combines the WS_EX_CLIENTEDGE and WS_EX_WINDOWEDGE styles.
            ''' </summary>
            WS_EX_OVERLAPPEDWINDOW = WS_EX_WINDOWEDGE Or WS_EX_CLIENTEDGE
            ''' <summary>
            ''' Combines the WS_EX_WINDOWEDGE, WS_EX_TOOLWINDOW, and WS_EX_TOPMOST styles.
            ''' </summary>
            WS_EX_PALETTEWINDOW = WS_EX_WINDOWEDGE Or WS_EX_TOOLWINDOW Or WS_EX_TOPMOST
            ''' <summary>
            ''' The window has generic "right-aligned" properties. This depends on the window class. This style has an effect only if the shell language is Hebrew, Arabic, or another language that supports reading-order alignment; otherwise, the style is ignored.
            ''' Using the WS_EX_RIGHT style for static or edit controls has the same effect as using the SS_RIGHT or ES_RIGHT style, respectively. Using this style with button controls has the same effect as using BS_RIGHT and BS_RIGHTBUTTON styles.
            ''' </summary>
            WS_EX_RIGHT = &H1000
            ''' <summary>
            ''' Vertical scroll bar (if present) is to the right of the client area. This is the default.
            ''' </summary>
            WS_EX_RIGHTSCROLLBAR = &H0
            ''' <summary>
            ''' If the shell language is Hebrew, Arabic, or another language that supports reading-order alignment, the window text is displayed using right-to-left reading-order properties. For other languages, the style is ignored.
            ''' </summary>
            WS_EX_RTLREADING = &H2000
            ''' <summary>
            ''' Creates a window with a three-dimensional border style intended to be used for items that do not accept user input.
            ''' </summary>
            WS_EX_STATICEDGE = &H20000
            ''' <summary>
            ''' Creates a tool window; that is, a window intended to be used as a floating toolbar. A tool window has a title bar that is shorter than a normal title bar, and the window title is drawn using a smaller font. A tool window does not appear in the taskbar or in the dialog that appears when the user presses ALT+TAB. If a tool window has a system menu, its icon is not displayed on the title bar. However, you can display the system menu by right-clicking or by typing ALT+SPACE. 
            ''' </summary>
            WS_EX_TOOLWINDOW = &H80
            ''' <summary>
            ''' Specifies that a window created with this style should be placed above all non-topmost windows and should stay above them, even when the window is deactivated. To add or remove this style, use the SetWindowPos function.
            ''' </summary>
            WS_EX_TOPMOST = &H8
            ''' <summary>
            ''' Specifies that a window created with this style should not be painted until siblings beneath the window (that were created by the same thread) have been painted. The window appears transparent because the bits of underlying sibling windows have already been painted.
            ''' To achieve transparency without these restrictions, use the SetWindowRgn function.
            ''' </summary>
            WS_EX_TRANSPARENT = &H20
            ''' <summary>
            ''' Specifies that a window has a border with a raised edge.
            ''' </summary>
            WS_EX_WINDOWEDGE = &H100
        End Enum
        <Flags()> _
        Public Enum WindowsMessages As UInteger
            ''' <summary>The WM_ACTIVATE message is sent when a window is being activated or deactivated. This message is sent first to the window procedure of the top-level window being deactivated; it is then sent to the window procedure of the top-level window being activated.</summary>
            WM_ACTIVATE = &H6
            ''' <summary>The WM_ACTIVATEAPP message is sent when a window belonging to a different application than the active window is about to be activated. The message is sent to the application whose window is being activated and to the application whose window is being deactivated.</summary>
            WM_ACTIVATEAPP = &H1C
            ''' <summary>The WM_AFXFIRST specifies the first afx message.</summary>
            WM_AFXFIRST = &H360
            ''' <summary>The WM_AFXFIRST specifies the last afx message.</summary>
            WM_AFXLAST = &H37F
            ''' <summary>The WM_APP constant is used by applications to help define private messages, usually of the form WM_APP+X, where X is an integer value.</summary>
            WM_APP = &H8000
            ''' <summary>The WM_ASKCBFORMATNAME message is sent to the clipboard owner by a clipboard viewer window to request the name of a CF_OWNERDISPLAY clipboard format.</summary>
            WM_ASKCBFORMATNAME = &H30C
            ''' <summary>The WM_CANCELJOURNAL message is posted to an application when a user cancels the application's journaling activities. The message is posted with a NULL window handle.</summary>
            WM_CANCELJOURNAL = &H4B
            ''' <summary>The WM_CANCELMODE message is sent to cancel certain modes, such as mouse capture. For example, the system sends this message to the active window when a dialog box or message box is displayed. Certain functions also send this message explicitly to the specified window regardless of whether it is the active window. For example, the EnableWindow function sends this message when disabling the specified window.</summary>
            WM_CANCELMODE = &H1F
            ''' <summary>The WM_CAPTURECHANGED message is sent to the window that is losing the mouse capture.</summary>
            WM_CAPTURECHANGED = &H215
            ''' <summary>The WM_CHANGECBCHAIN message is sent to the first window in the clipboard viewer chain when a window is being removed from the chain.</summary>
            WM_CHANGECBCHAIN = &H30D
            ''' <summary>An application sends the WM_CHANGEUISTATE message to indicate that the user interface (UI) state should be changed.</summary>
            WM_CHANGEUISTATE = &H127
            ''' <summary>The WM_CHAR message is posted to the window with the keyboard focus when a WM_KEYDOWN message is translated by the TranslateMessage function. The WM_CHAR message contains the character code of the key that was pressed.</summary>
            WM_CHAR = &H102
            ''' <summary>Sent by a list box with the LBS_WANTKEYBOARDINPUT style to its owner in response to a WM_CHAR message.</summary>
            WM_CHARTOITEM = &H2F
            ''' <summary>The WM_CHILDACTIVATE message is sent to a child window when the user clicks the window's title bar or when the window is activated, moved, or sized.</summary>
            WM_CHILDACTIVATE = &H22
            ''' <summary>An application sends a WM_CLEAR message to an edit control or combo box to delete (clear) the current selection, if any, from the edit control.</summary>
            WM_CLEAR = &H303
            ''' <summary>The WM_CLOSE message is sent as a signal that a window or an application should terminate.</summary>
            WM_CLOSE = &H10
            ''' <summary>The WM_COMMAND message is sent when the user selects a command item from a menu, when a control sends a notification message to its parent window, or when an accelerator keystroke is translated.</summary>
            WM_COMMAND = &H111
            ''' <summary>The WM_COMPACTING message is sent to all top-level windows when the system detects more than 12.5 percent of system time over a 30- to 60-second interval is being spent compacting memory. This indicates that system memory is low.</summary>
            WM_COMPACTING = &H41
            ''' <summary>The system sends the WM_COMPAREITEM message to determine the relative position of a new item in the sorted list of an owner-drawn combo box or list box. Whenever the application adds a new item, the system sends this message to the owner of a combo box or list box created with the CBS_SORT or LBS_SORT style.</summary>
            WM_COMPAREITEM = &H39
            ''' <summary>The WM_CONTEXTMENU message notifies a window that the user clicked the right mouse button (right-clicked) in the window.</summary>
            WM_CONTEXTMENU = &H7B
            ''' <summary>An application sends the WM_COPY message to an edit control or combo box to copy the current selection to the clipboard in CF_TEXT format.</summary>
            WM_COPY = &H301
            ''' <summary>An application sends the WM_COPYDATA message to pass data to another application.</summary>
            WM_COPYDATA = &H4A
            ''' <summary>The WM_CREATE message is sent when an application requests that a window be created by calling the CreateWindowEx or CreateWindow function. (The message is sent before the function returns.) The window procedure of the new window receives this message after the window is created, but before the window becomes visible.</summary>
            WM_CREATE = &H1
            ''' <summary>The WM_CTLCOLORBTN message is sent to the parent window of a button before drawing the button. The parent window can change the button's text and background colors. However, only owner-drawn buttons respond to the parent window processing this message.</summary>
            WM_CTLCOLORBTN = &H135
            ''' <summary>The WM_CTLCOLORDLG message is sent to a dialog box before the system draws the dialog box. By responding to this message, the dialog box can set its text and background colors using the specified display device context handle.</summary>
            WM_CTLCOLORDLG = &H136
            ''' <summary>An edit control that is not read-only or disabled sends the WM_CTLCOLOREDIT message to its parent window when the control is about to be drawn. By responding to this message, the parent window can use the specified device context handle to set the text and background colors of the edit control.</summary>
            WM_CTLCOLOREDIT = &H133
            ''' <summary>Sent to the parent window of a list box before the system draws the list box. By responding to this message, the parent window can set the text and background colors of the list box by using the specified display device context handle.</summary>
            WM_CTLCOLORLISTBOX = &H134
            ''' <summary>The WM_CTLCOLORMSGBOX message is sent to the owner window of a message box before Windows draws the message box. By responding to this message, the owner window can set the text and background colors of the message box by using the given display device context handle.</summary>
            WM_CTLCOLORMSGBOX = &H132
            ''' <summary>The WM_CTLCOLORSCROLLBAR message is sent to the parent window of a scroll bar control when the control is about to be drawn. By responding to this message, the parent window can use the display context handle to set the background color of the scroll bar control.</summary>
            WM_CTLCOLORSCROLLBAR = &H137
            ''' <summary>A static control, or an edit control that is read-only or disabled, sends the WM_CTLCOLORSTATIC message to its parent window when the control is about to be drawn. By responding to this message, the parent window can use the specified device context handle to set the text and background colors of the static control.</summary>
            WM_CTLCOLORSTATIC = &H138
            ''' <summary>An application sends a WM_CUT message to an edit control or combo box to delete (cut) the current selection, if any, in the edit control and copy the deleted text to the clipboard in CF_TEXT format.</summary>
            WM_CUT = &H300
            ''' <summary>The WM_DEADCHAR message is posted to the window with the keyboard focus when a WM_KEYUP message is translated by the TranslateMessage function. WM_DEADCHAR specifies a character code generated by a dead key. A dead key is a key that generates a character, such as the umlaut (double-dot), that is combined with another character to form a composite character. For example, the umlaut-O character (Ö) is generated by typing the dead key for the umlaut character, and then typing the O key.</summary>
            WM_DEADCHAR = &H103
            ''' <summary>Sent to the owner of a list box or combo box when the list box or combo box is destroyed or when items are removed by the LB_DELETESTRING, LB_RESETCONTENT, CB_DELETESTRING, or CB_RESETCONTENT message. The system sends a WM_DELETEITEM message for each deleted item. The system sends the WM_DELETEITEM message for any deleted list box or combo box item with nonzero item data.</summary>
            WM_DELETEITEM = &H2D
            ''' <summary>The WM_DESTROY message is sent when a window is being destroyed. It is sent to the window procedure of the window being destroyed after the window is removed from the screen. This message is sent first to the window being destroyed and then to the child windows (if any) as they are destroyed. During the processing of the message, it can be assumed that all child windows still exist.</summary>
            WM_DESTROY = &H2
            ''' <summary>The WM_DESTROYCLIPBOARD message is sent to the clipboard owner when a call to the EmptyClipboard function empties the clipboard.</summary>
            WM_DESTROYCLIPBOARD = &H307
            ''' <summary>Notifies an application of a change to the hardware configuration of a device or the computer.</summary>
            WM_DEVICECHANGE = &H219
            ''' <summary>The WM_DEVMODECHANGE message is sent to all top-level windows whenever the user changes device-mode settings.</summary>
            WM_DEVMODECHANGE = &H1B
            ''' <summary>The WM_DISPLAYCHANGE message is sent to all windows when the display resolution has changed.</summary>
            WM_DISPLAYCHANGE = &H7E
            ''' <summary>The WM_DRAWCLIPBOARD message is sent to the first window in the clipboard viewer chain when the content of the clipboard changes. This enables a clipboard viewer window to display the new content of the clipboard.</summary>
            WM_DRAWCLIPBOARD = &H308
            ''' <summary>The WM_DRAWITEM message is sent to the parent window of an owner-drawn button, combo box, list box, or menu when a visual aspect of the button, combo box, list box, or menu has changed.</summary>
            WM_DRAWITEM = &H2B
            ''' <summary>Sent when the user drops a file on the window of an application that has registered itself as a recipient of dropped files.</summary>
            WM_DROPFILES = &H233
            ''' <summary>The WM_ENABLE message is sent when an application changes the enabled state of a window. It is sent to the window whose enabled state is changing. This message is sent before the EnableWindow function returns, but after the enabled state (WS_DISABLED style bit) of the window has changed.</summary>
            WM_ENABLE = &HA
            ''' <summary>The WM_ENDSESSION message is sent to an application after the system processes the results of the WM_QUERYENDSESSION message. The WM_ENDSESSION message informs the application whether the session is ending.</summary>
            WM_ENDSESSION = &H16
            ''' <summary>The WM_ENTERIDLE message is sent to the owner window of a modal dialog box or menu that is entering an idle state. A modal dialog box or menu enters an idle state when no messages are waiting in its queue after it has processed one or more previous messages.</summary>
            WM_ENTERIDLE = &H121
            ''' <summary>The WM_ENTERMENULOOP message informs an application's main window procedure that a menu modal loop has been entered.</summary>
            WM_ENTERMENULOOP = &H211
            ''' <summary>The WM_ENTERSIZEMOVE message is sent one time to a window after it enters the moving or sizing modal loop. The window enters the moving or sizing modal loop when the user clicks the window's title bar or sizing border, or when the window passes the WM_SYSCOMMAND message to the DefWindowProc function and the wParam parameter of the message specifies the SC_MOVE or SC_SIZE value. The operation is complete when DefWindowProc returns.</summary>
            WM_ENTERSIZEMOVE = &H231
            ''' <summary>The WM_ERASEBKGND message is sent when the window background must be erased (for example, when a window is resized). The message is sent to prepare an invalidated portion of a window for painting.</summary>
            WM_ERASEBKGND = &H14
            ''' <summary>The WM_EXITMENULOOP message informs an application's main window procedure that a menu modal loop has been exited.</summary>
            WM_EXITMENULOOP = &H212
            ''' <summary>The WM_EXITSIZEMOVE message is sent one time to a window, after it has exited the moving or sizing modal loop. The window enters the moving or sizing modal loop when the user clicks the window's title bar or sizing border, or when the window passes the WM_SYSCOMMAND message to the DefWindowProc function and the wParam parameter of the message specifies the SC_MOVE or SC_SIZE value. The operation is complete when DefWindowProc returns.</summary>
            WM_EXITSIZEMOVE = &H232
            ''' <summary>An application sends the WM_FONTCHANGE message to all top-level windows in the system after changing the pool of font resources.</summary>
            WM_FONTCHANGE = &H1D
            ''' <summary>The WM_GETDLGCODE message is sent to the window procedure associated with a control. By default, the system handles all keyboard input to the control; the system interprets certain types of keyboard input as dialog box navigation keys. To override this default behavior, the control can respond to the WM_GETDLGCODE message to indicate the types of input it wants to process itself.</summary>
            WM_GETDLGCODE = &H87
            ''' <summary>An application sends a WM_GETFONT message to a control to retrieve the font with which the control is currently drawing its text.</summary>
            WM_GETFONT = &H31
            ''' <summary>An application sends a WM_GETHOTKEY message to determine the hot key associated with a window.</summary>
            WM_GETHOTKEY = &H33
            ''' <summary>The WM_GETICON message is sent to a window to retrieve a handle to the large or small icon associated with a window. The system displays the large icon in the ALT+TAB dialog, and the small icon in the window caption.</summary>
            WM_GETICON = &H7F
            ''' <summary>The WM_GETMINMAXINFO message is sent to a window when the size or position of the window is about to change. An application can use this message to override the window's default maximized size and position, or its default minimum or maximum tracking size.</summary>
            WM_GETMINMAXINFO = &H24
            ''' <summary>Active Accessibility sends the WM_GETOBJECT message to obtain information about an accessible object contained in a server application. Applications never send this message directly. It is sent only by Active Accessibility in response to calls to AccessibleObjectFromPoint, AccessibleObjectFromEvent, or AccessibleObjectFromWindow. However, server applications handle this message.</summary>     
            WM_GETOBJECT = &H3D
            ''' <summary>An application sends a WM_GETTEXT message to copy the text that corresponds to a window into a buffer provided by the caller.</summary>
            WM_GETTEXT = &HD
            ''' <summary>An application sends a WM_GETTEXTLENGTH message to determine the length, in characters, of the text associated with a window.</summary>
            WM_GETTEXTLENGTH = &HE
            ''' <summary>Definition Needed</summary>
            WM_HANDHELDFIRST = &H358
            ''' <summary>Definition Needed</summary>
            WM_HANDHELDLAST = &H35F
            ''' <summary>Indicates that the user pressed the F1 key. If a menu is active when F1 is pressed, WM_HELP is sent to the window associated with the menu; otherwise, WM_HELP is sent to the window that has the keyboard focus. If no window has the keyboard focus, WM_HELP is sent to the currently active window.</summary>
            WM_HELP = &H53
            ''' <summary>The WM_HOTKEY message is posted when the user presses a hot key registered by the RegisterHotKey function. The message is placed at the top of the message queue associated with the thread that registered the hot key.</summary>
            WM_HOTKEY = &H312
            ''' <summary>This message is sent to a window when a scroll event occurs in the window's standard horizontal scroll bar. This message is also sent to the owner of a horizontal scroll bar control when a scroll event occurs in the control.</summary>
            WM_HSCROLL = &H114
            ''' <summary>The WM_HSCROLLCLIPBOARD message is sent to the clipboard owner by a clipboard viewer window. This occurs when the clipboard contains data in the CF_OWNERDISPLAY format and an event occurs in the clipboard viewer's horizontal scroll bar. The owner should scroll the clipboard image and update the scroll bar values.</summary>
            WM_HSCROLLCLIPBOARD = &H30E
            ''' <summary>Windows NT 3.51 and earlier: The WM_ICONERASEBKGND message is sent to a minimized window when the background of the icon must be filled before painting the icon. A window receives this message only if a class icon is defined for the window; otherwise, WM_ERASEBKGND is sent. This message is not sent by newer versions of Windows.</summary>
            WM_ICONERASEBKGND = &H27
            ''' <summary>Sent to an application when the IME gets a character of the conversion result. A window receives this message through its WindowProc function.</summary>
            WM_IME_CHAR = &H286
            ''' <summary>Sent to an application when the IME changes composition status as a result of a keystroke. A window receives this message through its WindowProc function.</summary>
            WM_IME_COMPOSITION = &H10F
            ''' <summary>Sent to an application when the IME window finds no space to extend the area for the composition window. A window receives this message through its WindowProc function.</summary>
            WM_IME_COMPOSITIONFULL = &H284
            ''' <summary>Sent by an application to direct the IME window to carry out the requested command. The application uses this message to control the IME window that it has created. To send this message, the application calls the SendMessage function with the following parameters.</summary>
            WM_IME_CONTROL = &H283
            ''' <summary>Sent to an application when the IME ends composition. A window receives this message through its WindowProc function.</summary>
            WM_IME_ENDCOMPOSITION = &H10E
            ''' <summary>Sent to an application by the IME to notify the application of a key press and to keep message order. A window receives this message through its WindowProc function.</summary>
            WM_IME_KEYDOWN = &H290
            ''' <summary>Definition Needed</summary>
            WM_IME_KEYLAST = &H10F
            ''' <summary>Sent to an application by the IME to notify the application of a key release and to keep message order. A window receives this message through its WindowProc function.</summary>
            WM_IME_KEYUP = &H291
            ''' <summary>Sent to an application to notify it of changes to the IME window. A window receives this message through its WindowProc function.</summary>
            WM_IME_NOTIFY = &H282
            ''' <summary>Sent to an application to provide commands and request information. A window receives this message through its WindowProc function.</summary>
            WM_IME_REQUEST = &H288
            ''' <summary>Sent to an application when the operating system is about to change the current IME. A window receives this message through its WindowProc function.</summary>
            WM_IME_SELECT = &H285
            ''' <summary>Sent to an application when a window is activated. A window receives this message through its WindowProc function.</summary>
            WM_IME_SETCONTEXT = &H281
            ''' <summary>Sent immediately before the IME generates the composition string as a result of a keystroke. A window receives this message through its WindowProc function.</summary>
            WM_IME_STARTCOMPOSITION = &H10D
            ''' <summary>The WM_INITDIALOG message is sent to the dialog box procedure immediately before a dialog box is displayed. Dialog box procedures typically use this message to initialize controls and carry out any other initialization tasks that affect the appearance of the dialog box.</summary>
            WM_INITDIALOG = &H110
            ''' <summary>The WM_INITMENU message is sent when a menu is about to become active. It occurs when the user clicks an item on the menu bar or presses a menu key. This allows the application to modify the menu before it is displayed.</summary>
            WM_INITMENU = &H116
            ''' <summary>The WM_INITMENUPOPUP message is sent when a drop-down menu or submenu is about to become active. This allows an application to modify the menu before it is displayed, without changing the entire menu.</summary>
            WM_INITMENUPOPUP = &H117
            ''' <summary>The WM_INPUTLANGCHANGE message is sent to the topmost affected window after an application's input language has been changed. You should make any application-specific settings and pass the message to the DefWindowProc function, which passes the message to all first-level child windows. These child windows can pass the message to DefWindowProc to have it pass the message to their child windows, and so on.</summary>
            WM_INPUTLANGCHANGE = &H51
            ''' <summary>The WM_INPUTLANGCHANGEREQUEST message is posted to the window with the focus when the user chooses a new input language, either with the hotkey (specified in the Keyboard control panel application) or from the indicator on the system taskbar. An application can accept the change by passing the message to the DefWindowProc function or reject the change (and prevent it from taking place) by returning immediately.</summary>
            WM_INPUTLANGCHANGEREQUEST = &H50
            ''' <summary>The WM_KEYDOWN message is posted to the window with the keyboard focus when a nonsystem key is pressed. A nonsystem key is a key that is pressed when the ALT key is not pressed.</summary>
            WM_KEYDOWN = &H100
            ''' <summary>This message filters for keyboard messages.</summary>
            WM_KEYFIRST = &H100
            ''' <summary>This message filters for keyboard messages.</summary>
            WM_KEYLAST = &H108
            ''' <summary>The WM_KEYUP message is posted to the window with the keyboard focus when a nonsystem key is released. A nonsystem key is a key that is pressed when the ALT key is not pressed, or a keyboard key that is pressed when a window has the keyboard focus.</summary>
            WM_KEYUP = &H101
            ''' <summary>The WM_KILLFOCUS message is sent to a window immediately before it loses the keyboard focus.</summary>
            WM_KILLFOCUS = &H8
            ''' <summary>The WM_LBUTTONDBLCLK message is posted when the user double-clicks the left mouse button while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse.</summary>
            WM_LBUTTONDBLCLK = &H203
            ''' <summary>The WM_LBUTTONDOWN message is posted when the user presses the left mouse button while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse.</summary>
            WM_LBUTTONDOWN = &H201
            ''' <summary>The WM_LBUTTONUP message is posted when the user releases the left mouse button while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse.</summary>
            WM_LBUTTONUP = &H202
            ''' <summary>The WM_MBUTTONDBLCLK message is posted when the user double-clicks the middle mouse button while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse.</summary>
            WM_MBUTTONDBLCLK = &H209
            ''' <summary>The WM_MBUTTONDOWN message is posted when the user presses the middle mouse button while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse.</summary>
            WM_MBUTTONDOWN = &H207
            ''' <summary>The WM_MBUTTONUP message is posted when the user releases the middle mouse button while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse.</summary>
            WM_MBUTTONUP = &H208
            ''' <summary>An application sends the WM_MDIACTIVATE message to a multiple-document interface (MDI) client window to instruct the client window to activate a different MDI child window.</summary>
            WM_MDIACTIVATE = &H222
            ''' <summary>An application sends the WM_MDICASCADE message to a multiple-document interface (MDI) client window to arrange all its child windows in a cascade format.</summary>
            WM_MDICASCADE = &H227
            ''' <summary>An application sends the WM_MDICREATE message to a multiple-document interface (MDI) client window to create an MDI child window.</summary>
            WM_MDICREATE = &H220
            ''' <summary>An application sends the WM_MDIDESTROY message to a multiple-document interface (MDI) client window to close an MDI child window.</summary>
            WM_MDIDESTROY = &H221
            ''' <summary>An application sends the WM_MDIGETACTIVE message to a multiple-document interface (MDI) client window to retrieve the handle to the active MDI child window.</summary>
            WM_MDIGETACTIVE = &H229
            ''' <summary>An application sends the WM_MDIICONARRANGE message to a multiple-document interface (MDI) client window to arrange all minimized MDI child windows. It does not affect child windows that are not minimized.</summary>
            WM_MDIICONARRANGE = &H228
            ''' <summary>An application sends the WM_MDIMAXIMIZE message to a multiple-document interface (MDI) client window to maximize an MDI child window. The system resizes the child window to make its client area fill the client window. The system places the child window's window menu icon in the rightmost position of the frame window's menu bar, and places the child window's restore icon in the leftmost position. The system also appends the title bar text of the child window to that of the frame window.</summary>
            WM_MDIMAXIMIZE = &H225
            ''' <summary>An application sends the WM_MDINEXT message to a multiple-document interface (MDI) client window to activate the next or previous child window.</summary>
            WM_MDINEXT = &H224
            ''' <summary>An application sends the WM_MDIREFRESHMENU message to a multiple-document interface (MDI) client window to refresh the window menu of the MDI frame window.</summary>
            WM_MDIREFRESHMENU = &H234
            ''' <summary>An application sends the WM_MDIRESTORE message to a multiple-document interface (MDI) client window to restore an MDI child window from maximized or minimized size.</summary>
            WM_MDIRESTORE = &H223
            ''' <summary>An application sends the WM_MDISETMENU message to a multiple-document interface (MDI) client window to replace the entire menu of an MDI frame window, to replace the window menu of the frame window, or both.</summary>
            WM_MDISETMENU = &H230
            ''' <summary>An application sends the WM_MDITILE message to a multiple-document interface (MDI) client window to arrange all of its MDI child windows in a tile format.</summary>
            WM_MDITILE = &H226
            ''' <summary>The WM_MEASUREITEM message is sent to the owner window of a combo box, list box, list view control, or menu item when the control or menu is created.</summary>
            WM_MEASUREITEM = &H2C
            ''' <summary>The WM_MENUCHAR message is sent when a menu is active and the user presses a key that does not correspond to any mnemonic or accelerator key. This message is sent to the window that owns the menu.</summary>
            WM_MENUCHAR = &H120
            ''' <summary>The WM_MENUCOMMAND message is sent when the user makes a selection from a menu.</summary>
            WM_MENUCOMMAND = &H126
            ''' <summary>The WM_MENUDRAG message is sent to the owner of a drag-and-drop menu when the user drags a menu item.</summary>
            WM_MENUDRAG = &H123
            ''' <summary>The WM_MENUGETOBJECT message is sent to the owner of a drag-and-drop menu when the mouse cursor enters a menu item or moves from the center of the item to the top or bottom of the item.</summary>
            WM_MENUGETOBJECT = &H124
            ''' <summary>The WM_MENURBUTTONUP message is sent when the user releases the right mouse button while the cursor is on a menu item.</summary>
            WM_MENURBUTTONUP = &H122
            ''' <summary>The WM_MENUSELECT message is sent to a menu's owner window when the user selects a menu item.</summary>
            WM_MENUSELECT = &H11F
            ''' <summary>The WM_MOUSEACTIVATE message is sent when the cursor is in an inactive window and the user presses a mouse button. The parent window receives this message only if the child window passes it to the DefWindowProc function.</summary>
            WM_MOUSEACTIVATE = &H21
            ''' <summary>Use WM_MOUSEFIRST to specify the first mouse message. Use the PeekMessage() Function.</summary>
            WM_MOUSEFIRST = &H200
            ''' <summary>The WM_MOUSEHOVER message is posted to a window when the cursor hovers over the client area of the window for the period of time specified in a prior call to TrackMouseEvent.</summary>
            WM_MOUSEHOVER = &H2A1
            ''' <summary>Definition Needed</summary>
            WM_MOUSELAST = &H20D
            ''' <summary>The WM_MOUSELEAVE message is posted to a window when the cursor leaves the client area of the window specified in a prior call to TrackMouseEvent.</summary>
            WM_MOUSELEAVE = &H2A3
            ''' <summary>The WM_MOUSEMOVE message is posted to a window when the cursor moves. If the mouse is not captured, the message is posted to the window that contains the cursor. Otherwise, the message is posted to the window that has captured the mouse.</summary>
            WM_MOUSEMOVE = &H200
            ''' <summary>The WM_MOUSEWHEEL message is sent to the focus window when the mouse wheel is rotated. The DefWindowProc function propagates the message to the window's parent. There should be no internal forwarding of the message, since DefWindowProc propagates it up the parent chain until it finds a window that processes it.</summary>
            WM_MOUSEWHEEL = &H20A
            ''' <summary>The WM_MOUSEHWHEEL message is sent to the focus window when the mouse's horizontal scroll wheel is tilted or rotated. The DefWindowProc function propagates the message to the window's parent. There should be no internal forwarding of the message, since DefWindowProc propagates it up the parent chain until it finds a window that processes it.</summary>
            WM_MOUSEHWHEEL = &H20E
            ''' <summary>The WM_MOVE message is sent after a window has been moved.</summary>
            WM_MOVE = &H3
            ''' <summary>The WM_MOVING message is sent to a window that the user is moving. By processing this message, an application can monitor the position of the drag rectangle and, if needed, change its position.</summary>
            WM_MOVING = &H216
            ''' <summary>Non Client Area Activated Caption(Title) of the Form</summary>
            WM_NCACTIVATE = &H86
            ''' <summary>The WM_NCCALCSIZE message is sent when the size and position of a window's client area must be calculated. By processing this message, an application can control the content of the window's client area when the size or position of the window changes.</summary>
            WM_NCCALCSIZE = &H83
            ''' <summary>The WM_NCCREATE message is sent prior to the WM_CREATE message when a window is first created.</summary>
            WM_NCCREATE = &H81
            ''' <summary>The WM_NCDESTROY message informs a window that its nonclient area is being destroyed. The DestroyWindow function sends the WM_NCDESTROY message to the window following the WM_DESTROY message. WM_DESTROY is used to free the allocated memory object associated with the window.</summary>
            WM_NCDESTROY = &H82
            ''' <summary>The WM_NCHITTEST message is sent to a window when the cursor moves, or when a mouse button is pressed or released. If the mouse is not captured, the message is sent to the window beneath the cursor. Otherwise, the message is sent to the window that has captured the mouse.</summary>
            WM_NCHITTEST = &H84
            ''' <summary>The WM_NCLBUTTONDBLCLK message is posted when the user double-clicks the left mouse button while the cursor is within the nonclient area of a window. This message is posted to the window that contains the cursor. If a window has captured the mouse, this message is not posted.</summary>
            WM_NCLBUTTONDBLCLK = &HA3
            ''' <summary>The WM_NCLBUTTONDOWN message is posted when the user presses the left mouse button while the cursor is within the nonclient area of a window. This message is posted to the window that contains the cursor. If a window has captured the mouse, this message is not posted.</summary>
            WM_NCLBUTTONDOWN = &HA1
            ''' <summary>The WM_NCLBUTTONUP message is posted when the user releases the left mouse button while the cursor is within the nonclient area of a window. This message is posted to the window that contains the cursor. If a window has captured the mouse, this message is not posted.</summary>
            WM_NCLBUTTONUP = &HA2
            ''' <summary>The WM_NCMBUTTONDBLCLK message is posted when the user double-clicks the middle mouse button while the cursor is within the nonclient area of a window. This message is posted to the window that contains the cursor. If a window has captured the mouse, this message is not posted.</summary>
            WM_NCMBUTTONDBLCLK = &HA9
            ''' <summary>The WM_NCMBUTTONDOWN message is posted when the user presses the middle mouse button while the cursor is within the nonclient area of a window. This message is posted to the window that contains the cursor. If a window has captured the mouse, this message is not posted.</summary>
            WM_NCMBUTTONDOWN = &HA7
            ''' <summary>The WM_NCMBUTTONUP message is posted when the user releases the middle mouse button while the cursor is within the nonclient area of a window. This message is posted to the window that contains the cursor. If a window has captured the mouse, this message is not posted.</summary>
            WM_NCMBUTTONUP = &HA8
            ''' <summary>The WM_NCMOUSEMOVE message is posted to a window when the cursor is moved within the nonclient area of the window. This message is posted to the window that contains the cursor. If a window has captured the mouse, this message is not posted.</summary>
            WM_NCMOUSEMOVE = &HA0
            ''' <summary>The WM_NCPAINT message is sent to a window when its frame must be painted.</summary>
            WM_NCPAINT = &H85
            ''' <summary>The WM_NCRBUTTONDBLCLK message is posted when the user double-clicks the right mouse button while the cursor is within the nonclient area of a window. This message is posted to the window that contains the cursor. If a window has captured the mouse, this message is not posted.</summary>
            WM_NCRBUTTONDBLCLK = &HA6
            ''' <summary>The WM_NCRBUTTONDOWN message is posted when the user presses the right mouse button while the cursor is within the nonclient area of a window. This message is posted to the window that contains the cursor. If a window has captured the mouse, this message is not posted.</summary>
            WM_NCRBUTTONDOWN = &HA4
            ''' <summary>The WM_NCRBUTTONUP message is posted when the user releases the right mouse button while the cursor is within the nonclient area of a window. This message is posted to the window that contains the cursor. If a window has captured the mouse, this message is not posted.</summary>
            WM_NCRBUTTONUP = &HA5
            ''' <summary>The WM_NEXTDLGCTL message is sent to a dialog box procedure to set the keyboard focus to a different control in the dialog box</summary>
            WM_NEXTDLGCTL = &H28
            ''' <summary>The WM_NEXTMENU message is sent to an application when the right or left arrow key is used to switch between the menu bar and the system menu.</summary>
            WM_NEXTMENU = &H213
            ''' <summary>Sent by a common control to its parent window when an event has occurred or the control requires some information.</summary>
            WM_NOTIFY = &H4E
            ''' <summary>Determines if a window accepts ANSI or Unicode structures in the WM_NOTIFY notification message. WM_NOTIFYFORMAT messages are sent from a common control to its parent window and from the parent window to the common control.</summary>
            WM_NOTIFYFORMAT = &H55
            ''' <summary>The WM_NULL message performs no operation. An application sends the WM_NULL message if it wants to post a message that the recipient window will ignore.</summary>
            WM_NULL = &H0
            ''' <summary>Occurs when the control needs repainting</summary>
            WM_PAINT = &HF
            ''' <summary>The WM_PAINTCLIPBOARD message is sent to the clipboard owner by a clipboard viewer window when the clipboard contains data in the CF_OWNERDISPLAY format and the clipboard viewer's client area needs repainting.</summary>
            WM_PAINTCLIPBOARD = &H309
            ''' <summary>Windows NT 3.51 and earlier: The WM_PAINTICON message is sent to a minimized window when the icon is to be painted. This message is not sent by newer versions of Microsoft Windows, except in unusual circumstances explained in the Remarks.</summary>
            WM_PAINTICON = &H26
            ''' <summary>This message is sent by the OS to all top-level and overlapped windows after the window with the keyboard focus realizes its logical palette. This message enables windows that do not have the keyboard focus to realize their logical palettes and update their client areas.</summary>
            WM_PALETTECHANGED = &H311
            ''' <summary>The WM_PALETTEISCHANGING message informs applications that an application is going to realize its logical palette.</summary>
            WM_PALETTEISCHANGING = &H310
            ''' <summary>The WM_PARENTNOTIFY message is sent to the parent of a child window when the child window is created or destroyed, or when the user clicks a mouse button while the cursor is over the child window. When the child window is being created, the system sends WM_PARENTNOTIFY just before the CreateWindow or CreateWindowEx function that creates the window returns. When the child window is being destroyed, the system sends the message before any processing to destroy the window takes place.</summary>
            WM_PARENTNOTIFY = &H210
            ''' <summary>An application sends a WM_PASTE message to an edit control or combo box to copy the current content of the clipboard to the edit control at the current caret position. Data is inserted only if the clipboard contains data in CF_TEXT format.</summary>
            WM_PASTE = &H302
            ''' <summary>Definition Needed</summary>
            WM_PENWINFIRST = &H380
            ''' <summary>Definition Needed</summary>
            WM_PENWINLAST = &H38F
            ''' <summary>Notifies applications that the system, typically a battery-powered personal computer, is about to enter a suspended mode. Obsolete : use POWERBROADCAST instead</summary>
            WM_POWER = &H48
            ''' <summary>Notifies applications that a power-management event has occurred.</summary>
            WM_POWERBROADCAST = &H218
            ''' <summary>The WM_PRINT message is sent to a window to request that it draw itself in the specified device context, most commonly in a printer device context.</summary>
            WM_PRINT = &H317
            ''' <summary>The WM_PRINTCLIENT message is sent to a window to request that it draw its client area in the specified device context, most commonly in a printer device context.</summary>
            WM_PRINTCLIENT = &H318
            ''' <summary>The WM_QUERYDRAGICON message is sent to a minimized (iconic) window. The window is about to be dragged by the user but does not have an icon defined for its class. An application can return a handle to an icon or cursor. The system displays this cursor or icon while the user drags the icon.</summary>
            WM_QUERYDRAGICON = &H37
            ''' <summary>The WM_QUERYENDSESSION message is sent when the user chooses to end the session or when an application calls one of the system shutdown functions. If any application returns zero, the session is not ended. The system stops sending WM_QUERYENDSESSION messages as soon as one application returns zero. After processing this message, the system sends the WM_ENDSESSION message with the wParam parameter set to the results of the WM_QUERYENDSESSION message.</summary>
            WM_QUERYENDSESSION = &H11
            ''' <summary>This message informs a window that it is about to receive the keyboard focus, giving the window the opportunity to realize its logical palette when it receives the focus.</summary>
            WM_QUERYNEWPALETTE = &H30F
            ''' <summary>The WM_QUERYOPEN message is sent to an icon when the user requests that the window be restored to its previous size and position.</summary>
            WM_QUERYOPEN = &H13
            ''' <summary>The WM_QUEUESYNC message is sent by a computer-based training (CBT) application to separate user-input messages from other messages sent through the WH_JOURNALPLAYBACK Hook procedure.</summary>
            WM_QUEUESYNC = &H23
            ''' <summary>Once received, it ends the application's Message Loop, signaling the application to end. It can be sent by pressing Alt+F4, Clicking the X in the upper right-hand of the program, or going to File->Exit.</summary>
            WM_QUIT = &H12
            ''' <summary>The WM_RBUTTONDBLCLK message is posted when the user double-clicks the right mouse button while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse.</summary>
            WM_RBUTTONDBLCLK = &H206
            ''' <summary>The WM_RBUTTONDOWN message is posted when the user presses the right mouse button while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse.</summary>
            WM_RBUTTONDOWN = &H204
            ''' <summary>The WM_RBUTTONUP message is posted when the user releases the right mouse button while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse.</summary>
            WM_RBUTTONUP = &H205
            ''' <summary>The WM_RENDERALLFORMATS message is sent to the clipboard owner before it is destroyed, if the clipboard owner has delayed rendering one or more clipboard formats. For the content of the clipboard to remain available to other applications, the clipboard owner must render data in all the formats it is capable of generating, and place the data on the clipboard by calling the SetClipboardData function.</summary>
            WM_RENDERALLFORMATS = &H306
            ''' <summary>The WM_RENDERFORMAT message is sent to the clipboard owner if it has delayed rendering a specific clipboard format and if an application has requested data in that format. The clipboard owner must render data in the specified format and place it on the clipboard by calling the SetClipboardData function.</summary>
            WM_RENDERFORMAT = &H305
            ''' <summary>The WM_SETCURSOR message is sent to a window if the mouse causes the cursor to move within a window and mouse input is not captured.</summary>
            WM_SETCURSOR = &H20
            ''' <summary>When the controll got the focus</summary>
            WM_SETFOCUS = &H7
            ''' <summary>An application sends a WM_SETFONT message to specify the font that a control is to use when drawing text.</summary>
            WM_SETFONT = &H30
            ''' <summary>An application sends a WM_SETHOTKEY message to a window to associate a hot key with the window. When the user presses the hot key, the system activates the window.</summary>
            WM_SETHOTKEY = &H32
            ''' <summary>An application sends the WM_SETICON message to associate a new large or small icon with a window. The system displays the large icon in the ALT+TAB dialog box, and the small icon in the window caption.</summary>
            WM_SETICON = &H80
            ''' <summary>An application sends the WM_SETREDRAW message to a window to allow changes in that window to be redrawn or to prevent changes in that window from being redrawn.</summary>
            WM_SETREDRAW = &HB
            ''' <summary>Text / Caption changed on the control. An application sends a WM_SETTEXT message to set the text of a window.</summary>
            WM_SETTEXT = &HC
            ''' <summary>An application sends the WM_SETTINGCHANGE message to all top-level windows after making a change to the WIN.INI file. The SystemParametersInfo function sends this message after an application uses the function to change a setting in WIN.INI.</summary>
            WM_SETTINGCHANGE = &H1A
            ''' <summary>The WM_SHOWWINDOW message is sent to a window when the window is about to be hidden or shown</summary>
            WM_SHOWWINDOW = &H18
            ''' <summary>The WM_SIZE message is sent to a window after its size has changed.</summary>
            WM_SIZE = &H5
            ''' <summary>The WM_SIZECLIPBOARD message is sent to the clipboard owner by a clipboard viewer window when the clipboard contains data in the CF_OWNERDISPLAY format and the clipboard viewer's client area has changed size.</summary>
            WM_SIZECLIPBOARD = &H30B
            ''' <summary>The WM_SIZING message is sent to a window that the user is resizing. By processing this message, an application can monitor the size and position of the drag rectangle and, if needed, change its size or position.</summary>
            WM_SIZING = &H214
            ''' <summary>The WM_SPOOLERSTATUS message is sent from Print Manager whenever a job is added to or removed from the Print Manager queue.</summary>
            WM_SPOOLERSTATUS = &H2A
            ''' <summary>The WM_STYLECHANGED message is sent to a window after the SetWindowLong function has changed one or more of the window's styles.</summary>
            WM_STYLECHANGED = &H7D
            ''' <summary>The WM_STYLECHANGING message is sent to a window when the SetWindowLong function is about to change one or more of the window's styles.</summary>
            WM_STYLECHANGING = &H7C
            ''' <summary>The WM_SYNCPAINT message is used to synchronize painting while avoiding linking independent GUI threads.</summary>
            WM_SYNCPAINT = &H88
            ''' <summary>The WM_SYSCHAR message is posted to the window with the keyboard focus when a WM_SYSKEYDOWN message is translated by the TranslateMessage function. It specifies the character code of a system character key — that is, a character key that is pressed while the ALT key is down.</summary>
            WM_SYSCHAR = &H106
            ''' <summary>This message is sent to all top-level windows when a change is made to a system color setting.</summary>
            WM_SYSCOLORCHANGE = &H15
            ''' <summary>A window receives this message when the user chooses a command from the Window menu (formerly known as the system or control menu) or when the user chooses the maximize button, minimize button, restore button, or close button.</summary>
            WM_SYSCOMMAND = &H112
            ''' <summary>The WM_SYSDEADCHAR message is sent to the window with the keyboard focus when a WM_SYSKEYDOWN message is translated by the TranslateMessage function. WM_SYSDEADCHAR specifies the character code of a system dead key — that is, a dead key that is pressed while holding down the ALT key.</summary>
            WM_SYSDEADCHAR = &H107
            ''' <summary>The WM_SYSKEYDOWN message is posted to the window with the keyboard focus when the user presses the F10 key (which activates the menu bar) or holds down the ALT key and then presses another key. It also occurs when no window currently has the keyboard focus; in this case, the WM_SYSKEYDOWN message is sent to the active window. The window that receives the message can distinguish between these two contexts by checking the context code in the lParam parameter.</summary>
            WM_SYSKEYDOWN = &H104
            ''' <summary>The WM_SYSKEYUP message is posted to the window with the keyboard focus when the user releases a key that was pressed while the ALT key was held down. It also occurs when no window currently has the keyboard focus; in this case, the WM_SYSKEYUP message is sent to the active window. The window that receives the message can distinguish between these two contexts by checking the context code in the lParam parameter.</summary>
            WM_SYSKEYUP = &H105
            ''' <summary>Sent to an application that has initiated a training card with Microsoft Windows Help. The message informs the application when the user clicks an authorable button. An application initiates a training card by specifying the HELP_TCARD command in a call to the WinHelp function.</summary>
            WM_TCARD = &H52
            ''' <summary>A message that is sent whenever there is a change in the system time.</summary>
            WM_TIMECHANGE = &H1E
            ''' <summary>The WM_TIMER message is posted to the installing thread's message queue when a timer expires. The message is posted by the GetMessage or PeekMessage function.</summary>
            WM_TIMER = &H113
            ''' <summary>An application sends a WM_UNDO message to an edit control to undo the last operation. When this message is sent to an edit control, the previously deleted text is restored or the previously added text is deleted.</summary>
            WM_UNDO = &H304
            ''' <summary>The WM_UNINITMENUPOPUP message is sent when a drop-down menu or submenu has been destroyed.</summary>
            WM_UNINITMENUPOPUP = &H125
            ''' <summary>The WM_USER constant is used by applications to help define private messages for use by private window classes, usually of the form WM_USER+X, where X is an integer value.</summary>
            WM_USER = &H400
            ''' <summary>The WM_USERCHANGED message is sent to all windows after the user has logged on or off. When the user logs on or off, the system updates the user-specific settings. The system sends this message immediately after updating the settings.</summary>
            WM_USERCHANGED = &H54
            ''' <summary>Sent by a list box with the LBS_WANTKEYBOARDINPUT style to its owner in response to a WM_KEYDOWN message.</summary>
            WM_VKEYTOITEM = &H2E
            ''' <summary>The WM_VSCROLL message is sent to a window when a scroll event occurs in the window's standard vertical scroll bar. This message is also sent to the owner of a vertical scroll bar control when a scroll event occurs in the control.</summary>
            WM_VSCROLL = &H115
            ''' <summary>The WM_VSCROLLCLIPBOARD message is sent to the clipboard owner by a clipboard viewer window when the clipboard contains data in the CF_OWNERDISPLAY format and an event occurs in the clipboard viewer's vertical scroll bar. The owner should scroll the clipboard image and update the scroll bar values.</summary>
            WM_VSCROLLCLIPBOARD = &H30A
            ''' <summary>The WM_WINDOWPOSCHANGED message is sent to a window whose size, position, or place in the Z order has changed as a result of a call to the SetWindowPos function or another window-management function.</summary>
            WM_WINDOWPOSCHANGED = &H47
            ''' <summary>The WM_WINDOWPOSCHANGING message is sent to a window whose size, position, or place in the Z order is about to change as a result of a call to the SetWindowPos function or another window-management function.</summary>
            WM_WINDOWPOSCHANGING = &H46
            ''' <summary>An application sends the WM_WININICHANGE message to all top-level windows after making a change to the WIN.INI file. The SystemParametersInfo function sends this message after an application uses the function to change a setting in WIN.INI. Note The WM_WININICHANGE message is provided only for compatibility with earlier versions of the system. Applications should use the WM_SETTINGCHANGE message.</summary>
            WM_WININICHANGE = &H1A
            ''' <summary>The WM_XBUTTONDBLCLK message is posted when the user double-clicks the first or second X button while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse.</summary>
            WM_XBUTTONDBLCLK = &H20D
            ''' <summary>The WM_XBUTTONDOWN message is posted when the user presses the first or second X button while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse.</summary>
            WM_XBUTTONDOWN = &H20B
            ''' <summary>The WM_XBUTTONUP message is posted when the user releases the first or second X button while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse.</summary>
            WM_XBUTTONUP = &H20C
        End Enum
        <Flags()>
        Public Enum SetWindowPosFlags As UInteger
            SWP_ASYNCWINDOWPOS = &H4000
            SWP_DEFERERASE = &H2000
            ''' <summary>Draws a frame (defined in the window's class description) around the window.</summary>
            SWP_DRAWFRAME = SWP_FRAMECHANGED
            ''' <summary>Causes the operating system to recalculate the size and position of the windows client area, even if the window size is not being changed. If this flag is not specified, the client area is recalculated only when the size or position of the window changes.</summary>
            SWP_FRAMECHANGED = &H20
            ''' <summary>Hides the window.</summary>
            SWP_HIDEWINDOW = &H80
            ''' <summary>Prevents the window from being activated. When this flag is not set, the window is activated and positioned according to the value of hWndInstertAfter and the setting of the SWP_NOZORDER flag. Otherwise, the window moves to the top of its current topmost status group.</summary>
            SWP_NOACTIVATE = &H10
            ''' <summary>This value is not supported.</summary>
            SWP_NOCOPYBITS = &H100
            ''' <summary>Retains the current position (ignores the X and Y parameters).</summary>
            SWP_NOMOVE = &H2
            ''' <summary>Does not change the owner window's position in the z-order.</summary>
            SWP_NOOWNERZORDER = &H200
            SWP_NOREDRAW = &H8
            ''' <summary>Same as SWP_NOOWNERZORDER.</summary>
            SWP_NOREPOSITION = SWP_NOOWNERZORDER
            ''' <summary></summary>
            SWP_NOSENDCHANGING = &H400
            ''' <summary>Retains the current size (ignores cx and cy).</summary>
            SWP_NOSIZE = &H1
            ''' <summary>The z-order can change with SWP_NOZORDER set if a top-level window which is not on top has SWP_NOACTIVATE unset. This results in moving the window to the top of its current topmost status group.</summary>
            SWP_NOZORDER = &H4
            ''' <summary>Displays the window</summary>
            SWP_SHOWWINDOW = &H40
        End Enum
        'Note: Cats these values to IntPtr
        Public Enum SpecialWindowHandles
            ''' <summary>Places the window at the top of the z-order.</summary>
            HWND_TOP = 0
            ''' <summary>Places the window at the bottom of the z-order. If the hWnd parameter identifies a topmost window, the window loses its topmost status and is placed at the bottom of all other windows.</summary>
            HWND_BOTTOM = 1
            ''' <summary>Places the window above all non-topmost windows. The window maintains its topmost position even when it is deactivated.</summary>
            HWND_TOPMOST = -1
            ''' <summary>Places the window above all non-topmost windows and below all topmost windows. This flag has no effect when SWP_NOACTIVATE is set or if the window is already above all non-topmost windows.</summary>
            HWND_NOTOPMOST = -2
        End Enum
        Public Enum WindowShowStyle As UInteger
            ''' <summary>Hides the window and activates another window.</summary>
            ''' <remarks>See SW_HIDE</remarks>
            Hide = 0
            '''<summary>Activates and displays a window. If the window is minimized 
            ''' or maximized, the system restores it to its original size and 
            ''' position. An application should specify this flag when displaying 
            ''' the window for the first time.</summary>
            ''' <remarks>See SW_SHOWNORMAL</remarks>
            ShowNormal = 1
            ''' <summary>Activates the window and displays it as a minimized window.</summary>
            ''' <remarks>See SW_SHOWMINIMIZED</remarks>
            ShowMinimized = 2
            ''' <summary>Activates the window and displays it as a maximized window.</summary>
            ''' <remarks>See SW_SHOWMAXIMIZED</remarks>
            ShowMaximized = 3
            ''' <summary>Maximizes the specified window.</summary>
            ''' <remarks>See SW_MAXIMIZE</remarks>
            Maximize = 3
            ''' <summary>Displays a window in its most recent size and position. 
            ''' This value is similar to "ShowNormal", except the window is not 
            ''' actived.</summary>
            ''' <remarks>See SW_SHOWNOACTIVATE</remarks>
            ShowNormalNoActivate = 4
            ''' <summary>Activates the window and displays it in its current size 
            ''' and position.</summary>
            ''' <remarks>See SW_SHOW</remarks>
            Show = 5
            ''' <summary>Minimizes the specified window and activates the next 
            ''' top-level window in the Z order.</summary>
            ''' <remarks>See SW_MINIMIZE</remarks>
            Minimize = 6
            '''   <summary>Displays the window as a minimized window. This value is 
            '''   similar to "ShowMinimized", except the window is not activated.</summary>
            ''' <remarks>See SW_SHOWMINNOACTIVE</remarks>
            ShowMinNoActivate = 7
            ''' <summary>Displays the window in its current size and position. This 
            ''' value is similar to "Show", except the window is not activated.</summary>
            ''' <remarks>See SW_SHOWNA</remarks>
            ShowNoActivate = 8
            ''' <summary>Activates and displays the window. If the window is 
            ''' minimized or maximized, the system restores it to its original size 
            ''' and position. An application should specify this flag when restoring 
            ''' a minimized window.</summary>
            ''' <remarks>See SW_RESTORE</remarks>
            Restore = 9
            ''' <summary>Sets the show state based on the SW_ value specified in the 
            ''' STARTUPINFO structure passed to the CreateProcess function by the 
            ''' program that started the application.</summary>
            ''' <remarks>See SW_SHOWDEFAULT</remarks>
            ShowDefault = 10
            ''' <summary>Windows 2000/XP: Minimizes a window, even if the thread 
            ''' that owns the window is hung. This flag should only be used when 
            ''' minimizing windows from a different thread.</summary>
            ''' <remarks>See SW_FORCEMINIMIZE</remarks>
            ForceMinimized = 11
        End Enum










#End Region

        Public Sub PostKeyDown(ByVal hWnd As IntPtr, ByVal Key As Keys)
            Dim wParam = Int(Key)
            Dim lParam = 1 << 0 Or _
            0 << 16 Or _
            0 << 24 Or _
            0 << 25 Or _
            0 << 29 Or _
            0 << 30 Or _
            0 << 31
            PostMessage(hWnd, &H100, wParam, lParam)
        End Sub
        Public Sub PostKeyUp(ByVal hWnd As IntPtr, ByVal Key As Keys)
            Dim wParam = Int(Key)
            Dim lParam = 1 << 0 Or _
            0 << 16 Or _
            0 << 24 Or _
            0 << 25 Or _
            0 << 29 Or _
            0 << 30 Or _
            0 << 31
            PostMessage(hWnd, &H101, wParam, lParam)
        End Sub
    End Structure
    Public Structure PEPatch
        Dim eowwefil23odflkelwkvd As Byte
        Public Sub Patch(ByVal TargetFile As String, ByVal FileOffset As Long, ByVal NewValue As Byte()) 'Declare Patch Sub-Program to patch external program
            Using br As BinaryReader = New BinaryReader(File.Open(TargetFile, FileMode.Open))
                br.BaseStream.Position = FileOffset
                For Value As Byte = LBound(NewValue) To UBound(NewValue)
                    br.BaseStream.WriteByte(NewValue(Value))
                Next
                br.Close()
            End Using
        End Sub
        'Public Variables - Vital
        Public eFilePath As String
        'Prvate Variables - NotVital
        Private eBytes As Array

        'Use Functions
        Public Function FileExistance() As Boolean
            If IO.File.Exists(eFilePath) = True Then
                Return (True)
            Else
                Return (False)
            End If
        End Function
        Public Function CompareFileSize(ByRef TotalBytes As Integer) As Boolean
            eBytes = IO.File.ReadAllBytes(eFilePath)
            If eBytes.Length = TotalBytes Then
                Return (True)
            Else
                Return (False)
            End If
        End Function

        Public Function GetFileSize() As Long
            eBytes = IO.File.ReadAllBytes(eFilePath)
            Return (eBytes.Length)
        End Function
        Public Function ReadBytes(ByRef pOffset As Long, Optional ByRef pLength As Integer = 4) As String
            Dim BR As IO.BinaryReader = New IO.BinaryReader(New IO.FileStream(eFilePath, IO.FileMode.OpenOrCreate))
            BR.BaseStream.Position = pOffset
            eBytes = BR.ReadBytes(pLength)
            ReadBytes = BitConverter.ToString(eBytes, 0, pLength)
            BR.Close()
        End Function
        Public Sub WriteBytes(ByRef pOffset As Long, ByRef pBytes As Byte)
            Dim BW As IO.BinaryWriter = New IO.BinaryWriter(New IO.FileStream(eFilePath, IO.FileMode.OpenOrCreate))
            BW.BaseStream.Position = pOffset
            BW.Write(pBytes)
            BW.Close()
        End Sub
        Public Function CheckPatched(ByRef pOffset As Long, ByRef pOriginalByte As Byte) As Boolean
            Dim BR As IO.BinaryReader = New IO.BinaryReader(New IO.FileStream(eFilePath, IO.FileMode.OpenOrCreate))
            BR.BaseStream.Position = pOffset
            eBytes = BR.ReadBytes(1)
            BR.Close()
            If BitConverter.ToString(eBytes, 0, 1) = BitConverter.ToString(BitConverter.GetBytes(pOriginalByte), 0, 1) Then
                Return (False) 'Not Patched
            Else
                Return (True) 'Patched
            End If
        End Function
    End Structure
End Class
