Imports System.Runtime.InteropServices
Imports Microsoft.Win32
Imports System.IO

Module Main
    Public Memory As GFunction.Memory
    Public Window As GFunction.Window
    Public Cybergames_Path As String = Registry.CurrentUser.OpenSubKey("Software\Cybergames", False).GetValue("INSTALL_PATH")
    Public dllHack_Path As String = (System.AppDomain.CurrentDomain.BaseDirectory + "\CybergamesH.dll")
    Sub Main()
        Try
            Console.Title = "Cybergames Loader 1.43"
            Console.WriteLine("Created by BlackSource")
            Console.WriteLine("Special Thanks")
            Console.WriteLine("-Thanks to Chainy_S for his skiper 5 seccond")
            Console.WriteLine("Homepage: http://Cracker.in.th")
            Threading.Thread.Sleep(1000)
            If Process.GetProcessesByName("Cybergames").Length = 0 Then
                Dim tcgProc As New Process
                tcgProc.StartInfo.WorkingDirectory = Cybergames_Path
                tcgProc.StartInfo.FileName = "Cybergames.exe"
                tcgProc.StartInfo.CreateNoWindow = True
                tcgProc.Start()
                While (True)
                    If Process.GetProcessesByName("Cybergames").Length = 1 Then
                        Threading.Thread.Sleep(3000)
                        Memory.GetProcessHandle("Cybergames")
                        Memory.InjectDll(dllHack_Path)
                        Exit While
                    End If
                    Threading.Thread.Sleep(1000)
                End While
            Else
                Memory.GetProcessHandle("Cybergames")
                Memory.InjectDll(dllHack_Path)
            End If
        Catch ex As Exception
            Console.WriteLine(ex.Message)
            Console.ReadLine()
        End Try
    End Sub

End Module
