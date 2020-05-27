// dllmain.cpp : Defines the entry point for the DLL application.
#include "stdafx.h"
#include <iostream>
#include <Tlhelp32.h>
#include <cstdlib>
#include <direct.h>

DWORD GetPIDForProcess (char* process);
void EnableDebugPriv();
DWORD GetDLLBase(char* DllName, DWORD tPid);

void Hack_Ads_Tick()
{
	while(true)
	{
			HWND hwnd = NULL;

			// main from login
			hwnd = FindWindow("TTCGNetLoginFrm",NULL);
			hwnd = FindWindowEx(hwnd,NULL,"Shell Embedding",NULL);
			hwnd = FindWindowEx(hwnd,NULL,"Shell DocObject View",NULL);
			SendMessage(hwnd,WM_CLOSE,0,0);

			// Top Form
			hwnd = FindWindow("TTCGNetMainFrm",NULL);
			hwnd = FindWindowEx(hwnd,NULL,"TsPanel","BannerPnl");
			hwnd = FindWindowEx(hwnd,NULL,"Shell Embedding",NULL);
			hwnd = FindWindowEx(hwnd,NULL,"Shell DocObject View",NULL);
			SendMessage(hwnd,WM_CLOSE,0,0);

			// Login form
			hwnd = FindWindow("TTCGNetMainFrm",NULL);
			hwnd = FindWindowEx(hwnd,NULL,"TsPanel",NULL);
			hwnd = FindWindowEx(hwnd,NULL,"TsPageControl",NULL);
			hwnd = FindWindowEx(hwnd,NULL,"TsTabSheet","Main");
			hwnd = FindWindowEx(hwnd,NULL,"TsPanel","sPanel3");
			hwnd = FindWindowEx(hwnd,NULL,"Shell Embedding",NULL);
			hwnd = FindWindowEx(hwnd,NULL,"Shell DocObject View",NULL);
			SendMessage(hwnd,WM_CLOSE,0,0);

			// Top form of Channel Form
			hwnd = FindWindow("TTCGNetChannelFrm",NULL);
			hwnd = FindWindowEx(hwnd,NULL,"Shell Embedding",NULL);
			hwnd = FindWindowEx(hwnd,NULL,"Shell DocObject View",NULL);
			SendMessage(hwnd,WM_CLOSE,0,0);

			// Chat TCG form
			hwnd = FindWindow("TTCGNetMainFrm",NULL);
			hwnd = FindWindowEx(hwnd,NULL,"TsPanel",NULL);
			hwnd = FindWindowEx(hwnd,NULL,"TsPageControl",NULL);
			hwnd = FindWindowEx(hwnd,NULL,"TsTabSheet","Lobby");
			hwnd = FindWindowEx(hwnd,NULL,"TsPanel",NULL);
			hwnd = GetWindow(hwnd,GW_HWNDNEXT);
			hwnd = FindWindowEx(hwnd,NULL,"TPanel",NULL);
			hwnd = FindWindowEx(hwnd,NULL,"TsPanel","sPanel2");
			hwnd = FindWindowEx(hwnd,NULL,"Shell Embedding",NULL);
			hwnd = FindWindowEx(hwnd,NULL,"Shell DocObject View",NULL);
			SendMessage(hwnd,WM_CLOSE,0,0);

			// top of start game form
			hwnd = FindWindow("TCGNetFrmStartGame",NULL);
			hwnd = FindWindowEx(hwnd,NULL,"Shell Embedding",NULL);
			hwnd = FindWindowEx(hwnd,NULL,"Shell DocObject View",NULL);
			SendMessage(hwnd,WM_CLOSE,0,0);

			Sleep(1000);
	}
}

void Memory_Hack()
{
	DWORD PID = 0;
	if (GetPIDForProcess("Cybergames.exe") != NULL)
    	PID = GetPIDForProcess("Cybergames.exe");
	HANDLE hProc = OpenProcess(PROCESS_ALL_ACCESS, false, PID);
	if (hProc)
		{
			 // GetWindowThreadProcessId
			 // GetCurrentProcess()
			 DWORD BaseAddress = GetDLLBase("Cybergames.exe", PID);
			 DWORD dSize = 0;

			 // patchwar26a(hProc, BaseAddress, dSize);
			 // 4EE2D1 No exit without confirm OLD:75 20 NEW:EB 20
			 WriteProcessMemory(hProc,(LPVOID)(BaseAddress + 0xEE2D1),"\xEB\x20",2,0);
			 // 4EE2B0 "\x90\x90\x90\x90\x90 #no Open web after exit #1

			 // 727B99, 727BD1 , 727C09 , 727C2E
			 WriteProcessMemory(hProc,(LPVOID)(0x727B99),"\x63\x6C\x6F\x73\x65",5,0);

			 // 004 Remove MessageBox VIP ONLY CAN JOIN //90 90 90 90 90
			 WriteProcessMemory(hProc,(LPVOID)(BaseAddress + 0xE6979),"\x90\x90\x90\x90\x90",5,0);

			 // 4E6990 No Open web if join full //90 90 90 90 90
			 WriteProcessMemory(hProc,(LPVOID)(BaseAddress + 0xE6990),"\x90\x90\x90\x90\x90",5,0);
			 
			 // Address : 004E92ED - 7A 3A
			 WriteProcessMemory(hProc,(LPVOID)(BaseAddress + 0xE92ED),"\x7A\3A",2,0);
			 
			 CloseHandle(hProc);
		}
}

BOOL APIENTRY DllMain( HMODULE hModule,DWORD  ul_reason_for_call, LPVOID lpReserved)
{
	switch (ul_reason_for_call)
	{
	case DLL_PROCESS_ATTACH:
		{
			DisableThreadLibraryCalls(hModule);
			EnableDebugPriv(); 
			while(1){

				if(IsWindowVisible(FindWindow("TTCGNetLoginFrm",NULL)))
				{
					CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)&Hack_Ads_Tick, NULL, 0, NULL);
					Memory_Hack();
					break;
				}
			Sleep(1000);
			}
	     }
	case DLL_THREAD_ATTACH:
	case DLL_THREAD_DETACH:
	case DLL_PROCESS_DETACH:
		//TerminateProcess(GetCurrentProcess(),0);
		break;
	}
	return TRUE;
}

void EnableDebugPriv()
{
    HANDLE hToken;
    LUID sedebugnameValue;
    TOKEN_PRIVILEGES tkp;

    if (!OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &hToken))
        return;

    if (!LookupPrivilegeValue(NULL, SE_DEBUG_NAME,&sedebugnameValue))
	{
        CloseHandle(hToken);
        return;
    }

    tkp.PrivilegeCount = 1;
    tkp.Privileges[0].Luid = sedebugnameValue;
    tkp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;
    if (!AdjustTokenPrivileges(hToken, FALSE, &tkp, sizeof tkp, NULL, NULL))
        CloseHandle(hToken);
} 

DWORD GetPIDForProcess(char* process)
{
    BOOL            working=0;
    PROCESSENTRY32 lppe= {0};
DWORD            targetPid=0;
    HANDLE hSnapshot=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS ,0);
    if (hSnapshot)
    {
        lppe.dwSize=sizeof(lppe);
        working=Process32First(hSnapshot,&lppe);
        while (working)
        {
   if(strcmp(lppe.szExeFile,process)==0)
            {
                targetPid=lppe.th32ProcessID;
                break;
            }
            working=Process32Next(hSnapshot,&lppe);
        }
    }
    CloseHandle( hSnapshot );
    return targetPid;
}

DWORD GetDLLBase(char* DllName, DWORD tPid)
{
    HANDLE snapMod;
    MODULEENTRY32 me32;
    if (tPid == 0) return 0;
    snapMod = CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, tPid);
    me32.dwSize = sizeof(MODULEENTRY32);
    if (Module32First(snapMod, &me32)){
        do{
            if (strcmp(DllName,me32.szModule) == 0){
                CloseHandle(snapMod);
    return (DWORD) me32.modBaseAddr;
            }
        }while(Module32Next(snapMod,&me32));
    }
    CloseHandle(snapMod);
    return 0;
}
