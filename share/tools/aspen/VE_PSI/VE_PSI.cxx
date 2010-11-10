#include "stdafx.h"
#include "VE_PSI.h"
#include "VE_PSIDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

BEGIN_MESSAGE_MAP(VE_PSIApp, CWinApp)
    ON_COMMAND(ID_HELP, &CWinApp::OnHelp)
END_MESSAGE_MAP()

///////////////////////////////////////////////////////////////////////////////
VE_PSIApp::VE_PSIApp()
{
}

VE_PSIApp theApp;

///////////////////////////////////////////////////////////////////////////////
BOOL VE_PSIApp::InitInstance()
{
    CWinApp::InitInstance();
    HRESULT hr = CoInitializeEx(NULL, COINIT_MULTITHREADED);
    AfxEnableControlContainer();
    SetRegistryKey(_T("VE-PSI"));

    VE_PSIDlg dlg;
    m_pMainWnd = &dlg;
    INT_PTR nResponse = dlg.DoModal();
    if (nResponse == IDOK)
    {
    }
    else if (nResponse == IDCANCEL)
    {
    }

    return FALSE;
}
