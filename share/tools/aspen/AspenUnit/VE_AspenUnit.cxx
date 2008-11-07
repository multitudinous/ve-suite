// VE_AspenUnit.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"
#include "VE_AspenUnit.h"
#include "VE_AspenUnitDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif


// CVE_AspenUnitApp

BEGIN_MESSAGE_MAP(CVE_AspenUnitApp, CWinApp)
	ON_COMMAND(ID_HELP, &CWinApp::OnHelp)
END_MESSAGE_MAP()


// CVE_AspenUnitApp construction

CVE_AspenUnitApp::CVE_AspenUnitApp()
{
	// TODO: add construction code here,
	// Place all significant initialization in InitInstance
}


// The one and only CVE_AspenUnitApp object

CVE_AspenUnitApp theApp;


// CVE_AspenUnitApp initialization

BOOL CVE_AspenUnitApp::InitInstance()
{
	CWinApp::InitInstance();

	// Standard initialization
	// If you are not using these features and wish to reduce the size
	// of your final executable, you should remove from the following
	// the specific initialization routines you do not need
	// Change the registry key under which our settings are stored
	// TODO: You should modify this string to be something appropriate
	// such as the name of your company or organization

	if(!AfxOleInit())  // Your addition starts here.
    {
        AfxMessageBox(_T("Cannot initialize COM dll"));
        return FALSE;
        // End of your addition.
    }
    AfxEnableControlContainer();

	SetRegistryKey(_T("Aspen Unit"));

	CVE_AspenUnitDlg dlg;
	m_pMainWnd = &dlg;
	INT_PTR nResponse = dlg.DoModal();
	if (nResponse == IDOK)
	{
		// TODO: Place code here to handle when the dialog is
		//  dismissed with OK
	}
	else if (nResponse == IDCANCEL)
	{
		// TODO: Place code here to handle when the dialog is
		//  dismissed with Cancel
	}

	// Since the dialog has been closed, return FALSE so that we exit the
	//  application, rather than start the application's message pump.
	return FALSE;
}
