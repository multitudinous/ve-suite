// DlgProxy.cpp : implementation file
//

#include "stdafx.h"
#include "Tester.h"
#include "DlgProxy.h"
#include "TesterDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif


// CTesterDlgAutoProxy

IMPLEMENT_DYNCREATE(CTesterDlgAutoProxy, CCmdTarget)

CTesterDlgAutoProxy::CTesterDlgAutoProxy()
{
	EnableAutomation();
	
	// To keep the application running as long as an automation 
	//	object is active, the constructor calls AfxOleLockApp.
	AfxOleLockApp();

	// Get access to the dialog through the application's
	//  main window pointer.  Set the proxy's internal pointer
	//  to point to the dialog, and set the dialog's back pointer to
	//  this proxy.
	ASSERT_VALID(AfxGetApp()->m_pMainWnd);
	if (AfxGetApp()->m_pMainWnd)
	{
		ASSERT_KINDOF(CTesterDlg, AfxGetApp()->m_pMainWnd);
		if (AfxGetApp()->m_pMainWnd->IsKindOf(RUNTIME_CLASS(CTesterDlg)))
		{
			m_pDialog = reinterpret_cast<CTesterDlg*>(AfxGetApp()->m_pMainWnd);
			m_pDialog->m_pAutoProxy = this;
		}
	}
}

CTesterDlgAutoProxy::~CTesterDlgAutoProxy()
{
	// To terminate the application when all objects created with
	// 	with automation, the destructor calls AfxOleUnlockApp.
	//  Among other things, this will destroy the main dialog
	if (m_pDialog != NULL)
		m_pDialog->m_pAutoProxy = NULL;
	AfxOleUnlockApp();
}

void CTesterDlgAutoProxy::OnFinalRelease()
{
	// When the last reference for an automation object is released
	// OnFinalRelease is called.  The base class will automatically
	// deletes the object.  Add additional cleanup required for your
	// object before calling the base class.

	CCmdTarget::OnFinalRelease();
}

BEGIN_MESSAGE_MAP(CTesterDlgAutoProxy, CCmdTarget)
END_MESSAGE_MAP()

BEGIN_DISPATCH_MAP(CTesterDlgAutoProxy, CCmdTarget)
END_DISPATCH_MAP()

// Note: we add support for IID_ITester to support typesafe binding
//  from VBA.  This IID must match the GUID that is attached to the 
//  dispinterface in the .IDL file.

// {CC1C21B3-16AB-463F-97FF-5AC47B0589EE}
static const IID IID_ITester =
{ 0xCC1C21B3, 0x16AB, 0x463F, { 0x97, 0xFF, 0x5A, 0xC4, 0x7B, 0x5, 0x89, 0xEE } };

BEGIN_INTERFACE_MAP(CTesterDlgAutoProxy, CCmdTarget)
	INTERFACE_PART(CTesterDlgAutoProxy, IID_ITester, Dispatch)
END_INTERFACE_MAP()

// The IMPLEMENT_OLECREATE2 macro is defined in StdAfx.h of this project
// {44686D56-282A-4FB8-AB9C-AC1D6FBE39DB}
IMPLEMENT_OLECREATE2(CTesterDlgAutoProxy, "Tester.Application", 0x44686d56, 0x282a, 0x4fb8, 0xab, 0x9c, 0xac, 0x1d, 0x6f, 0xbe, 0x39, 0xdb)


// CTesterDlgAutoProxy message handlers
