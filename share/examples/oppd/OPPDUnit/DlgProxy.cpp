// DlgProxy.cpp : implementation file
//

#include "stdafx.h"
#include "OPPDUnit.h"
#include "DlgProxy.h"
#include "OPPDUnitDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif


// COPPDUnitDlgAutoProxy

IMPLEMENT_DYNCREATE(COPPDUnitDlgAutoProxy, CCmdTarget)

COPPDUnitDlgAutoProxy::COPPDUnitDlgAutoProxy()
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
		ASSERT_KINDOF(COPPDUnitDlg, AfxGetApp()->m_pMainWnd);
		if (AfxGetApp()->m_pMainWnd->IsKindOf(RUNTIME_CLASS(COPPDUnitDlg)))
		{
			m_pDialog = reinterpret_cast<COPPDUnitDlg*>(AfxGetApp()->m_pMainWnd);
			m_pDialog->m_pAutoProxy = this;
		}
	}
}

COPPDUnitDlgAutoProxy::~COPPDUnitDlgAutoProxy()
{
	// To terminate the application when all objects created with
	// 	with automation, the destructor calls AfxOleUnlockApp.
	//  Among other things, this will destroy the main dialog
	if (m_pDialog != NULL)
		m_pDialog->m_pAutoProxy = NULL;
	AfxOleUnlockApp();
}

void COPPDUnitDlgAutoProxy::OnFinalRelease()
{
	// When the last reference for an automation object is released
	// OnFinalRelease is called.  The base class will automatically
	// deletes the object.  Add additional cleanup required for your
	// object before calling the base class.

	CCmdTarget::OnFinalRelease();
}

BEGIN_MESSAGE_MAP(COPPDUnitDlgAutoProxy, CCmdTarget)
END_MESSAGE_MAP()

BEGIN_DISPATCH_MAP(COPPDUnitDlgAutoProxy, CCmdTarget)
END_DISPATCH_MAP()

// Note: we add support for IID_IOPPDUnit to support typesafe binding
//  from VBA.  This IID must match the GUID that is attached to the 
//  dispinterface in the .IDL file.

// {395E090F-393C-4AF9-91CA-8EC8EAF660C0}
static const IID IID_IOPPDUnit =
{ 0x395E090F, 0x393C, 0x4AF9, { 0x91, 0xCA, 0x8E, 0xC8, 0xEA, 0xF6, 0x60, 0xC0 } };

BEGIN_INTERFACE_MAP(COPPDUnitDlgAutoProxy, CCmdTarget)
	INTERFACE_PART(COPPDUnitDlgAutoProxy, IID_IOPPDUnit, Dispatch)
END_INTERFACE_MAP()

// The IMPLEMENT_OLECREATE2 macro is defined in StdAfx.h of this project
// {458DEF56-B4F7-4F57-838E-7E40C7D9C6BE}
IMPLEMENT_OLECREATE2(COPPDUnitDlgAutoProxy, "OPPDUnit.Application", 0x458def56, 0xb4f7, 0x4f57, 0x83, 0x8e, 0x7e, 0x40, 0xc7, 0xd9, 0xc6, 0xbe)


// COPPDUnitDlgAutoProxy message handlers
