// DynamicsUnit.h : main header file for the DYNAMICSUNIT application
//

#if !defined(AFX_DYNAMICSUNIT_H__28020577_9503_4BFE_97CF_831516B1F89A__INCLUDED_)
#define AFX_DYNAMICSUNIT_H__28020577_9503_4BFE_97CF_831516B1F89A__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"		// main symbols
#include "aspendynamics.h"

/////////////////////////////////////////////////////////////////////////////
// CDynamicsUnitApp:
// See DynamicsUnit.cpp for the implementation of this class
//

class CDynamicsUnitApp : public CWinApp
{
public:
	CDynamicsUnitApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDynamicsUnitApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CDynamicsUnitApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_DYNAMICSUNIT_H__28020577_9503_4BFE_97CF_831516B1F89A__INCLUDED_)
