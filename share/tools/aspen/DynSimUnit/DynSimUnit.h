// DynSimUnit.h : main header file for the PROJECT_NAME application
//

#pragma once

#ifndef __AFXWIN_H__
	#error "include 'stdafx.h' before including this file for PCH"
#endif

#include "Resource.h"		// main symbols


// CDynSimUnitApp:
// See DynSimUnit.cpp for the implementation of this class
//

class CDynSimUnitApp : public CWinApp
{
public:
	CDynSimUnitApp();

// Overrides
	public:
	virtual BOOL InitInstance();

// Implementation

	DECLARE_MESSAGE_MAP()
};

extern CDynSimUnitApp theApp;