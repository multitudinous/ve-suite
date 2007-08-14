// VE_AspenUnit.h : main header file for the PROJECT_NAME application
//

#pragma once

#ifndef __AFXWIN_H__
	#error "include 'stdafx.h' before including this file for PCH"
#endif

#include "resource.h"		// main symbols


// CVE_AspenUnitApp:
// See VE_AspenUnit.cpp for the implementation of this class
//

class CVE_AspenUnitApp : public CWinApp
{
public:
	CVE_AspenUnitApp();

// Overrides
	public:
	virtual BOOL InitInstance();

// Implementation

	DECLARE_MESSAGE_MAP()
};

extern CVE_AspenUnitApp theApp;