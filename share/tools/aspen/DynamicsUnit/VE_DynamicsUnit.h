// VE_DynamicsUnit.h : main header file for the PROJECT_NAME application
//

#pragma once

#ifndef __AFXWIN_H__
	#error "include 'stdafx.h' before including this file for PCH"
#endif

#include "Resource.h"		// main symbols


// CVE_DynamicsUnitApp:
// See VE_DynamicsUnit.cpp for the implementation of this class
//

class CVE_DynamicsUnitApp : public CWinApp
{
public:
	CVE_DynamicsUnitApp();

// Overrides
	public:
	virtual BOOL InitInstance();

// Implementation

	DECLARE_MESSAGE_MAP()
};

extern CVE_DynamicsUnitApp theApp;