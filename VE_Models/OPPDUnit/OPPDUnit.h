// OPPDUnit.h : main header file for the PROJECT_NAME application
//

#pragma once

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif
#include "stdafx.h"
//#include "OPPDUnit_i.h"

#include "afxwin.h"
#include <iostream>
#include "resource.h"		// main symbols

// COPPDUnitApp:
// See OPPDUnit.cpp for the implementation of this class
//

class COPPDUnitApp : public CWinApp
{
public:
	COPPDUnitApp();

// Overrides
	public:
	virtual BOOL InitInstance();

// Implementation

	DECLARE_MESSAGE_MAP()
};

extern COPPDUnitApp theApp;