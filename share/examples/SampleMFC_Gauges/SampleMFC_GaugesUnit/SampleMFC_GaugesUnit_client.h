// SampleMFC_GaugesUnit.h : main header file for the PROJECT_NAME application
//

#pragma once

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif
#include "stdafx.h"

#include "afxwin.h"
#include <iostream>
#include "resource.h"		// main symbols

// CSampleMFCApp:
// See SampleMFC_GaugesUnit_client.cpp for the implementation of this class
//

class CSampleMFCApp : public CWinApp
{
public:
	CSampleMFCApp();

// Overrides
	public:
	virtual BOOL InitInstance();

// Implementation

	DECLARE_MESSAGE_MAP()
};

extern CSampleMFCApp theApp;
