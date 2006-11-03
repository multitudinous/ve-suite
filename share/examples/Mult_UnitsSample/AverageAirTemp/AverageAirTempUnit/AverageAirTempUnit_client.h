// AverageAirTempUnit.h : main header file for the PROJECT_NAME application
//

#pragma once

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif
#include "stdafx.h"

#include "afxwin.h"
#include <iostream>
#include "resource.h"		// main symbols

// CAverageAirTempApp:
// See AverageAirTempUnit_client.cpp for the implementation of this class
//

class CAverageAirTempApp : public CWinApp
{
public:
	CAverageAirTempApp();

// Overrides
	public:
	virtual BOOL InitInstance();

// Implementation

	DECLARE_MESSAGE_MAP()
};

extern CAverageAirTempApp theApp;
