// AdiabaticFlameTempUnit.h : main header file for the PROJECT_NAME application
//

#pragma once

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif
#include "stdafx.h"

#include "afxwin.h"
#include <iostream>
#include "resource.h"		// main symbols

// CAdiabaticFlameTempApp:
// See AdiabaticFlameTempUnit_client.cpp for the implementation of this class
//

class CAdiabaticFlameTempApp : public CWinApp
{
public:
	CAdiabaticFlameTempApp();

// Overrides
	public:
	virtual BOOL InitInstance();

// Implementation

	DECLARE_MESSAGE_MAP()
};

extern CAdiabaticFlameTempApp theApp;
