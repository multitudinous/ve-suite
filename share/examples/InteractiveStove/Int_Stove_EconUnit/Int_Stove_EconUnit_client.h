// Int_Stove_EconUnit_client.h : main header file for the PROJECT_NAME application
//
#pragma once

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"		// main symbols

// CInt_Stove_EconUnitApp:
// See Int_Stove_EconUnit_client.cpp for the implementation of this class
//

class CInt_Stove_EconUnitApp : public CWinApp
{
public:
	CInt_Stove_EconUnitApp();

// Overrides
	public:
	virtual BOOL InitInstance();

// Implementation

	DECLARE_MESSAGE_MAP()
};

extern CInt_Stove_EconUnitApp theApp;
