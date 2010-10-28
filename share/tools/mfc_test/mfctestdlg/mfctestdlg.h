// mfctestdlg.h : main header file for the mfctestdlg application
//
#pragma once

#ifndef __AFXWIN_H__
	#error "include 'stdafx.h' before including this file for PCH"
#endif

#include "resource.h"       // main symbols
#include <string>


// CmfctestdlgApp:
// See mfctestdlg.cpp for the implementation of this class
//

class CmfctestdlgApp : public CWinApp
{
public:
	CmfctestdlgApp();
	std::string test;

// Overrides
public:
	virtual BOOL InitInstance();

// Implementation
	afx_msg void OnAppAbout();
	DECLARE_MESSAGE_MAP()
};

extern CmfctestdlgApp theApp;