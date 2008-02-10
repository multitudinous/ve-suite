// HyperLabUnit.h : main header file for the PROJECT_NAME application

#pragma once

#ifndef __AFXWIN_H__
	#error "include 'stdafx.h' before including this file for PCH"
#endif

//Main symbols
#include "resource.h"

class CHyperLabUnitApp : public CWinApp
{
public:
    CHyperLabUnitApp();

    //Overrides
    virtual BOOL InitInstance();

    virtual int ExitInstance();

    //Implementation
    DECLARE_MESSAGE_MAP()
};

extern CHyperLabUnitApp theApp;
