#pragma once

#ifndef __AFXWIN_H__
    #error "include 'stdafx.h' before including this file for PCH"
#endif

#include "Resource.h"

class VE_PSIApp : public CWinApp
{
public:
    VE_PSIApp();
    virtual BOOL InitInstance();

    DECLARE_MESSAGE_MAP()
};

extern VE_PSIApp theApp;