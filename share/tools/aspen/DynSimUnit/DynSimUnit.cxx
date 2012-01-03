/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
// DynSimUnit.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"
#include "DynSimUnit.h"
#include "DynSimUnitDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif


// CDynSimUnitApp

BEGIN_MESSAGE_MAP(CDynSimUnitApp, CWinApp)
    ON_COMMAND(ID_HELP, &CWinApp::OnHelp)
END_MESSAGE_MAP()


// CDynSimUnitApp construction

CDynSimUnitApp::CDynSimUnitApp()
{
    // TODO: add construction code here,
    // Place all significant initialization in InitInstance
}


// The one and only CDynSimUnitApp object

CDynSimUnitApp theApp;


// CDynSimUnitApp initialization

BOOL CDynSimUnitApp::InitInstance()
{
    CWinApp::InitInstance();

    // Standard initialization
    // If you are not using these features and wish to reduce the size
    // of your final executable, you should remove from the following
    // the specific initialization routines you do not need
    // Change the registry key under which our settings are stored
    // TODO: You should modify this string to be something appropriate
    // such as the name of your company or organization

    //if(!AfxOleInit())  // Your addition starts here.
    //{
    //    AfxMessageBox(_T("Cannot initialize COM dll"));
    //    return FALSE;
        // End of your addition.
    //}
    
    HRESULT hr = CoInitializeEx(NULL, COINIT_MULTITHREADED);

    AfxEnableControlContainer();

    SetRegistryKey(_T("VE-DynSim"));

    CDynSimUnitDlg dlg;
    m_pMainWnd = &dlg;
    INT_PTR nResponse = dlg.DoModal();
    if (nResponse == IDOK)
    {
        // TODO: Place code here to handle when the dialog is
        //  dismissed with OK
    }
    else if (nResponse == IDCANCEL)
    {
        // TODO: Place code here to handle when the dialog is
        //  dismissed with Cancel
    }

    // Since the dialog has been closed, return FALSE so that we exit the
    //  application, rather than start the application's message pump.
    return FALSE;
}
