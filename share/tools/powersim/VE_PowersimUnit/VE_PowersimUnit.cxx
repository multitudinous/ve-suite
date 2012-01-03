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

// --- VE_PowersimUnit Includes --- //
#include "StdAtl.h"
#include "Resource.h"
#include "MainDlg.h"

CComModule _Module;

////////////////////////////////////////////////////////////////////////////////
int Run( LPTSTR = NULL, int nCmdShow = SW_SHOWDEFAULT )
{
    CMainDlg dlgMain;
    MSG msg;

    if( dlgMain.Create( NULL ) == NULL )
    {
        ATLTRACE( _T( "Main dialog creation failed!\n" ) );

        return 0;
    }

    dlgMain.ShowWindow( nCmdShow );

    for( ;; )
    {
        while( !PeekMessage( &msg, NULL, 0, 0, PM_NOREMOVE ) )
        {
            dlgMain.OnIdle();
        }

        BOOL bRet = GetMessage( &msg, NULL, 0, 0 );
        if( bRet == -1 )
        {
            //Error, don't process
            continue;
        }
        else if( !bRet )
        {
            //WM_QUIT, exit message loop
            break;
        }

        if( !dlgMain.PreTranslateMessage( &msg ) )
        {
            TranslateMessage( &msg );
            DispatchMessage( &msg );
        }
    }

    return static_cast< int >( msg.wParam );
}
////////////////////////////////////////////////////////////////////////////////
int WINAPI _tWinMain(
    HINSTANCE hInstance, HINSTANCE, LPTSTR lpstrCmdLine, int nCmdShow )
{
    HRESULT hRes = CoInitialize( NULL );
    //If you are running on NT 4.0 or higher you can use the
    //following call instead to make the EXE free threaded.
    //This means that calls come in on a random RPC thread.
    //HRESULT hRes = CoInitializeEx( NULL, COINIT_MULTITHREADED );
    ATLASSERT( SUCCEEDED( hRes ) );

    //This resolves ATL window thunking problem when
    //Microsoft Layer for Unicode (MSLU) is used
    DefWindowProc( NULL, 0, 0, 0L );

    hRes = _Module.Init( NULL, hInstance );
    ATLASSERT( SUCCEEDED( hRes ) );

    int nRet = Run( lpstrCmdLine, nCmdShow );

    _Module.Term();
    CoUninitialize();

    return nRet;
}
////////////////////////////////////////////////////////////////////////////////
