
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

    //Run the message loop
    while( GetMessage( &msg, NULL, 0, 0 ) > 0 )
    {
        TranslateMessage( &msg );
        DispatchMessage( &msg );
    }

    return 1;
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
    // Microsoft Layer for Unicode (MSLU) is used
    DefWindowProc( NULL, 0, 0, 0L );

    hRes = _Module.Init( NULL, hInstance );
    ATLASSERT( SUCCEEDED( hRes ) );

    int nRet = Run( lpstrCmdLine, nCmdShow );

    _Module.Term();
    CoUninitialize();

    return nRet;
}
////////////////////////////////////////////////////////////////////////////////
