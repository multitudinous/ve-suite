
// --- VE_PowersimUnit Includes --- //
#include "StdAtl.h"
#include "Resource.h"
#include "MainDlg.h"
#include "AboutDlg.h"
#include "VE_PowersimUnit_i.h"
#include "CorbaUnitManager.h"

//
#include "SIPParser.h"
//

// --- WIN32 Includes --- //
#include <shlobj.h> //For SHBrowseForFolder

////////////////////////////////////////////////////////////////////////////////
CMainDlg::CMainDlg()
    :
    m_initialized( false ),
    m_unitObject( NULL ),
    m_corbaUnitManager( NULL )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
BOOL CMainDlg::PreTranslateMessage( MSG* pMsg )
{
    return CWindow::IsDialogMessage( pMsg );
}
////////////////////////////////////////////////////////////////////////////////
BOOL CMainDlg::OnIdle()
{
    Sleep( 100 );

    if( m_initialized )
    {
        m_corbaUnitManager->CheckCORBAWork();
    }

    return FALSE;
}
////////////////////////////////////////////////////////////////////////////////
LRESULT CMainDlg::OnInitDialog( UINT, WPARAM, LPARAM, BOOL& )
{
    //Center the dialog on the screen
    CenterWindow();

    //Set icons
    HICON hIcon = (HICON)::LoadImage(
        _Module.GetResourceInstance(), MAKEINTRESOURCE( IDR_MAINFRAME ),
        IMAGE_ICON, GetSystemMetrics( SM_CXICON ),
        GetSystemMetrics( SM_CYICON ), LR_DEFAULTCOLOR );
    SetIcon( hIcon, TRUE );
    HICON hIconSmall = (HICON)::LoadImage(
        _Module.GetResourceInstance(), MAKEINTRESOURCE( IDR_MAINFRAME ),
        IMAGE_ICON, GetSystemMetrics( SM_CXSMICON ),
        GetSystemMetrics( SM_CYSMICON ), LR_DEFAULTCOLOR );
    SetIcon( hIconSmall, FALSE );

    //Add "About..." menu item to system menu
    //IDM_ABOUTBOX must be in the system command range
    _ASSERTE( ( IDM_ABOUTBOX & 0xFFF0 ) == IDM_ABOUTBOX );
    _ASSERTE( IDM_ABOUTBOX < 0xF000 );
    HMENU SysMenu = GetSystemMenu( FALSE );
    if( IsMenu( SysMenu ) )
    {
        TCHAR szAboutMenu[ 256 ];
        if( LoadString( _Module.GetResourceInstance(),
                        IDS_ABOUTBOX, szAboutMenu, 255 ) > 0 )
        {
            AppendMenu( SysMenu, MF_SEPARATOR, 0, "" );
            AppendMenu( SysMenu, MF_STRING, IDM_ABOUTBOX, szAboutMenu );
        }
    }

    SetDlgItemText( IDC_DIR, "C:\\" );
    SetDlgItemText( IDC_PCNAME, "localhost" );
    SetDlgItemText( IDC_PORT, "1239" );

    return TRUE;
}
////////////////////////////////////////////////////////////////////////////////
LRESULT CMainDlg::OnDestroy( UINT, WPARAM, LPARAM, BOOL& )
{
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
LRESULT CMainDlg::OnAppAbout( WORD, WORD, HWND, BOOL& )
{
    CAboutDlg dlg;
    dlg.DoModal();

    return 0;
}
////////////////////////////////////////////////////////////////////////////////
LRESULT CMainDlg::OnOK( WORD, WORD wID, HWND, BOOL& )
{
    if( !m_initialized )
    {
        ATL::CString workingDir;
        GetDlgItemText( IDC_DIR, workingDir );
        ATL::CString computerName;
        GetDlgItemText( IDC_PCNAME, computerName );
        ATL::CString computerPort;
        GetDlgItemText( IDC_PORT, computerPort );

        m_corbaUnitManager = new CorbaUnitManager( this );
        m_corbaUnitManager->SetComputerNameUnitNameAndPort(
            computerName, computerName, computerPort, "PowersimUnit" );
        m_corbaUnitManager->RunORB();
        m_unitObject = m_corbaUnitManager->GetUnitObject();

        //Test Code
        SIPParser* sipParser = m_corbaUnitManager->CreateParser();
        sipParser->SetWorkingDir( std::string( workingDir ) );
        sipParser->OpenSimulation( "InventoryTutorial" );
        //

        if( !m_unitObject )
        {
            MessageBox( _T( "Unable to connect to VE-CE" ) );
            m_corbaUnitManager->DestroyORB();
            delete m_corbaUnitManager;
            m_corbaUnitManager = NULL;
            m_initialized = false;

            return 0;
        }
        else
        {
            m_initialized = true;
            GetDlgItem( IDOK ).EnableWindow( FALSE );
        }
    }

    return 0;
}
////////////////////////////////////////////////////////////////////////////////
LRESULT CMainDlg::OnCancel( WORD, WORD wID, HWND, BOOL& )
{
    CloseDialog( wID );

    return 0;
}
////////////////////////////////////////////////////////////////////////////////
LRESULT CMainDlg::OnDirPicker( WORD, WORD wID, HWND, BOOL& )
{
    //TODO: Add your control notification handler code here
    BROWSEINFO bi = { 0 };
    bi.lpszTitle = _T( "Pick a Directory" );
    LPITEMIDLIST pidl = SHBrowseForFolder( &bi );
    if( pidl != 0 )
    {
        //Get the name of the folder
        TCHAR path[ MAX_PATH ];
        if ( SHGetPathFromIDList( pidl, path ) )
        {
            ATL::CString workingDir( path );
            SetDlgItemText( IDC_DIR, workingDir + "\\" );
        }

        //Free memory used
        IMalloc* imalloc = 0;
        if( SUCCEEDED( SHGetMalloc ( &imalloc ) ) )
        {
            imalloc->Free ( pidl );
            imalloc->Release ( );
        }
    }

    return 0;
}
////////////////////////////////////////////////////////////////////////////////
LRESULT CMainDlg::OnSysCommand( UINT, WPARAM wParam, LPARAM, BOOL& bHandled )
{
    UINT uCmdType = static_cast< UINT >( wParam );

    if( ( uCmdType & 0xFFF0 ) == IDM_ABOUTBOX )
    {
        CAboutDlg dlg;
        dlg.DoModal();
    }
    else
    {
        bHandled = FALSE;
    }

    return 0;
}
////////////////////////////////////////////////////////////////////////////////
void CMainDlg::CloseDialog( int nVal )
{
    if( m_corbaUnitManager != NULL )
    {
        m_corbaUnitManager->DestroyORB();
        delete m_corbaUnitManager;
        m_corbaUnitManager = NULL;
    }

    DestroyWindow();
    PostQuitMessage( nVal );
}
////////////////////////////////////////////////////////////////////////////////
