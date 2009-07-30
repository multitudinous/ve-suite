
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
    CMenu SysMenu = GetSystemMenu( FALSE );
    if( IsMenu( SysMenu ) )
    {
        TCHAR szAboutMenu[ 256 ];
        if( LoadString( _Module.GetResourceInstance(),
                        IDS_ABOUTBOX, szAboutMenu, 255 ) > 0 )
        {
            SysMenu.AppendMenu( MF_SEPARATOR );
            SysMenu.AppendMenu( MF_STRING, IDM_ABOUTBOX, szAboutMenu );
        }
    }
    SysMenu.Detach();

    //Register object for message filtering and idle updates
    CMessageLoop* pLoop = _Module.GetMessageLoop();
    ATLASSERT( pLoop != NULL );
    pLoop->AddMessageFilter( this );
    pLoop->AddIdleHandler( this );

    UIAddChildWindowContainer( m_hWnd );

    CEdit workingDir;
    workingDir.Attach( GetDlgItem( IDC_DIR ) );
    ATL::CString workingDirText( "C:\\" );
    workingDir.SetWindowTextA( workingDirText );

    CEdit computerName;
    computerName.Attach( GetDlgItem( IDC_PCNAME ) );
    ATL::CString computerNameText( "localhost" );
    computerName.SetWindowTextA( computerNameText );

    CEdit computerPort;
    computerPort.Attach( GetDlgItem( IDC_PORT ) );
    ATL::CString computerPortText( "1239" );
    computerPort.SetWindowTextA( computerPortText );

    return TRUE;
}
////////////////////////////////////////////////////////////////////////////////
LRESULT CMainDlg::OnDestroy( UINT, WPARAM, LPARAM, BOOL& )
{
    //Unregister message filtering and idle updates
    CMessageLoop* pLoop = _Module.GetMessageLoop();
    ATLASSERT( pLoop != NULL );
    pLoop->RemoveMessageFilter( this );
    pLoop->RemoveIdleHandler( this );

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
        CEdit workingDir;
        workingDir.Attach( GetDlgItem( IDC_DIR ) );
        ATL::CString workingDirText;
        workingDir.GetWindowText( workingDirText );

        CEdit computerName;
        computerName.Attach( GetDlgItem( IDC_PCNAME ) );
        ATL::CString computerNameText;
        computerName.GetWindowText( computerNameText );

        CEdit computerPort;
        computerPort.Attach( GetDlgItem( IDC_PORT ) );
        ATL::CString computerPortText;
        computerPort.GetWindowText( computerPortText );

        m_corbaUnitManager = new CorbaUnitManager( this );
        m_corbaUnitManager->SetComputerNameUnitNameAndPort(
            workingDirText, computerNameText , computerPortText, "PowersimUnit" );
        m_corbaUnitManager->RunORB();
        m_unitObject = m_corbaUnitManager->GetUnitObject();

        //Test Code
        SIPParser* sipParser = m_corbaUnitManager->CreateParser();
        //std::string workingDir( "C:/Documents and Settings/kochjb/Desktop/" );
        sipParser->SetWorkingDir( std::string( workingDirText ) );
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
    LPITEMIDLIST pidl = SHBrowseForFolder ( &bi );
    if( pidl != 0 )
    {
        //Get the name of the folder
        TCHAR path[ MAX_PATH ];
        if ( SHGetPathFromIDList( pidl, path ) )
        {
            CEdit workingDir;
            workingDir.Attach( GetDlgItem( IDC_DIR ) );
            ATL::CString workingDirText( path );
            workingDir.SetWindowTextA( workingDirText + "\\" );
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
