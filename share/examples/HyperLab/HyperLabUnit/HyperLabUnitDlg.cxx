// --- My Includes --- //
#include "stdafx.h"
#include "HyperLabUnit.h"
#include "HyperLabUnitDlg.h"

#include "ExcelWrap.h"

// --- C/C++ Libraries --- //
#include <string>

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

////////////////////////////////////////////////////////////////////////////////
//CAboutDlg dialog used for App About
class CAboutDlg : public CDialog
{
public:
    CAboutDlg();

    //Dialog Data
    enum{ IDD = IDD_ABOUTBOX };

protected:
    //DDX/DDV support
    virtual void DoDataExchange( CDataExchange* pDX );

    //Implementation
    DECLARE_MESSAGE_MAP()
};

////////////////////////////////////////////////////////////////////////////////
CAboutDlg::CAboutDlg()
:
CDialog( CAboutDlg::IDD )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CAboutDlg::DoDataExchange( CDataExchange* pDX )
{
    CDialog::DoDataExchange( pDX );
}
////////////////////////////////////////////////////////////////////////////////
BEGIN_MESSAGE_MAP( CAboutDlg, CDialog )
END_MESSAGE_MAP()
////////////////////////////////////////////////////////////////////////////////
//CHyperLabUnitDlg dialog
CHyperLabUnitDlg::CHyperLabUnitDlg( CWnd* pParent /*=NULL*/ )
:
CDialog( CHyperLabUnitDlg::IDD, pParent )
{
    m_hIcon = AfxGetApp()->LoadIcon( IDR_MAINFRAME );

    COleException *e = new COleException;

    if( !app.CreateDispatch( "Excel.Application", e ) )
    {
        e->ReportError();
        AfxMessageBox( "Cannot start Excel and get Application object." );

        return;
    }

    commManager = new CorbaUnitManager();

    //app.put_Visible( TRUE );
}
////////////////////////////////////////////////////////////////////////////////
CHyperLabUnitDlg::~CHyperLabUnitDlg()
{
    app.Quit();

    if( commManager )
    {
        delete commManager;
    }
}
////////////////////////////////////////////////////////////////////////////////
void CHyperLabUnitDlg::DoDataExchange( CDataExchange* pDX )
{
    CDialog::DoDataExchange( pDX );
    DDX_Control( pDX, ID_COMPUTER, editComputerName );
    DDX_Control( pDX, ID_PORT, editPortNumber );
    DDX_Control( pDX, ID_UNIT, editUnitName );
}
////////////////////////////////////////////////////////////////////////////////
BEGIN_MESSAGE_MAP( CHyperLabUnitDlg, CDialog )
ON_WM_SYSCOMMAND()
ON_WM_PAINT()
ON_WM_QUERYDRAGICON()
//AFX_MSG_MAP
ON_BN_CLICKED( ID_OPEN, &CHyperLabUnitDlg::OnBnClickedOpen )
ON_BN_CLICKED( ID_CONNECT, &CHyperLabUnitDlg::OnBnClickedConnect )
ON_EN_CHANGE( ID_COMPUTER, &CHyperLabUnitDlg::OnEnChangeComputer )
ON_EN_CHANGE( ID_PORT, &CHyperLabUnitDlg::OnEnChangePort )
ON_EN_CHANGE( ID_UNIT, &CHyperLabUnitDlg::OnEnChangeUnit )
END_MESSAGE_MAP()
////////////////////////////////////////////////////////////////////////////////
//CHyperLabUnitDlg message handlers
BOOL CHyperLabUnitDlg::OnInitDialog()
{
    CDialog::OnInitDialog();

    //Add "About..." menu item to system menu.   

    //IDM_ABOUTBOX must be in the system command range.
    ASSERT( ( IDM_ABOUTBOX & 0xFFF0 ) == IDM_ABOUTBOX );
    ASSERT( IDM_ABOUTBOX < 0xF000 );

    CMenu* pSysMenu = GetSystemMenu( FALSE );
    if( pSysMenu != NULL )
    {
        CString strAboutMenu;
        strAboutMenu.LoadString( IDS_ABOUTBOX );
        if( !strAboutMenu.IsEmpty() )
        {
            pSysMenu->AppendMenu( MF_SEPARATOR );
            pSysMenu->AppendMenu( MF_STRING, IDM_ABOUTBOX, strAboutMenu );
        }
    }

    //Set the icon for this dialog.  The framework does this automatically
    //when the application's main window is not a dialog
    //Set big icon
    SetIcon( m_hIcon, TRUE );
    //Set small icon
    SetIcon( m_hIcon, FALSE );

    //Return TRUE, unless you set the focus to a control	
    return TRUE;
}
////////////////////////////////////////////////////////////////////////////////
void CHyperLabUnitDlg::OnSysCommand( UINT nID, LPARAM lParam )
{
    if( ( nID & 0xFFF0 ) == IDM_ABOUTBOX )
    {
        CAboutDlg dlgAbout;
        dlgAbout.DoModal();
    }
    else
    {
        CDialog::OnSysCommand( nID, lParam );
    }
}
////////////////////////////////////////////////////////////////////////////////
//If you add a minimize button to your dialog, you will need the code below
//to draw the icon.  For MFC applications using the document/view model,
//this is automatically done for you by the framework.
void CHyperLabUnitDlg::OnPaint()
{
    if( IsIconic() )
    {
        //Device context for painting
        CPaintDC dc( this );

        SendMessage( WM_ICONERASEBKGND, reinterpret_cast< WPARAM >( dc.GetSafeHdc() ), 0 );

        //Center icon in client rectangle
        int cxIcon=GetSystemMetrics( SM_CXICON );
        int cyIcon=GetSystemMetrics( SM_CYICON );
        CRect rect;
        GetClientRect( &rect );
        int x = ( rect.Width() - cxIcon + 1 ) / 2;
        int y = ( rect.Height() - cyIcon + 1 ) / 2;

        //Draw the icon
        dc.DrawIcon( x, y, m_hIcon );
    }
    else
    {
        CDialog::OnPaint();
    }
}
////////////////////////////////////////////////////////////////////////////////
//The system calls this function to obtain the cursor to display while the user drags the minimized window
HCURSOR CHyperLabUnitDlg::OnQueryDragIcon()
{
    return static_cast< HCURSOR >( m_hIcon );
}
////////////////////////////////////////////////////////////////////////////////
void CHyperLabUnitDlg::OnEnterIdle( UINT nWhy, CWnd* pWho )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
LRESULT CHyperLabUnitDlg::WindowProc( UINT message, WPARAM wParam, LPARAM lParam )
{
    DWORD QueueStatus;
    LRESULT resValue = 0;
    bool OnIdleRetVal = true;

    if( message == WM_ENTERIDLE )
    {
        OnIdleRetVal = OnIdle( ( UINT )wParam );
        if( !OnIdleRetVal )
        {
            wParam = 0;
        }
    }
    else
    {
        resValue = CDialog::WindowProc( message, wParam, lParam );
    }

    QueueStatus = GetQueueStatus( QS_ALLINPUT );

    if( HIWORD( QueueStatus ) == 0 )
    {
        PostMessage( WM_ENTERIDLE, wParam + ( OnIdleRetVal ? 1 : 0 ), 0 );
    }

    return resValue;
}
////////////////////////////////////////////////////////////////////////////////
/*
void CHyperLabUnitDlg::SetVESUnit( Body_Unit_i* unitPtr )
{
    unitObject = unitPtr;
}
*/
////////////////////////////////////////////////////////////////////////////////
BOOL CHyperLabUnitDlg::OnIdle( LONG test )
{
    Sleep( 100 );
    commManager->CheckCORBAWork();

    return FALSE;
}
////////////////////////////////////////////////////////////////////////////////
void CHyperLabUnitDlg::OnBnClickedOpen()
{
    CString fn;
    std::string fileName;
    std::string p;
    char szFilter[] = "Excel Files (*.xls)|*.xls||";
    CFileDialog dlg( TRUE, NULL, NULL, OFN_HIDEREADONLY | OFN_ALLOWMULTISELECT, szFilter );
    LPTSTR lpstrFile = new TCHAR[ 2048 ];
    *lpstrFile = _T( '\0' );

    dlg.m_ofn.lpstrFile = lpstrFile;
    dlg.m_ofn.nMaxFile = 2046;

    if( dlg.DoModal() == TRUE )
    {
        fileList.clear();
        path = dlg.GetPathName();
        p=path.GetString();
        //AfxMessageBox( path );
        POSITION pos = dlg.GetStartPosition();
        std::string tmpHold;
        while( pos )
        {
            fn = dlg.GetNextPathName( pos );
            //AfxMessageBox( fn );
            tmpHold=fn.GetString();
            for( size_t i = p.size() + 1; i < tmpHold.size(); ++i )
            {
                fileName.push_back( tmpHold[ i ] );
            }
            //fileName=fn.GetString();
            fileList.push_back( fileName );
            fileName.clear();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void CHyperLabUnitDlg::OnBnClickedConnect()
{
    commManager->RunORB( computerName.GetString(), portNumber.GetString(), unitName.GetString() );
    unitObject = commManager->GetUnitObject();

    if( unitObject )
    {
        AfxMessageBox( "Connected to VE-CE" );
    }
    else
    {
        AfxMessageBox( "Unable to connect to VE-CE" );
    }

    excelWrap.SetSensorData( &app );
}
////////////////////////////////////////////////////////////////////////////////
void CHyperLabUnitDlg::OnEnChangeComputer()
{
    editComputerName.GetWindowTextA( computerName );
}
////////////////////////////////////////////////////////////////////////////////
void CHyperLabUnitDlg::OnEnChangePort()
{
    editPortNumber.GetWindowTextA( portNumber );
}
////////////////////////////////////////////////////////////////////////////////
void CHyperLabUnitDlg::OnEnChangeUnit()
{
    editUnitName.GetWindowTextA( unitName );
}
////////////////////////////////////////////////////////////////////////////////
