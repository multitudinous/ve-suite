// VE_DynamicsUnitDlg.cpp : implementation file
//

#include "stdafx.h"
#include "VE_DynamicsUnit.h"
#include "VE_DynamicsUnitDlg.h"

#include "DynamicsUnit_i.h"
#include "CorbaUnitManager.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif


// CVE_DynamicsUnitDlg dialog
CVE_DynamicsUnitDlg::CVE_DynamicsUnitDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CVE_DynamicsUnitDlg::IDD, pParent)
{
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
}

void CVE_DynamicsUnitDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
}

BEGIN_MESSAGE_MAP(CVE_DynamicsUnitDlg, CDialog)
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	//}}AFX_MSG_MAP
	ON_BN_CLICKED(IDCANCEL, &CVE_DynamicsUnitDlg::OnBnClickedCancel)
	ON_BN_CLICKED(IDC_OK, &CVE_DynamicsUnitDlg::OnBnClickedOk)
	ON_BN_CLICKED(IDC_BUTTON2, &CVE_DynamicsUnitDlg::OnBnClickedButton2)
END_MESSAGE_MAP()


// CVE_DynamicsUnitDlg message handlers

BOOL CVE_DynamicsUnitDlg::OnInitDialog()
{
	CDialog::OnInitDialog();

	// Set the icon for this dialog.  The framework does this automatically
	//  when the application's main window is not a dialog
	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon
	
	/*commManager = new CorbaUnitManager(this);
	commManager->SetComputerNameUnitNameAndPort( "dell29", "1239", "DynamicsUnit" );
	commManager->RunORB();
	unitObject = commManager->GetUnitObject();
	//if ( unitObject )
	//{
	//   AfxMessageBox( _T("Connected to VE-CE" ));
	//}
	// else
	if ( !unitObject )
	{
		AfxMessageBox( _T("Unable to connect to VE-CE" ));
	}*/

    CString path = AfxGetApp()->GetProfileString( _T("Dynamics"), _T("Path"), _T("C:\\") );
    CString name = AfxGetApp()->GetProfileString( _T("Dynamics"), _T("Name"), _T("localhost") );
    CString port = AfxGetApp()->GetProfileString( _T("Dynamics"), _T("Port"), _T("1239") );
    CEdit *Display;
    Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT3));
	Display->SetWindowTextA(name);
	Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT4));
	Display->SetWindowTextA(port);
	Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT5));
	Display->SetWindowTextA( path );


	// TODO: Add extra initialization here	
	initialized = false;
	return TRUE;  // return TRUE  unless you set the focus to a control
}

// If you add a minimize button to your dialog, you will need the code below
//  to draw the icon.  For MFC applications using the document/view model,
//  this is automatically done for you by the framework.

void CVE_DynamicsUnitDlg::OnPaint()
{
	if (IsIconic())
	{
		CPaintDC dc(this); // device context for painting

		SendMessage(WM_ICONERASEBKGND, reinterpret_cast<WPARAM>(dc.GetSafeHdc()), 0);

		// Center icon in client rectangle
		int cxIcon = GetSystemMetrics(SM_CXICON);
		int cyIcon = GetSystemMetrics(SM_CYICON);
		CRect rect;
		GetClientRect(&rect);
		int x = (rect.Width() - cxIcon + 1) / 2;
		int y = (rect.Height() - cyIcon + 1) / 2;

		// Draw the icon
		dc.DrawIcon(x, y, m_hIcon);
	}
	else
	{
		CDialog::OnPaint();
	}
}

// The system calls this function to obtain the cursor to display while the user drags
//  the minimized window.
HCURSOR CVE_DynamicsUnitDlg::OnQueryDragIcon()
{
	return static_cast<HCURSOR>(m_hIcon);
}


BOOL CVE_DynamicsUnitDlg::OnIdle( LONG test )
{
	Sleep( 100 );
	if(initialized)
	{
	    commManager->CheckCORBAWork();
	}
	return FALSE;
}

LRESULT CVE_DynamicsUnitDlg::WindowProc(UINT message, 
                WPARAM wParam, LPARAM lParam)
{
    DWORD QueueStatus;
    LRESULT resValue = 0;
    bool OnIdleRetVal = true;

    if(message == WM_ENTERIDLE) {
        OnIdleRetVal = OnIdle((UINT)wParam);
        if(!OnIdleRetVal)
            wParam = 0;
    } else
        resValue = CDialog::WindowProc(message, 
        wParam, lParam);

    QueueStatus = GetQueueStatus(QS_ALLINPUT);

    if(HIWORD(QueueStatus) == 0)
        PostMessage(WM_ENTERIDLE, 
            wParam + (OnIdleRetVal ? 1 : 0), 0);

    return resValue;
}
void CVE_DynamicsUnitDlg::OnBnClickedCancel()
{   
    CEdit *Display;
    Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT5));
    CString path;
    Display->GetWindowText( path );
    AfxGetApp()->WriteProfileString( _T("Dynamics"), _T("Path"), path );
    Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT3));
    CString name;
    Display->GetWindowText( name );
    AfxGetApp()->WriteProfileString( _T("Dynamics"), _T("Name"), name );
    Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT4));
    CString port;
    Display->GetWindowText( port );
    AfxGetApp()->WriteProfileString( _T("Dynamics"), _T("Port"), port );

	if(commManager != NULL)
	{
		//delete unitObject;
		commManager->DestroyORB();
		delete commManager;
        commManager = NULL;
	}
	// TODO: Add your control notification handler code here
	this->OnOK();
}

void CVE_DynamicsUnitDlg::OnBnClickedOk()
{
	if(!initialized)
	{
        GetDlgItem(IDC_OK)->EnableWindow(FALSE);
        // TODO: Add your control notification handler code here
        CEdit *Display;
        Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT3));
        CString name;
	    Display->GetWindowText(name);
        Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT4));
        CString port;
	    Display->GetWindowText(port);
        Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT5));
        CString dir;
	    Display->GetWindowText(dir);

	    commManager = new CorbaUnitManager(this);
        //commManager->SetComputerNameUnitNameAndPort( "localhost", "1239", "DynamicsUnit" );
        commManager->SetComputerNameUnitNameAndPort( dir, name , port, "DynamicsUnit" );
        commManager->RunORB();
        unitObject = commManager->GetUnitObject();
        if ( !unitObject )
        {
            AfxMessageBox( _T("Unable to connect to VE-CE" ));
        }
		initialized = true;
	}
}

void CVE_DynamicsUnitDlg::OnBnClickedButton2()
{
	// TODO: Add your control notification handler code here
    BROWSEINFO bi = { 0 };
    bi.lpszTitle = _T("Pick a Directory");
    LPITEMIDLIST pidl = SHBrowseForFolder ( &bi );
    if ( pidl != 0 )
    {
        // get the name of the folder
        TCHAR path[MAX_PATH];
        if ( SHGetPathFromIDList ( pidl, path ) )
		{
            CEdit *Display;
            Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT5));
			CString thePath( path );
            Display->SetWindowTextA( thePath + "\\" );
        }

        // free memory used
        IMalloc * imalloc = 0;
        if ( SUCCEEDED( SHGetMalloc ( &imalloc )) )
        {
            imalloc->Free ( pidl );
            imalloc->Release ( );
        }
    }
}
