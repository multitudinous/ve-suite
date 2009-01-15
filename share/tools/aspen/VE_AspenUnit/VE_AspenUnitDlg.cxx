// VE_AspenUnitDlg.cpp : implementation file
//

#include "stdafx.h"
#include "VE_AspenUnit.h"
#include "VE_AspenUnitDlg.h"

#include "AspenUnit_i.h"
#include "CorbaUnitManager.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif


// CVE_AspenUnitDlg dialog
CVE_AspenUnitDlg::CVE_AspenUnitDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CVE_AspenUnitDlg::IDD, pParent)
{
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
}

void CVE_AspenUnitDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
}

BEGIN_MESSAGE_MAP(CVE_AspenUnitDlg, CDialog)
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	//}}AFX_MSG_MAP
	ON_BN_CLICKED(IDCANCEL, &CVE_AspenUnitDlg::OnBnClickedCancel)
	ON_BN_CLICKED(IDC_OK, &CVE_AspenUnitDlg::OnBnClickedOk)
	ON_BN_CLICKED(IDC_BUTTON2, &CVE_AspenUnitDlg::OnBnClickedButton2)
END_MESSAGE_MAP()


// CVE_AspenUnitDlg message handlers

BOOL CVE_AspenUnitDlg::OnInitDialog()
{
	CDialog::OnInitDialog();

	// Set the icon for this dialog.  The framework does this automatically
	//  when the application's main window is not a dialog
	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon
	
	/*commManager = new CorbaUnitManager(this);
	commManager->SetComputerNameUnitNameAndPort( "dell29", "1239", "AspenUnit" );
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

    CString path = AfxGetApp()->GetProfileString( _T("Aspen"), _T("Path"), _T("C:\\") );
    CString name = AfxGetApp()->GetProfileString( _T("Aspen"), _T("Name"), _T("localhost") );
    CString port = AfxGetApp()->GetProfileString( _T("Aspen"), _T("Port"), _T("1239") );
    CEdit *Display;
    Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT3));
	Display->SetWindowTextA(name);
	Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT4));
	Display->SetWindowTextA(port);
	Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT5));
	Display->SetWindowTextA( path );


	// TODO: Add extra initialization here	
	initialized = false;
    //commManager = 0;
    //unitObject = 0;
	return TRUE;  // return TRUE  unless you set the focus to a control
}

// If you add a minimize button to your dialog, you will need the code below
//  to draw the icon.  For MFC applications using the document/view model,
//  this is automatically done for you by the framework.

void CVE_AspenUnitDlg::OnPaint()
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
HCURSOR CVE_AspenUnitDlg::OnQueryDragIcon()
{
	return static_cast<HCURSOR>(m_hIcon);
}


BOOL CVE_AspenUnitDlg::OnIdle( LONG test )
{
	Sleep( 100 );
	if(initialized)
	{
	    commManager->CheckCORBAWork();
	}
	return FALSE;
}

LRESULT CVE_AspenUnitDlg::WindowProc(UINT message, 
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
void CVE_AspenUnitDlg::OnBnClickedCancel()
{   
    CEdit *Display;
    Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT5));
    CString path;
    Display->GetWindowText( path );
    AfxGetApp()->WriteProfileString( _T("Aspen"), _T("Path"), path );
    Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT3));
    CString name;
    Display->GetWindowText( name );
    AfxGetApp()->WriteProfileString( _T("Aspen"), _T("Name"), name );
    Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT4));
    CString port;
    Display->GetWindowText( port );
    AfxGetApp()->WriteProfileString( _T("Aspen"), _T("Port"), port );

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

void CVE_AspenUnitDlg::OnBnClickedOk()
{
	if(!initialized)
	{
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
        //commManager->SetComputerNameUnitNameAndPort( "localhost", "1239", "AspenUnit" );
        commManager->SetComputerNameUnitNameAndPort( dir, name , port, "AspenUnit" );
        commManager->RunORB();
        unitObject = commManager->GetUnitObject();
        if ( !unitObject )
        {
            AfxMessageBox( _T("Unable to connect to VE-CE" ));
            //delete unitObject;
            commManager->DestroyORB();
            delete commManager;
            commManager = NULL;
            initialized = false;
            return;
        }
        else
        {
		    initialized = true;
            GetDlgItem(IDC_OK)->EnableWindow(FALSE);
        }
	}
}

void CVE_AspenUnitDlg::OnBnClickedButton2()
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
