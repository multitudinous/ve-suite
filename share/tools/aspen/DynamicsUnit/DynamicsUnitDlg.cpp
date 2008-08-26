// DynamicsUnitDlg.cpp : implementation file
//

#include "stdafx.h"
#include "DynamicsUnit.h"
#include "DynamicsUnitDlg.h"
#include "dynparser.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CAboutDlg dialog used for App About

class CAboutDlg : public CDialog
{
public:
	CAboutDlg();

// Dialog Data
	//{{AFX_DATA(CAboutDlg)
	enum { IDD = IDD_ABOUTBOX };
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CAboutDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	//{{AFX_MSG(CAboutDlg)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

CAboutDlg::CAboutDlg() : CDialog(CAboutDlg::IDD)
{
	//{{AFX_DATA_INIT(CAboutDlg)
	//}}AFX_DATA_INIT
}

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAboutDlg)
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
	//{{AFX_MSG_MAP(CAboutDlg)
		// No message handlers
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CDynamicsUnitDlg dialog

CDynamicsUnitDlg::CDynamicsUnitDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CDynamicsUnitDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CDynamicsUnitDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
	// Note that LoadIcon does not require a subsequent DestroyIcon in Win32
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
}

void CDynamicsUnitDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CDynamicsUnitDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CDynamicsUnitDlg, CDialog)
	//{{AFX_MSG_MAP(CDynamicsUnitDlg)
	ON_WM_SYSCOMMAND()
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	ON_BN_CLICKED(IDC_BUTTON1, OnButton1)
	//}}AFX_MSG_MAP
	ON_BN_CLICKED(IDCANCEL, OnBnClickedCancel)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CDynamicsUnitDlg message handlers

BOOL CDynamicsUnitDlg::OnInitDialog()
{
	CDialog::OnInitDialog();

	// Add "About..." menu item to system menu.

	// IDM_ABOUTBOX must be in the system command range.
	ASSERT((IDM_ABOUTBOX & 0xFFF0) == IDM_ABOUTBOX);
	ASSERT(IDM_ABOUTBOX < 0xF000);

	CMenu* pSysMenu = GetSystemMenu(FALSE);
	if (pSysMenu != NULL)
	{
		CString strAboutMenu;
		strAboutMenu.LoadString(IDS_ABOUTBOX);
		if (!strAboutMenu.IsEmpty())
		{
			pSysMenu->AppendMenu(MF_SEPARATOR);
			pSysMenu->AppendMenu(MF_STRING, IDM_ABOUTBOX, strAboutMenu);
		}
	}

	// Set the icon for this dialog.  The framework does this automatically
	//  when the application's main window is not a dialog
	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon
	
	// TODO: Add extra initialization here
	
	return TRUE;  // return TRUE  unless you set the focus to a control
}

void CDynamicsUnitDlg::OnSysCommand(UINT nID, LPARAM lParam)
{
	if ((nID & 0xFFF0) == IDM_ABOUTBOX)
	{
		CAboutDlg dlgAbout;
		dlgAbout.DoModal();
	}
	else
	{
		CDialog::OnSysCommand(nID, lParam);
	}
}

// If you add a minimize button to your dialog, you will need the code below
//  to draw the icon.  For MFC applications using the document/view model,
//  this is automatically done for you by the framework.

void CDynamicsUnitDlg::OnPaint() 
{
	if (IsIconic())
	{
		CPaintDC dc(this); // device context for painting

		SendMessage(WM_ICONERASEBKGND, (WPARAM) dc.GetSafeHdc(), 0);

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

// The system calls this to obtain the cursor to display while the user drags
//  the minimized window.
HCURSOR CDynamicsUnitDlg::OnQueryDragIcon()
{
	return (HCURSOR) m_hIcon;
}

void CDynamicsUnitDlg::OnOK() 
{
	// TODO: Add extra validation here
    hAPsim = new IAspenModeler(); 
	BOOL bSuccess = hAPsim->CreateDispatch(_T("AD Application"));

	CEdit *Display;
	Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT1));

	CString str;
	Display->GetWindowText( str );
	COleVariant strVar(str);
	VARIANT var=strVar.Detach();

	DynParser dyn;
	dyn.ParseFile( str );

	hAPsim->OpenDocument(var);

	VARIANT cv;
	cv.vt=VT_BOOL;                      //Type is BOOL
    cv.boolVal=VARIANT_TRUE;      //value is True (VARIANT_TRUE)
	
	hAPsim->SetVisible(cv);
	
	//CDialog::OnOK();
}

void CDynamicsUnitDlg::OnButton1() 
{
	// TODO: Add your control notification handler code here
	char strFilter[] = { "Dynamics Files (*.dynf)|*.dynf|All Files (*.*)|*.*||" };
	CFileDialog FileDlg(TRUE, ".dynf", NULL, 0, strFilter);
	if( FileDlg.DoModal() == IDOK )
	{
		CEdit *Display;
		Display = reinterpret_cast<CEdit *>(GetDlgItem(IDC_EDIT1));
		Display->SetWindowTextA(FileDlg.GetPathName());
	}
	else
		return;
	
}

void CDynamicsUnitDlg::OnBnClickedCancel()
{
	VARIANTARG reserved;
	::VariantInit(&reserved);
	reserved.vt=VT_BOOL;
	reserved.boolVal=VARIANT_FALSE;
	
	hAPsim->CloseDocument(reserved);
	hAPsim->ReleaseDispatch();
	hAPsim->Quit();
    //delete hAPsim;
	//hAPsim = NULL;

	// TODO: Add your control notification handler code here
	//OnCancel();
}
