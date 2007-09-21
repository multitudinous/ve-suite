// TesterDlg.h : header file
//

#pragma once

class CTesterDlgAutoProxy;


// CTesterDlg dialog
class CTesterDlg : public CDialog
{
	DECLARE_DYNAMIC(CTesterDlg);
	friend class CTesterDlgAutoProxy;

// Construction
public:
	CTesterDlg(CWnd* pParent = NULL);	// standard constructor
	virtual ~CTesterDlg();

// Dialog Data
	enum { IDD = IDD_TESTER_DIALOG };

	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support


// Implementation
protected:
	CTesterDlgAutoProxy* m_pAutoProxy;
	HICON m_hIcon;

	BOOL CanExit();

	// Generated message map functions
	virtual BOOL OnInitDialog();
	afx_msg void OnSysCommand(UINT nID, LPARAM lParam);
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	afx_msg void OnClose();
	virtual void OnOK();
	virtual void OnCancel();
	DECLARE_MESSAGE_MAP()
};
