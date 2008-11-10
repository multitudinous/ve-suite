// OPPDUnitDlg.h : header file
//

#pragma once

class COPPDUnitDlgAutoProxy;


// COPPDUnitDlg dialog
class COPPDUnitDlg : public CDialog
{
	DECLARE_DYNAMIC(COPPDUnitDlg);
	friend class COPPDUnitDlgAutoProxy;

// Construction
public:
	COPPDUnitDlg(CWnd* pParent = NULL);	// standard constructor
	virtual ~COPPDUnitDlg();

// Dialog Data
	enum { IDD = IDD_OPPDUNIT_DIALOG };

	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support


// Implementation
protected:
	COPPDUnitDlgAutoProxy* m_pAutoProxy;
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
