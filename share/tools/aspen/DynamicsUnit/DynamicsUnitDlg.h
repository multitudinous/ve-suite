// DynamicsUnitDlg.h : header file
//

#if !defined(AFX_DYNAMICSUNITDLG_H__8059ED00_B2A1_4E50_A7EC_1DD9E0431448__INCLUDED_)
#define AFX_DYNAMICSUNITDLG_H__8059ED00_B2A1_4E50_A7EC_1DD9E0431448__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/////////////////////////////////////////////////////////////////////////////
// CDynamicsUnitDlg dialog

class CDynamicsUnitDlg : public CDialog
{
// Construction
public:
	CDynamicsUnitDlg(CWnd* pParent = NULL);	// standard constructor

// Dialog Data
	//{{AFX_DATA(CDynamicsUnitDlg)
	enum { IDD = IDD_DYNAMICSUNIT_DIALOG };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDynamicsUnitDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	HICON m_hIcon;

	// Generated message map functions
	//{{AFX_MSG(CDynamicsUnitDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnSysCommand(UINT nID, LPARAM lParam);
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	virtual void OnOK();
	afx_msg void OnButton1();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
	IAspenModeler * hAPsim;

public:
	afx_msg void OnBnClickedCancel();
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_DYNAMICSUNITDLG_H__8059ED00_B2A1_4E50_A7EC_1DD9E0431448__INCLUDED_)
