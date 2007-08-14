// VE_AspenUnitDlg.h : header file
//

#pragma once
#include "afxwin.h"

class Body_Unit_i;
class CorbaUnitManager;
class CDialogThread;

// CVE_AspenUnitDlg dialog
class CVE_AspenUnitDlg : public CDialog
{
// Construction
public:
	CVE_AspenUnitDlg(CWnd* pParent = NULL);	// standard constructor

// Dialog Data
	enum { IDD = IDD_VE_ASPENUNIT_DIALOG };

	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	virtual LRESULT WindowProc(UINT message, WPARAM wParam, LPARAM lParam);
	BOOL OnIdle( LONG lCount );


// Implementation
protected:
	HICON m_hIcon;

	// Generated message map functions
	virtual BOOL OnInitDialog();
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	DECLARE_MESSAGE_MAP()

private:
	Body_Unit_i* unitObject;
	CString moduleName;
	CorbaUnitManager* commManager;
	CDialogThread* orbThread;
	CString computerName;
	CString unitName;
	CString portNumber;
	afx_msg void OnBnClickedCancel();
};
