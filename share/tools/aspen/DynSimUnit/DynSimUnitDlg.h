// DynSimUnitDlg.h : header file
//

#pragma once
#include "afxwin.h"

#define REFRESH_OPC 1

class Body_Unit_i;
class CorbaUnitManager;
class CDialogThread;

// CDynSimUnitDlg dialog
class CDynSimUnitDlg : public CDialog
{
// Construction
public:
	CDynSimUnitDlg(CWnd* pParent = NULL);	// standard constructor

// Dialog Data
	enum { IDD = IDD_DYNSIMUNIT_DIALOG };

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
    afx_msg void OnTimer (UINT TimerVal);
	DECLARE_MESSAGE_MAP()

private:
	Body_Unit_i* unitObject;
	CString moduleName;
	CorbaUnitManager* commManager;
	//CDialogThread* orbThread;
	CString computerName;
	CString unitName;
	CString portNumber;
	afx_msg void OnBnClickedCancel();
	bool initialized;
public:
	afx_msg void OnBnClickedOk();
	afx_msg void OnBnClickedButton2();
};
