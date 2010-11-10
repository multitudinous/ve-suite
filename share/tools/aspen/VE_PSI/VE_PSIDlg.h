#pragma once
#include "afxwin.h"
#include <string>

class VEPSI_i;
class CorbaUnitManager;
class CDialogThread;

class VE_PSIDlg : public CDialog
{
protected:
    virtual void DoDataExchange(CDataExchange* pDX);
    virtual LRESULT WindowProc(UINT message, WPARAM wParam, LPARAM lParam);
    BOOL OnIdle( LONG lCount );
    HICON m_hIcon;
    virtual BOOL OnInitDialog();
    afx_msg void OnPaint();
    afx_msg HCURSOR OnQueryDragIcon();
    DECLARE_MESSAGE_MAP()

private:
    VEPSI_i* unitObject;
    CString moduleName;
    CorbaUnitManager* commManager;
    CString computerName;
    CString unitName;
    CString portNumber;
    afx_msg void OnBnClickedCancel();
    bool initialized;
    std::string testing2;

public:
    VE_PSIDlg(CWnd* pParent = NULL);
    enum { IDD = IDD_VE_ASPENUNIT_DIALOG };
    afx_msg void OnBnClickedOk();
    afx_msg void OnBnClickedButton2();
};
