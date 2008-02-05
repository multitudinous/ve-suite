// --- My Includes --- //
#include "ExcelWrap.h"
#include "HyperLabUnit_i.h"
#include "HyperLabUnit_client.h"

//#include "Excel_2000/CApplication.h"
#include "Excel_2003/CApplication.h"

// --- C/C++ Libraries --- //
#include <vector>
#include <string>

#pragma once

//CHyperLabUnitDlg dialog
class CHyperLabUnitDlg : public CDialog
{
public:
    //Standard Constructor
    CHyperLabUnitDlg( CWnd* pParent = NULL );
    ~CHyperLabUnitDlg();

    //Dialog Data
    enum{ IDD = IDD_HYPERLABUNIT_DIALOG };

    //void SetVESUnit(Body_Unit_i* unitPtr);
    void OnEnterIdle( UINT nWhy, CWnd* pWho );
    virtual LRESULT WindowProc( UINT message, WPARAM wParam, LPARAM lParam );
    BOOL OnIdle( LONG lCount );

    CEdit editComputerName;
    CEdit editPortNumber;
    CEdit editUnitName;
    afx_msg void OnBnClickedOpen();
    afx_msg void OnBnClickedConnect();
    afx_msg void OnEnChangePort();
    afx_msg void OnEnChangeComputer();
    afx_msg void OnEnChangeUnit();

    CApplication app;
    CorbaUnitManager* commManager;
    Body_Unit_i* unitObject;

protected:
    //DDX/DDV support
    virtual void DoDataExchange( CDataExchange* pDX );

    //Implementation
    HICON m_hIcon;

    //Generated message map functions
    BOOL OnInitDialog();

    afx_msg void OnSysCommand( UINT nID, LPARAM lParam );
    afx_msg void OnPaint();
    afx_msg HCURSOR OnQueryDragIcon();
    DECLARE_MESSAGE_MAP()

private:
    std::vector< std::string > fileList;
    CString path;
    CString computerName;
    CString portNumber;
    CString unitName;

    ExcelWrap excelWrap;
};
