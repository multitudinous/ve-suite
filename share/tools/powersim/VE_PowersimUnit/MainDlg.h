
#ifndef CMAIN_DLG_H
#define CMAIN_DLG_H

// --- VE_PowersimUnit Includes --- //
class Body_Unit_i;
class CorbaUnitManager;

class CMainDlg : public CDialogImpl< CMainDlg >
{
public:
    enum { IDD = IDD_MAINDLG };

    CMainDlg();

    virtual BOOL PreTranslateMessage( MSG* pMsg );
    virtual BOOL OnIdle();

    BEGIN_MSG_MAP( CMainDlg )
        MESSAGE_HANDLER( WM_INITDIALOG, OnInitDialog )
        MESSAGE_HANDLER( WM_DESTROY, OnDestroy )
        COMMAND_ID_HANDLER( IDD_ABOUTDLG, OnAppAbout )
        COMMAND_ID_HANDLER( IDOK, OnOK )
        COMMAND_ID_HANDLER( IDCANCEL, OnCancel )
        COMMAND_ID_HANDLER( IDC_DIRPICKER, OnDirPicker )
        MESSAGE_HANDLER( WM_SYSCOMMAND, OnSysCommand )
    END_MSG_MAP()

    LRESULT OnInitDialog( UINT, WPARAM, LPARAM, BOOL& );
    LRESULT OnDestroy( UINT, WPARAM, LPARAM, BOOL& );
    LRESULT OnAppAbout( WORD , WORD, HWND, BOOL& );
    LRESULT OnOK( WORD, WORD wID, HWND, BOOL& );
    LRESULT OnCancel( WORD, WORD wID, HWND, BOOL& );
    LRESULT OnDirPicker( WORD, WORD wID, HWND, BOOL& );
    LRESULT OnSysCommand( UINT, WPARAM wParam, LPARAM, BOOL& bHandled );

    void CloseDialog( int nVal );

protected:

private:
    bool m_initialized;

    Body_Unit_i* m_unitObject;
    CorbaUnitManager* m_corbaUnitManager;

};

#endif //CMAIN_DLG_H
