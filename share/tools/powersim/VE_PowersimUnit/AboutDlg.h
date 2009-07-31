
#ifndef CABOUT_DLG_H
#define CABOUT_DLG_H

// --- ATL Includes --- //
#include <atlbase.h>
#include <atlwin.h>

class CAboutDlg : public CDialogImpl< CAboutDlg >
{
public:
    enum { IDD = IDD_ABOUTDLG };

    BEGIN_MSG_MAP( CAboutDlg )
        MESSAGE_HANDLER( WM_INITDIALOG, OnInitDialog )
        COMMAND_ID_HANDLER( IDOK, OnCloseCmd )
        COMMAND_ID_HANDLER( IDCANCEL, OnCloseCmd )
    END_MSG_MAP()

    LRESULT OnInitDialog( UINT, WPARAM, LPARAM, BOOL& );
    LRESULT OnCloseCmd( WORD, WORD wID, HWND, BOOL& );

};

#endif //CABOUT_DLG_H
