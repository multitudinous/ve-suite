
// --- VE_PowersimUnit Includes --- //
#include "StdAtl.h"
#include "Resource.h"
#include "AboutDlg.h"

////////////////////////////////////////////////////////////////////////////////
LRESULT CAboutDlg::OnInitDialog(
    UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/ )
{
    CenterWindow( GetParent() );

    return TRUE;
}

////////////////////////////////////////////////////////////////////////////////
LRESULT CAboutDlg::OnCloseCmd(
    WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/ )
{
    EndDialog( wID );

    return 0;
}
////////////////////////////////////////////////////////////////////////////////
