// --- My Includes --- //
#include "HyperLabUI.h"
#include "HyperLabUIDialog.h"

// --- wxWidgets Includes --- //
#include <wx/wx.h>

IMPLEMENT_DYNAMIC_CLASS( HyperLabUI, UIPluginBase )

////////////////////////////////////////////////////////////////////////////////
HyperLabUI::HyperLabUI()
{
    RegistVar( "portNumber", &portNumber );

    name = _("HyperLab");

    wxImage my_img( _("Icons/hyper.xpm") );
    icon_w = static_cast< int >( my_img.GetWidth() );
    icon_h = static_cast< int >( my_img.GetHeight() );
    my_icon = new wxBitmap( my_img.Scale( icon_w, icon_h ) );

    n_pts = 4;

    poly[ 0 ] = wxPoint( 0, 0 );
    poly[ 1 ] = wxPoint( icon_w, 0 );
    poly[ 2 ] = wxPoint( icon_w, icon_h );
    poly[ 3 ] = wxPoint( 0, icon_h );
}
////////////////////////////////////////////////////////////////////////////////
HyperLabUI::~HyperLabUI()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
double HyperLabUI::GetVersion()
{
    double result = 1.0;

    return result;
}
////////////////////////////////////////////////////////////////////////////////
int HyperLabUI::GetNumPoly()
{
    int result = 0;

    return n_pts;
}
////////////////////////////////////////////////////////////////////////////////
int HyperLabUI::GetNumIports()
{
    int result = 0;

    return result;
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabUI::GetIPorts( POLY &iports )
{
    return;
}
////////////////////////////////////////////////////////////////////////////////
int HyperLabUI::GetNumOports()
{
    int result = 0;

    return result;
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabUI::GetOPorts( POLY &oports )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabUI::DrawIcon( wxDC* dc )
{
    dc->DrawBitmap( *my_icon, pos.x, pos.y );
}
////////////////////////////////////////////////////////////////////////////////
ves::conductor::UIDialog* HyperLabUI::UI( wxWindow* parent )
{
    if( dlg != NULL )
    {
        return dlg;
    }

    dlg = new HyperLabUIDialog( parent, -1, serviceList, &portNumber );
    ConfigurePluginDialogs( dlg );

    return dlg;
}
////////////////////////////////////////////////////////////////////////////////
wxString HyperLabUI::GetConductorName()
{
    wxString result( _("NETL_HyperLab") );

    return result;
}
////////////////////////////////////////////////////////////////////////////////
wxString HyperLabUI::GetName()
{
    if( name.IsEmpty() )
    {
        name = _("PleaseDefineClassName");
    }

    return name;
}
////////////////////////////////////////////////////////////////////////////////
wxString HyperLabUI::GetDesc()
{
    wxString result = _("None");

    return result;
}
////////////////////////////////////////////////////////////////////////////////
