// --- My Includes --- //
#include "ConstructionBotsUI.h"
#include "ConstructionBotsUIDialog.h"

#include "Icons/ConstructionBots.xpm"

// --- VE-Suite Includes --- //
#include <ves/open/xml/DataValuePair.h>

// --- wxWidgets Includes --- //
#include <wx/wx.h>

IMPLEMENT_DYNAMIC_CLASS( ConstructionBotsUI, UIPluginBase )

////////////////////////////////////////////////////////////////////////////////
ConstructionBotsUI::ConstructionBotsUI()
{
    name = wxT( "ConstructionBots" );

    wxImage my_img( ConstructionBots_xpm );
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
ConstructionBotsUI::~ConstructionBotsUI()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
double ConstructionBotsUI::GetVersion()
{
    double result = 1.0;

    return result;
}
////////////////////////////////////////////////////////////////////////////////
int ConstructionBotsUI::GetNumPoly()
{
    int result = 0;

    return n_pts;
}
////////////////////////////////////////////////////////////////////////////////
int ConstructionBotsUI::GetNumIports()
{
    int result = 0;

    return result;
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionBotsUI::GetIPorts( POLY &iports )
{
    return;
}
////////////////////////////////////////////////////////////////////////////////
int ConstructionBotsUI::GetNumOports()
{
    int result = 0;

    return result;
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionBotsUI::GetOPorts( POLY &oports )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionBotsUI::DrawIcon( wxDC* dc )
{
    dc->DrawBitmap( *my_icon, pos.x, pos.y );
}
////////////////////////////////////////////////////////////////////////////////
ves::conductor::UIDialog* ConstructionBotsUI::UI( wxWindow* parent )
{
    if( dlg != NULL )
    {
        return dlg;
    }

    dlg = new ConstructionBotsUIDialog( parent, -1, serviceList );
    ConfigurePluginDialogs( dlg );

    return dlg;
}
////////////////////////////////////////////////////////////////////////////////
wxString ConstructionBotsUI::GetConductorName()
{         
    wxString result = wxT( "NETL_ConstructionBots" );

    return result;
}
////////////////////////////////////////////////////////////////////////////////
wxString ConstructionBotsUI::GetName()
{
    if( name.IsEmpty() )
    {
        name = wxT( "PleaseDefineClassName" );
    }

    return name;
}
////////////////////////////////////////////////////////////////////////////////
wxString ConstructionBotsUI::GetDesc()
{
    wxString result = wxT( "None" );

    return result;
}
////////////////////////////////////////////////////////////////////////////////
