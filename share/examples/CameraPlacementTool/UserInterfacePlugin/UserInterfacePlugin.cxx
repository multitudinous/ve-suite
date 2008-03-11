// --- My Includes --- //
#include "UserInterfacePlugin.h"
#include "UserInterfaceDialog.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/DataValuePair.h>

// --- wxWidgets Includes --- //
#include <wx/wx.h>

using namespace cpt;

IMPLEMENT_DYNAMIC_CLASS( UserInterfacePlugin, UIPluginBase )

////////////////////////////////////////////////////////////////////////////////
UserInterfacePlugin::UserInterfacePlugin()
{
    RegistVar( "portNumber", &portNumber );

    name = _( "CameraPlacementTool" );

    wxImage my_img( _( "Icons/INLBlueBanner.jpg" ) );
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
UserInterfacePlugin::~UserInterfacePlugin()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
double UserInterfacePlugin::GetVersion()
{
    double result = 1.0;
    //Your code

    return result;
}
////////////////////////////////////////////////////////////////////////////////
int UserInterfacePlugin::GetNumPoly()
{
    int result = 0;
    //Your code

    return n_pts;
}
////////////////////////////////////////////////////////////////////////////////
int UserInterfacePlugin::GetNumIports()
{
    int result = 0;

    return result;
}
////////////////////////////////////////////////////////////////////////////////
void UserInterfacePlugin::GetIPorts( POLY &iports )
{
    return;
}
////////////////////////////////////////////////////////////////////////////////
int UserInterfacePlugin::GetNumOports()
{
    int result = 0;
    //Your code

    return result;
}
////////////////////////////////////////////////////////////////////////////////
void UserInterfacePlugin::GetOPorts( POLY &oports )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void UserInterfacePlugin::DrawIcon( wxDC* dc )
{
    //Your implementation
    dc->DrawBitmap( *my_icon, pos.x, pos.y );
}
////////////////////////////////////////////////////////////////////////////////
ves::conductor::UIDialog* UserInterfacePlugin::UI( wxWindow* parent )
{
    if( dlg != NULL )
    {
        return dlg;
    }

    dlg = new cpt::UserInterfaceDialog( parent, -1, serviceList, &portNumber );
    ConfigurePluginDialogs( dlg );

    return dlg;
}
////////////////////////////////////////////////////////////////////////////////
wxString UserInterfacePlugin::GetConductorName()
{         
    //Your name
    wxString result = _( "INL_CameraPlacementTool" );

    return result;
}
////////////////////////////////////////////////////////////////////////////////
wxString UserInterfacePlugin::GetName()
{
    if( name.IsEmpty() )
    {
        name = _( "PleaseDefineClassName" );
    }

    return name;
}
////////////////////////////////////////////////////////////////////////////////
wxString UserInterfacePlugin::GetDesc()
{
    //Your description
    wxString result = _( "None" );

    return result;
}
////////////////////////////////////////////////////////////////////////////////
