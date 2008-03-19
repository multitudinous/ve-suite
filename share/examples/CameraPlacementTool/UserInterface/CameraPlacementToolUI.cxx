// --- My Includes --- //
#include "CameraPlacementToolUI.h"
#include "CameraPlacementToolUIDialog.h"

#include "Icons/camera.xpm"

// --- VE-Suite Includes --- //
#include <ves/open/xml/DataValuePair.h>

// --- wxWidgets Includes --- //
#include <wx/wx.h>

using namespace cpt;

IMPLEMENT_DYNAMIC_CLASS( CameraPlacementToolUI, UIPluginBase )

////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolUI::CameraPlacementToolUI()
{
    name = wxT( "CameraPlacementTool" );

    wxImage my_img( camera_xpm );
    icon_w = static_cast< int >( my_img.GetWidth() * 0.5f );
    icon_h = static_cast< int >( my_img.GetHeight() * 0.5f );
    my_icon = new wxBitmap( my_img.Scale( icon_w, icon_h ) );

    n_pts = 4;

    poly[ 0 ] = wxPoint( 0, 0 );
    poly[ 1 ] = wxPoint( icon_w, 0 );
    poly[ 2 ] = wxPoint( icon_w, icon_h );
    poly[ 3 ] = wxPoint( 0, icon_h );
}
////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolUI::~CameraPlacementToolUI()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
double CameraPlacementToolUI::GetVersion()
{
    double result = 1.0;

    return result;
}
////////////////////////////////////////////////////////////////////////////////
int CameraPlacementToolUI::GetNumPoly()
{
    int result = 0;

    return n_pts;
}
////////////////////////////////////////////////////////////////////////////////
int CameraPlacementToolUI::GetNumIports()
{
    int result = 0;

    return result;
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUI::GetIPorts( POLY &iports )
{
    return;
}
////////////////////////////////////////////////////////////////////////////////
int CameraPlacementToolUI::GetNumOports()
{
    int result = 0;

    return result;
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUI::GetOPorts( POLY &oports )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUI::DrawIcon( wxDC* dc )
{
    dc->DrawBitmap( *my_icon, pos.x, pos.y );
}
////////////////////////////////////////////////////////////////////////////////
ves::conductor::UIDialog* CameraPlacementToolUI::UI( wxWindow* parent )
{
    if( dlg != NULL )
    {
        return dlg;
    }

    dlg = new cpt::CameraPlacementToolUIDialog( parent, -1, serviceList );
    ConfigurePluginDialogs( dlg );

    return dlg;
}
////////////////////////////////////////////////////////////////////////////////
wxString CameraPlacementToolUI::GetConductorName()
{         
    wxString result = wxT( "CameraPlacementTool" );

    return result;
}
////////////////////////////////////////////////////////////////////////////////
wxString CameraPlacementToolUI::GetName()
{
    if( name.IsEmpty() )
    {
        name = wxT( "PleaseDefineClassName" );
    }

    return name;
}
////////////////////////////////////////////////////////////////////////////////
wxString CameraPlacementToolUI::GetDesc()
{
    wxString result = wxT( "None" );

    return result;
}
////////////////////////////////////////////////////////////////////////////////
