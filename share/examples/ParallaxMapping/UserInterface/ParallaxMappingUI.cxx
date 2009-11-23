
// --- ParallaxMapping Includes --- //
#include "ParallaxMappingUI.h"
#include "ParallaxMappingUIDialog.h"

#include "Icons/parallax_mapping.xpm"

// --- VE-Suite Includes --- //
#include <ves/conductor/ConductorLibEnums.h>

// --- wxWidgets Includes --- //
#include <wx/wx.h>

IMPLEMENT_DYNAMIC_CLASS( ParallaxMappingUI, ves::conductor::UIPluginBase )

////////////////////////////////////////////////////////////////////////////////
ParallaxMappingUI::ParallaxMappingUI()
{
    mPluginName = wxT( "ParallaxMapping" );

    wxImage image( parallax_mapping_xpm );

    float scale = 0.10;
    int iconW = static_cast< int >( ( image.GetWidth() - 1 ) * scale );
    int iconH = static_cast< int >( ( image.GetHeight() - 1 ) * scale );

    SetImage( image.Rescale( iconW, iconH, wxIMAGE_QUALITY_HIGH ) );
}
////////////////////////////////////////////////////////////////////////////////
ParallaxMappingUI::~ParallaxMappingUI()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
double ParallaxMappingUI::GetVersion()
{
    double result = 1.0;

    return result;
}
////////////////////////////////////////////////////////////////////////////////
PORT ParallaxMappingUI::GetIPorts()
{
    if( inputPort.size() == 0 )
    {
        wxPoint tempPort( GetIconImage()->GetWidth() * 10 / 52, 
            GetIconImage()->GetHeight() * 56 / 98 ); 
        AddPortToModel( tempPort, UIPLUGINBASE_ADD_INPUT_PORT );
    }
    
    return UIPluginBase::GetIPorts();
}
////////////////////////////////////////////////////////////////////////////////
PORT ParallaxMappingUI::GetOPorts()
{
    if( outputPort.size() == 0 )
    {
        wxPoint tempPort( GetIconImage()->GetWidth() * 43 / 52,
            GetIconImage()->GetHeight() * 56 / 98 );
        AddPortToModel( tempPort, UIPLUGINBASE_ADD_OUTPUT_PORT );
    }
    
    return UIPluginBase::GetOPorts();  
}
////////////////////////////////////////////////////////////////////////////////
ves::conductor::UIDialog* ParallaxMappingUI::UI( wxWindow* parent )
{
    if( dlg != NULL )
    {
        return dlg;
    }

    dlg = new ParallaxMappingUIDialog( parent, -1, serviceList );
    ConfigurePluginDialogs( dlg );

    return dlg;
}
////////////////////////////////////////////////////////////////////////////////
wxString ParallaxMappingUI::GetConductorName()
{         
    wxString result = wxT( "Parallax Mapping" );

    return result;
}
////////////////////////////////////////////////////////////////////////////////
wxString ParallaxMappingUI::GetName()
{
    if( mPluginName.IsEmpty() )
    {
        mPluginName = wxT( "PleaseDefineClassName" );
    }

    return mPluginName;
}
////////////////////////////////////////////////////////////////////////////////
