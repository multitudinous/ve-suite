#include "VirtualLabUI.h"
#include "VirtualLabUIDialog.h"
#include <ves/conductor/ConductorLibEnums.h>

#include <wx/dc.h>

IMPLEMENT_DYNAMIC_CLASS( VirtualLabUI, ves::conductor::UIPluginBase )

/////////////////////////////////////////////////////////////////////////////
VirtualLabUI::VirtualLabUI()
{
    mPluginName = wxT("VirtualLab");
    
    RegistVar("mTextOne", &mTextOne);

    mDescription = _("VirtualLab");
}
///////////////////////////////////////////////////////////////////////////////
VirtualLabUI::~VirtualLabUI()
{

}
///////////////////////////////////////////////////////////////////////////////
double VirtualLabUI::GetVersion()
{
    double result=1.0;
    return result;
}
///////////////////////////////////////////////////////////////////////////////
PORT VirtualLabUI::GetIPorts()
{
    if( inputPort.size() == 0 )
    {
        wxPoint tempPort1(GetIconImage()->GetWidth()*10/52, 
            GetIconImage()->GetHeight()*26/98); 
        AddPortToModel( tempPort1, UIPLUGINBASE_ADD_INPUT_PORT );

        wxPoint tempPort2(GetIconImage()->GetWidth()*10/52, 
            GetIconImage()->GetHeight()*74/98); 
        AddPortToModel( tempPort2, UIPLUGINBASE_ADD_INPUT_PORT );
    }
    
    return UIPluginBase::GetIPorts();
}
///////////////////////////////////////////////////////////////////////////////
PORT VirtualLabUI::GetOPorts()
{
    if( outputPort.size() == 0 )
    {
        wxPoint tempPort1(GetIconImage()->GetWidth()*43/52,
            GetIconImage()->GetHeight()*26/98);
        AddPortToModel( tempPort1, UIPLUGINBASE_ADD_OUTPUT_PORT );

        wxPoint tempPort2(GetIconImage()->GetWidth()*43/52,
            GetIconImage()->GetHeight()*74/98);
        AddPortToModel( tempPort2, UIPLUGINBASE_ADD_OUTPUT_PORT );
    }
    
    return UIPluginBase::GetOPorts();
}
///////////////////////////////////////////////////////////////////////////////
ves::conductor::UIDialog* VirtualLabUI::UI(wxWindow* parent)
{
    if (dlg!=NULL)
    {
        return dlg;
    }
  
    dlg = new VirtualLabUIDialog(parent, -1, serviceList,
        &mTextOne);
      
    dlg->CenterOnScreen(wxBOTH);

    return dlg;
}
///////////////////////////////////////////////////////////////////////////////
wxString VirtualLabUI::GetName()
{
    wxString result(_("Virtual_Lab")); //your name
    return result;
}
///////////////////////////////////////////////////////////////////////////////
wxString VirtualLabUI::GetConductorName()
{
    wxString result(_("VirtualLabUI")); //your name
    return result;
}
