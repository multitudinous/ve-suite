#include "VE_Conductor/DefaultPlugin/DefaultPlugin.h"
#include "VE_Conductor/DefaultPlugin/DefaultPlugin_UI_Dialog.h"
#include "VE_Installer/installer/installerImages/icons.xpm"
#include "VE_Open/XML/Model/Point.h"
#include <iostream>

#include <wx/dc.h>
IMPLEMENT_DYNAMIC_CLASS(DefaultPlugin, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
DefaultPlugin
::DefaultPlugin()
{
   wxImage my_img( icons_xpm );
   icon_w = (int)my_img.GetWidth()*1.20;
   icon_h = (int)my_img.GetHeight()*1.20;
   my_icon=new wxBitmap(my_img.Scale(icon_w, icon_h));

   n_pts = 4;

   poly[0]=wxPoint(0,0);
   poly[1]=wxPoint(icon_w,0);
   poly[2]=wxPoint(icon_w,icon_h);
   poly[3]=wxPoint(0,icon_h);

//#define TESTPORT
#ifdef TESTPORT
   VE_Model::Port* Aport = new VE_Model::Port();
   VE_Model::Point * Aportloc = new VE_Model::Point();
   Aportloc->SetPoint(std::pair<unsigned int, unsigned int> (0, icon_h/3));
   Aport->SetDataFlowDirection("INPUT");
   Aport->SetModelName("DefaultPlugin");
   Aport->SetPortNumber(0);
   Aport->SetPortType("MPMICEGRID");
   Aport->SetPortLocation(Aportloc);
   inputPort.push_back(Aport);

   VE_Model::Port* Bport = new VE_Model::Port();
   VE_Model::Point * Bportloc = new VE_Model::Point();
   Bportloc->SetPoint(std::pair<unsigned int, unsigned int> (0, icon_h*2/3));
   Bport->SetDataFlowDirection("INPUT");
   Bport->SetModelName("DefaultPlugin");
   Bport->SetPortNumber(1);
   Bport->SetPortType("TOMCAT");
   Bport->SetPortLocation(Bportloc);
   inputPort.push_back(Bport);

   VE_Model::Port* Cport = new VE_Model::Port();
   VE_Model::Point * Cportloc = new VE_Model::Point();
   Cportloc->SetPoint(std::pair<unsigned int, unsigned int> (icon_w, icon_h/3));
   Cport->SetDataFlowDirection("OUTPUT");
   Cport->SetModelName("DefaultPlugin");
   Cport->SetPortNumber(0);
   Cport->SetPortType("MPMICEGRID");
   Cport->SetPortLocation(Cportloc);
   outputPort.push_back(Cport);

   VE_Model::Port* Dport = new VE_Model::Port();
   VE_Model::Point * Dportloc = new VE_Model::Point();
   Dportloc->SetPoint(std::pair<unsigned int, unsigned int> (icon_w, icon_h*2/3));
   Dport->SetDataFlowDirection("OUTUT");
   Dport->SetModelName("DefaultPlugin");
   Dport->SetPortNumber(1);
   Dport->SetPortType("TOMCAT");
   Dport->SetPortLocation(Dportloc);
   outputPort.push_back(Dport);
#endif //TESTPORT
}
/////////////////////////////////////////////////////////////////////////////
DefaultPlugin
::~DefaultPlugin()
{
   delete my_icon;
}

/////////////////////////////////////////////////////////////////////////////
double DefaultPlugin::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}
/////////////////////////////////////////////////////////////////////////////
void DefaultPlugin::DrawIcon(wxDC* dc)
{
  //Your implementation
	dc->DrawBitmap(*my_icon,pos.x, pos.y);
}
/////////////////////////////////////////////////////////////////////////////
UIDialog* DefaultPlugin::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new DefaultPlugin_UI_Dialog(parent, -1);
      
  return dlg;
}
/////////////////////////////////////////////////////////////////////////////
wxString DefaultPlugin::GetName()
{
  wxString result="DefaultPlugin"; //your name
  return result;
}
/////////////////////////////////////////////////////////////////////////////
wxString DefaultPlugin::GetDesc()
{
  wxString result="DefaultPlugin for VE-Conductor."; //your description

  return result;
}
