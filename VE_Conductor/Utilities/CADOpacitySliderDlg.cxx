#include "VE_Conductor/Utilities/CADOpacitySliderDlg.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/CAD/CADMaterial.h"
#include "VE_Open/XML/XMLReaderWriter.h"
#include "VE_Open/XML/Command.h"
#include <wx/slider.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/msgdlg.h>

using namespace VE_CAD;
using namespace VE_Conductor::GUI_Utilities;
BEGIN_EVENT_TABLE(CADOpacitySliderDlg,wxDialog)
   EVT_COMMAND_SCROLL(OPACITY_SLIDER,CADOpacitySliderDlg::_onSlider)
END_EVENT_TABLE()
//////////////////////////////////////////////////////////////////
//Constructor                                                   //
//////////////////////////////////////////////////////////////////
CADOpacitySliderDlg::CADOpacitySliderDlg(wxWindow* parent, int id,
                                         unsigned int cadNodeID,
                                         CADMaterial* material)
:wxDialog(parent,id,wxString("CADMaterial Opacity"),wxDefaultPosition,wxDefaultSize,
(wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX|wxCLOSE_BOX),wxString("CADMaterial Opacity"))
{
   _cadID = cadNodeID;
   _material = material;
   _buildDialog();
}
///////////////////////////////////////////
CADOpacitySliderDlg::~CADOpacitySliderDlg()
{
}
////////////////////////////////////////
void CADOpacitySliderDlg::_buildDialog()
{
   wxStaticBox* opacityGroup = new wxStaticBox(this, -1, wxT("Opacity"));
   wxStaticBoxSizer* mainSizer = new wxStaticBoxSizer(opacityGroup,wxHORIZONTAL);

   wxBoxSizer* sliderSizer = new wxBoxSizer(wxHORIZONTAL);

   _opacitySlider = new wxSlider(this, OPACITY_SLIDER,0 , 0, 100, wxDefaultPosition,wxDefaultSize,
                                 wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS); 
   SetSliderValue(_material->GetOpacity());
   sliderSizer->Add(_opacitySlider,1,wxALIGN_CENTER|wxEXPAND);

   mainSizer->Add(sliderSizer,1,wxALIGN_CENTER|wxEXPAND);
   SetAutoLayout(true);
   SetSizer(mainSizer);
}
//////////////////////////////////////////////////////
void CADOpacitySliderDlg::SetSliderValue(double value)
{
   //This should be a pecentage that comes in so we convert it
   //to an int 0-100
   _opacitySlider->SetValue(static_cast<int>(100 - 100*(1.0-value)));
}
///////////////////////////////////////////////////////////
void CADOpacitySliderDlg::SetVjObsPtr(VjObs_ptr xplorerCom)
{
   _vjObsPtr = VjObs::_duplicate(xplorerCom);
}
////////////////////////////////////////
double CADOpacitySliderDlg::GetOpacity()
{
   return (double)(_opacitySlider->GetValue())/100.0;
}
//////////////////////////////////////////////////////////
void CADOpacitySliderDlg::_onSlider(wxScrollEvent& WXUNUSED(event))
{
   //update the material
   //convert int to double
   float opacityValue = (float)(_opacitySlider->GetValue())/100.0;
   
   _material->SetOpacity(opacityValue);

   //build the command
   //_commandName = "CAD_ATTRIBUTE_MATERIAL_OPACITY_UPDATE";
    _commandName = std::string("CAD_ATTRIBUTE_MATERIAL_UPDATE");

    VE_XML::DataValuePair* nodeID = new VE_XML::DataValuePair();
    nodeID->SetDataType("UNSIGNED INT");
    nodeID->SetDataName(std::string("Node ID"));
    nodeID->SetDataValue(_cadID);
    _instructions.push_back(nodeID);

    VE_XML::DataValuePair* componentToUpdate = new VE_XML::DataValuePair();
    componentToUpdate->SetDataType("STRING");
    componentToUpdate->SetData("Material Component","Opacity");
    _instructions.push_back(componentToUpdate);

    VE_XML::DataValuePair* materialToUpdate = new VE_XML::DataValuePair();
    materialToUpdate->SetDataType("XMLOBJECT");
    materialToUpdate->SetData("Material",_material);
    _instructions.push_back(materialToUpdate);

    _sendCommandsToXplorer();
  
   /*VE_XML::DataValuePair* opacityData = new VE_XML::DataValuePair();
   opacityData->SetData(std::string("Opacity"),opacityValue);
   _instructions.push_back(opacityData);

   VE_XML::DataValuePair* materialName = new VE_XML::DataValuePair();
   materialName->SetDataType("STRING");
   materialName->SetDataName(std::string("Material Name"));
   materialName->SetDataString(_material->GetMaterialName());
   _instructions.push_back(materialName);

   VE_XML::DataValuePair* nodeID = new VE_XML::DataValuePair();
   nodeID->SetDataType("UNSIGNED INT");
   nodeID->SetDataValue(_cadID);
   nodeID->SetDataName(std::string("Node ID"));
   _instructions.push_back(nodeID);

   _sendCommandsToXplorer();*/
   _clearInstructions();
}
//////////////////////////////////////////////
void CADOpacitySliderDlg::_clearInstructions()
{
   _instructions.clear();
   _commandName.clear() ;
}
//////////////////////////////////////////////////
void CADOpacitySliderDlg::_sendCommandsToXplorer()
{
   VE_XML::Command* opacityCommand = new VE_XML::Command();

   for(size_t i =0; i < _instructions.size(); i++)
   {
      opacityCommand->AddDataValuePair(_instructions.at(i));
   }
   opacityCommand->SetCommandName(_commandName);
   std::string commandString("returnString");

   VE_XML::XMLReaderWriter opacityCommandWriter;
   opacityCommandWriter.UseStandaloneDOMDocumentManager();
   opacityCommandWriter.WriteToString();

   std::pair<VE_XML::Command*,std::string> nodeTagPair;
   nodeTagPair.first = opacityCommand;
   nodeTagPair.second = std::string("vecommand");
   std::vector< std::pair<VE_XML::XMLObject*,std::string> > nodeToWrite;
   nodeToWrite.push_back(nodeTagPair);

   opacityCommandWriter.WriteXMLDocument(nodeToWrite,commandString,"Command");


   if ( !CORBA::is_nil( _vjObsPtr ) && !commandString.empty() )
   {
      try
      {
         // CORBA releases the allocated memory so we do not have to
         _vjObsPtr->SetCommandString( CORBA::string_dup( commandString.c_str() ) );
      }
      catch ( ... )
      {
         wxMessageBox( "Send data to VE-Xplorer failed. Probably need to disconnect and reconnect.",
                        "Communication Failure", wxOK | wxICON_INFORMATION );
      }
   }
   //Clean up memory
   delete opacityCommand;
   _clearInstructions();
}

