/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "ves/conductor/util/CADOpacitySliderDlg.h"
#include "ves/conductor/util/CORBAServiceList.h"
#include "ves/open/xml/DataValuePair.h"
#include "ves/open/xml/cad/CADMaterial.h"
#include "ves/open/xml/XMLReaderWriter.h"
#include "ves/open/xml/Command.h"
#include <wx/slider.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/msgdlg.h>

using namespace VE_XML::VE_CAD;
using namespace VE_Conductor::GUI_Utilities;
BEGIN_EVENT_TABLE(CADOpacitySliderDlg,wxDialog)
   EVT_COMMAND_SCROLL(OPACITY_SLIDER,CADOpacitySliderDlg::_onSlider)
END_EVENT_TABLE()
//////////////////////////////////////////////////////////////////
//Constructor                                                   //
//////////////////////////////////////////////////////////////////
CADOpacitySliderDlg::CADOpacitySliderDlg(wxWindow* parent, int id,
                                         std::string cadNodeID,
                                         CADMaterial* material)
:wxDialog(parent,id,_("CADMaterial Opacity"),wxDefaultPosition,wxDefaultSize,
(wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX|wxCLOSE_BOX),_("CADMaterial Opacity"))
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
    nodeID->SetDataType("STRING");
    nodeID->SetData(std::string("Node ID"),_cadID);
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
   
   {
      try
      {
         VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer(opacityCommand);
      }
      catch ( ... )
      {
         wxMessageBox( _("Send data to VE-Xplorer failed. Probably need to disconnect and reconnect."),
                        _("Communication Failure"), wxOK | wxICON_INFORMATION );
      }
   }
   //Clean up memory
   delete opacityCommand;
   _clearInstructions();
}

