/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: SceneGraphBuilder.cxx,v $
 * Date modified: $Date: 2006-01-10 13:45:28 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3477 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Conductor/Framework/ROIDialog.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/XMLReaderWriter.h"

using namespace VE_Conductor::GUI_Utilities;
////////////////////////////////////////////////////////////////
ROIDialog::ROIDialog(wxWindow* parent, int id,std::string title)
:BaseDialog(parent,id,title)
{
   _xBounds = 0;
   _yBounds = 0; 
   _zBounds = 0;
   _buildGUI();
}
///////////////////////
///Destructor        //
///////////////////////
ROIDialog::~ROIDialog()
{
   /*if(_xBounds)
   {
      _xBounds->Destroy();
      _xBounds = 0;
   }
   if(_yBounds)
   {
      _yBounds->Destroy();
      _yBounds = 0;
   }
   if(_zBounds)
   {
      _zBounds->Destroy();
      _zBounds = 0;
   }*/
}
///////////////////////////
void ROIDialog::_buildGUI()
{
   wxBoxSizer* mainSizer = new wxBoxSizer(wxVERTICAL);
 
   _createDualSliders();
   wxBoxSizer* xdualSizer = new wxBoxSizer(wxHORIZONTAL);
   xdualSizer->Add(_xBounds,1,wxALIGN_CENTER|wxEXPAND);
   

   wxBoxSizer* ydualSizer = new wxBoxSizer(wxHORIZONTAL);
   ydualSizer->Add(_yBounds,1,wxALIGN_CENTER|wxEXPAND);

   wxBoxSizer* zdualSizer = new wxBoxSizer(wxHORIZONTAL);
   zdualSizer->Add(_zBounds,1,wxALIGN_CENTER|wxEXPAND);

   mainSizer->Add(xdualSizer,1,wxALIGN_CENTER|wxEXPAND);
   mainSizer->Add(ydualSizer,1,wxALIGN_CENTER|wxEXPAND);
   mainSizer->Add(zdualSizer,1,wxALIGN_CENTER|wxEXPAND);

   //set this flag and let wx handle alignment
   SetAutoLayout(true);

   //assign the group to the panel
   SetSizer(mainSizer);
   mainSizer->Fit(this);
}
////////////////////////////////////
void ROIDialog::_createDualSliders()
{
   _xBounds = new DualSlider(this,-1,1,0, 100,0,0, wxDefaultPosition,wxDefaultSize,
                             wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS,wxString("X Bounds"));
   ROIMinSliderCallback* minX = new ROIMinSliderCallback(this,"X");
   ROIMaxSliderCallback* maxX = new ROIMaxSliderCallback(this,"X");
   ROIBothMoveCallback* bothX = new ROIBothMoveCallback(this,"X");

   _xBounds->SetMinSliderCallback(minX);
   _xBounds->SetMaxSliderCallback(maxX);
   _xBounds->SetBothSliderUpdateCallback(bothX);

   _yBounds = new DualSlider(this,-1,1,0, 100,0,0, wxDefaultPosition,wxDefaultSize,
                             wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS,wxString("Y Bounds"));

   ROIMinSliderCallback* minY = new ROIMinSliderCallback(this,"Y");
   ROIMaxSliderCallback* maxY = new ROIMaxSliderCallback(this,"Y");
   ROIBothMoveCallback* bothY = new ROIBothMoveCallback(this,"Y");

   _yBounds->SetMinSliderCallback(minY);
   _yBounds->SetMaxSliderCallback(maxY);
   _yBounds->SetBothSliderUpdateCallback(bothY);

   _zBounds = new DualSlider(this,-1,1,0, 100,0,0, wxDefaultPosition,wxDefaultSize,
                             wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS,wxString("Z Bounds"));

   ROIMinSliderCallback* minZ = new ROIMinSliderCallback(this,"Z");
   ROIMaxSliderCallback* maxZ = new ROIMaxSliderCallback(this,"Z");
   ROIBothMoveCallback* bothZ = new ROIBothMoveCallback(this,"Z");

   _zBounds->SetMinSliderCallback(minZ);
   _zBounds->SetMaxSliderCallback(maxZ);
   _zBounds->SetBothSliderUpdateCallback(bothZ);
}
///////////////////////////////////////////////////////
void ROIDialog::ROIMinSliderCallback::SliderOperation()     
{
   _roidlg->SetCommandName("TB_ROI_MIN_UPDATE");

   VE_XML::DataValuePair* coordinate = new VE_XML::DataValuePair();
   coordinate->SetDataType("STRING");
   coordinate->SetDataName(std::string("Coordinate"));
   coordinate->SetDataString(_direction);
   _roidlg->AddInstruction(coordinate);

   VE_XML::DataValuePair* direction = new VE_XML::DataValuePair();
   direction->SetDataType("STRING");
   direction->SetDataName(std::string("Direction"));
   direction->SetDataString("Positive");
   _roidlg->AddInstruction(direction);

   VE_XML::DataValuePair* value = new VE_XML::DataValuePair();
   value->SetData("ROI Value",static_cast<double>(_dualSlider->GetMinSliderValue())/100.0);
   _roidlg->AddInstruction(value);

   _roidlg->SendCommands();
   _roidlg->ClearInstructions();
}
///////////////////////////////////////////////////////
void ROIDialog::ROIMaxSliderCallback::SliderOperation()     
{
   _roidlg->SetCommandName("TB_ROI_MAX_UPDATE");
   VE_XML::DataValuePair* coordinate = new VE_XML::DataValuePair();
   coordinate->SetDataType("STRING");
   coordinate->SetDataName(std::string("Coordinate"));
   coordinate->SetDataString(_direction);
   _roidlg->AddInstruction(coordinate);

   VE_XML::DataValuePair* direction = new VE_XML::DataValuePair();
   direction->SetDataType("STRING");
   direction->SetDataName(std::string("Direction"));
   direction->SetDataString("Negative");
   _roidlg->AddInstruction(direction);

   VE_XML::DataValuePair* value = new VE_XML::DataValuePair();
   value->SetData("ROI Value",static_cast<double>(_dualSlider->GetMaxSliderValue())/100.0);
   _roidlg->AddInstruction(value);

   _roidlg->SendCommands();
   _roidlg->ClearInstructions();
}
//////////////////////////////////////////////////////
void ROIDialog::ROIBothMoveCallback::SliderOperation()     
{
   _roidlg->SetCommandName("TB_ROI_BOTH_UPDATE");

   VE_XML::DataValuePair* coordinate = new VE_XML::DataValuePair();
   coordinate->SetDataType("STRING");
   coordinate->SetDataName(std::string("Coordinate"));
   coordinate->SetDataString(_direction);
   _roidlg->AddInstruction(coordinate);

   VE_XML::DataValuePair* minvalue = new VE_XML::DataValuePair();
   minvalue->SetData("ROI Min Value",static_cast<double>(_dualSlider->GetMinSliderValue())/100.0);
   _roidlg->AddInstruction(minvalue);

   VE_XML::DataValuePair* maxvalue = new VE_XML::DataValuePair();
   maxvalue->SetData("ROI Max Value",static_cast<double>(_dualSlider->GetMaxSliderValue())/100.0);
   _roidlg->AddInstruction(maxvalue);

   _roidlg->SendCommands();
   _roidlg->ClearInstructions();
}
////////////////////////////////////////////////////
void ROIDialog::SetCommandName(std::string newName)
{
   _commandName = newName;
}
///////////////////////////////////////////////////////////////////
void ROIDialog::AddInstruction(VE_XML::DataValuePair* newInstruct)
{
   _instructions.push_back(newInstruct);
}
//////////////////////////////
void ROIDialog::SendCommands()
{
   _sendCommandsToXplorer();
}

