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
#include <ves/conductor/util/ScalarToolsDlg.h>
//#include "VE_Conductor/Framework/Frame.h"

#include <wx/app.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/XMLReaderWriter.h>

#include <wx/statbox.h>
#include <wx/combobox.h>
#include <wx/checkbox.h>
#include <wx/button.h>
#include <wx/slider.h>
#include <wx/choicdlg.h>
#include <wx/filename.h>
using namespace ves::conductor::util;

BEGIN_EVENT_TABLE( ScalarToolsDialog, wxDialog )
   EVT_COMBOBOX(AVAILABLE_SCALARS,ScalarToolsDialog::_updateActiveScalar)
   EVT_COMBOBOX(AVAILABLE_SHADER_MANAGERS,ScalarToolsDialog::_updateActiveScalarShaderManager)
   EVT_BUTTON(ADVANCED_TB_ISOSURFACE,ScalarToolsDialog::_setColorByFace)
   EVT_COMMAND_SCROLL_THUMBTRACK (TB_ISOSURFACE_SLIDER,ScalarToolsDialog::_onUpdateIsosurface)
   EVT_COMMAND_SCROLL_THUMBRELEASE(TB_ISOSURFACE_SLIDER,ScalarToolsDialog::_onPreIntegrate)
   EVT_COMMAND_SCROLL(TB_SLICE_SLIDER,ScalarToolsDialog::_onUpdateNumberOfSlicePlanes)
   EVT_CHECKBOX(ISO_ENABLE_CHECK,ScalarToolsDialog::_onEnableIsoSurface)
END_EVENT_TABLE()
////////////////////////////////////////////////////////////////
ScalarToolsDialog::ScalarToolsDialog(wxWindow* parent, int id,std::string title)
:BaseDialog(parent,id,title)
{
   _scalarRange = 0;
   _scalarSelection = 0;
   _buildGUI();
   /*wxSize displaySize = ::wxGetDisplaySize();
   wxRect dialogPosition( displaySize.GetWidth()-427, 440, 427, displaySize.GetHeight()-480 );
   this->SetSize( dialogPosition );*/
   //SetSize(dynamic_cast<AppFrame*>(wxTheApp->GetTopWindow())->GetAppropriateSubDialogSize());
}
///////////////////////
///Destructor        //
///////////////////////
ScalarToolsDialog::~ScalarToolsDialog()
{
   if(_scalarRange)
   {
      _scalarRange->Destroy();
      _scalarRange = 0;
   }
}
///////////////////////////
void ScalarToolsDialog::_buildGUI()
{
   wxStaticBox* scalarToolsGroup = new wxStaticBox(this, -1, wxT("Scalar Tools"));
   wxStaticBoxSizer* mainSizer = new wxStaticBoxSizer(scalarToolsGroup,wxVERTICAL);
   
   _scalarSelection = new wxComboBox(this,AVAILABLE_SCALARS,
                                     _(""), wxDefaultPosition, 
                                     wxSize(150,wxDefaultCoord) );
   /*
   wxStaticBox* shaderManagerBox = new wxStaticBox(this, -1, _T("Transfer Functions"));
   wxStaticBoxSizer* smSizer = new wxStaticBoxSizer(shaderManagerBox,wxVERTICAL);

   _shaderManagerSelection = new wxComboBox(this,AVAILABLE_SHADER_MANAGERS,
                                     _(""), wxDefaultPosition, 
                                     wxSize(150,wxDefaultCoord) );
   _shaderManagerSelection->Append(_T("BLUE_RED_LINEAR_SHADER"));
      _shaderManagerSelection->Append(_T("GREY_SCALE_SHADER"));
   smSizer->Add(_shaderManagerSelection,0,wxALIGN_CENTER|wxEXPAND);*/

   wxStaticBox* numSlicesSliderBox = new wxStaticBox(this, -1, _T("Number of Slice Planes/Brick"));
   wxStaticBoxSizer* sliceSizer = new wxStaticBoxSizer(numSlicesSliderBox,wxVERTICAL);

   _numSlicesSlider = new wxSlider(this,TB_SLICE_SLIDER,100,32,1000,wxDefaultPosition,wxSize(300,-1),wxSL_HORIZONTAL|wxSL_LABELS);
   sliceSizer->Add(_numSlicesSlider,0,wxALIGN_CENTER|wxEXPAND);
   
   wxStaticBox* isoSliderBox = new wxStaticBox(this, -1, _T("Isosurface"));
   wxStaticBoxSizer* isoSizer = new wxStaticBoxSizer(isoSliderBox,wxVERTICAL);
   
   wxBoxSizer* enableIsoSizer = new wxBoxSizer(wxHORIZONTAL);
   _isosurfaceCheck = new wxCheckBox(this,ISO_ENABLE_CHECK,_T("Enable Isosurface"));
   enableIsoSizer->Add(_isosurfaceCheck,0,wxALIGN_CENTER);

   _advancedButton = new wxButton(this,ADVANCED_TB_ISOSURFACE,_T("Advanced..."));
   _advancedButton->Enable(false);
   enableIsoSizer->Add(_advancedButton,0,wxALIGN_CENTER);

   isoSizer->Add(enableIsoSizer,1,wxALIGN_CENTER|wxEXPAND);

   _isoSlider = new wxSlider(this, TB_ISOSURFACE_SLIDER, 0, 0, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
   _isoSlider->Enable(false);
   isoSizer->Add(_isoSlider,1,wxALIGN_CENTER|wxEXPAND);

  
   mainSizer->Add(_scalarSelection,0,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   //mainSizer->Add(smSizer,0,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   _createDualSliders();
   
   mainSizer->Add(sliceSizer,0,wxEXPAND|wxALIGN_CENTER);
   mainSizer->Add(isoSizer,1,wxEXPAND|wxALIGN_CENTER);
   wxBoxSizer* scalarRangeSizer = new wxBoxSizer(wxHORIZONTAL);
   scalarRangeSizer->Add(_scalarRange,1,wxALIGN_CENTER|wxEXPAND);

   mainSizer->Add(scalarRangeSizer,1,wxALIGN_CENTER|wxEXPAND);

   wxBoxSizer* buttonRowSizer = new wxBoxSizer(wxHORIZONTAL);
   _addOKButton(buttonRowSizer);
   
   mainSizer->Add(buttonRowSizer,0,wxALIGN_CENTER);
   //set this flag and let wx handle alignment
   SetAutoLayout(true);

   //assign the group to the panel
   SetSizer(mainSizer);
   mainSizer->Fit(this);
}
////////////////////////////////////////////
void ScalarToolsDialog::_createDualSliders()
{
   _scalarRange = new DualSlider(this,-1,1,0,100,0,100,wxDefaultPosition,wxDefaultSize,
                             wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS,_("Scalar Range"));

   _scalarRange->SetMinSliderCallback( new ScalarToolsSliderCallback(this));
   _scalarRange->SetMaxSliderCallback( new ScalarToolsSliderCallback(this));
   _scalarRange->SetBothSliderUpdateCallback( new ScalarToolsSliderCallback(this));
   _scalarRange->SetStopSliderUpdateCallback( new ScalarToolsStopSliderCallback(this));
}
///////////////////////////////////////////////////////////////////////////////////
void ScalarToolsDialog::ScalarToolsSliderCallback::SliderOperation()     
{
   _scalarDlg->ClearInstructions();
   _scalarDlg->SetCommandName("TB_SCALAR_RANGE");

   ves::open::xml::DataValuePair* minRangevalue = new ves::open::xml::DataValuePair();
   minRangevalue ->SetData("Mininum Scalar Range",static_cast<double>(_dualSlider->GetMinSliderValue())/100.0);
   _scalarDlg->AddInstruction(minRangevalue );

   ves::open::xml::DataValuePair* maxRangevalue  = new ves::open::xml::DataValuePair();
   maxRangevalue->SetData("Maximum Scalar Range",static_cast<double>(_dualSlider->GetMaxSliderValue())/100.0);
   _scalarDlg->AddInstruction(maxRangevalue );

   _scalarDlg->SendCommands();
   _scalarDlg->ClearInstructions();
}
/////////////////////////////////////////////////////////////////////////////////////////
void ScalarToolsDialog::ScalarToolsStopSliderCallback::SliderOperation()     
{
   _scalarDlg->ClearInstructions();
   _scalarDlg->SetCommandName("TB_FULL_PREINTEGRATE_UPDATE");

   ves::open::xml::DataValuePair* fullUpdate = new ves::open::xml::DataValuePair();
   unsigned int on = 1;
   fullUpdate ->SetData("Recalculate Pre-Integration",on);
   _scalarDlg->AddInstruction(fullUpdate );

   _scalarDlg->SendCommands();
   _scalarDlg->ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////////
void ScalarToolsDialog::UpdateScalarList(wxArrayString scalarNames)
{
   _scalarSelection->Clear();
   for(size_t i = 0; i < scalarNames.GetCount(); i++)
   {
      _scalarSelection->Append(scalarNames[i]);
   }
   if(scalarNames.GetCount())
   {
      _scalarSelection->SetValue(_activeScalar);
   }
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ScalarToolsDialog::_updateActiveScalar(wxCommandEvent& command)
{
   ClearInstructions();

   _commandName = "TB_ACTIVE_SOLUTION";
   _activeScalar = _scalarSelection->GetValue();
   ves::open::xml::DataValuePair* name = new ves::open::xml::DataValuePair();
   name->SetData("Active Dataset", ConvertUnicode( _activeScalar.GetData() ) );
   _instructions.push_back(name);

   ves::open::xml::DataValuePair* type = new ves::open::xml::DataValuePair();
   type->SetData("Data Type","Scalar");
   _instructions.push_back(type);

   ves::open::xml::DataValuePair* minRangevalue = new ves::open::xml::DataValuePair();
   minRangevalue->SetData("Mininum Scalar Range",static_cast<double>(_scalarRange->GetMinSliderValue())/100.0);
   _instructions.push_back(minRangevalue );

   ves::open::xml::DataValuePair* maxRangevalue  = new ves::open::xml::DataValuePair();
   maxRangevalue->SetData("Maximum Scalar Range",static_cast<double>(_scalarRange->GetMaxSliderValue())/100.0);
   _instructions.push_back(maxRangevalue );

   _sendCommandsToXplorer();
   ClearInstructions();
   wxScrollEvent event;
   _onPreIntegrate(event);
}
/////////////////////////////////////////////////////////////////////////////////
void ScalarToolsDialog::_updateActiveScalarShaderManager(wxCommandEvent& command)
{
   ClearInstructions();

   _commandName = "TB_SET_ACTIVE_SHADER_MANAGER";
  
   ves::open::xml::DataValuePair* name = new ves::open::xml::DataValuePair();
   name->SetData("Active Shader Manager", ConvertUnicode( _shaderManagerSelection->GetValue().GetData() ) );
   _instructions.push_back(name);

   _sendCommandsToXplorer();
   ClearInstructions();
   wxScrollEvent event;
   _onPreIntegrate(event);
}
////////////////////////////////////////////////////////////////
void ScalarToolsDialog::_setColorByFace(wxCommandEvent& command)
{
   int selectionIndex = 0;
   wxArrayString scalarNames;
   for(size_t i = 0; i < _scalarSelection->GetCount(); i++)
   {
      if(!_scalarSelection->GetString(i).Cmp(_colorByScalarName.c_str()) )
      {
         selectionIndex = i;
      }
      scalarNames.Add(_scalarSelection->GetString(i));
   }
   wxSingleChoiceDialog scalarSelector(this, _T("Select Scalar to color isosurface by."), _T("Color by Scalar"),
                                       scalarNames);
   scalarSelector.SetSize(GetRect());
   scalarSelector.SetSelection(selectionIndex);
   if (scalarSelector.ShowModal() == wxID_OK)
   {
      _colorByScalarName = scalarSelector.GetStringSelection();
   }
}
//////////////////////////////////////////////////////////////////////////////////
void ScalarToolsDialog::_onPreIntegrate(wxScrollEvent& command)
{
   ClearInstructions();
   _commandName = "TB_FULL_PREINTEGRATE_UPDATE";
   
   unsigned int on = 1;
   ves::open::xml::DataValuePair* fullUpdate = new ves::open::xml::DataValuePair();
   fullUpdate ->SetData("Recalculate Pre-Integration",on);
   AddInstruction(fullUpdate );

   _sendCommandsToXplorer();
   ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////////////
void ScalarToolsDialog::_onUpdateIsosurface(wxScrollEvent& command)
{
   ClearInstructions();
   _commandName = "TB_UPDATE_ISOSURFACE";
   
   ves::open::xml::DataValuePair* isosurfaceValue = new ves::open::xml::DataValuePair();
   isosurfaceValue->SetData("Iso-Surface Value",static_cast<double>((_isoSlider->GetValue()/100.0)));
   _instructions.push_back(isosurfaceValue);

   ves::open::xml::DataValuePair* colorByScalar = new ves::open::xml::DataValuePair();
   colorByScalar->SetData("Color By Scalar", ConvertUnicode( _colorByScalarName.GetData() ) );
   _instructions.push_back(colorByScalar);

   _sendCommandsToXplorer();
   ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////
void ScalarToolsDialog::_onUpdateNumberOfSlicePlanes(wxScrollEvent& command)
{
   ClearInstructions();
   _commandName = "TB_UPDATE_NUMBER_SLICE_PLANES";
   
   ves::open::xml::DataValuePair* nPlanesValue = new ves::open::xml::DataValuePair();
   nPlanesValue->SetData("Number of Slice Planes",static_cast<unsigned int>((_numSlicesSlider->GetValue())));
   _instructions.push_back(nPlanesValue);

   _sendCommandsToXplorer();
   ClearInstructions();
}
////////////////////////////////////////////////////////////////////
void ScalarToolsDialog::_onEnableIsoSurface(wxCommandEvent& command)
{
   _isoSlider->Enable(_isosurfaceCheck->GetValue());
   //this isn't ready yet
   //_advancedButton->Enable(_isosurfaceCheck->GetValue());

   ClearInstructions();
   _commandName = "TB_ISOSURFACE_ENABLE";
   
   ves::open::xml::DataValuePair* isosurfaceValue = new ves::open::xml::DataValuePair();
   isosurfaceValue->SetData("Iso-Surface State",(_isosurfaceCheck->GetValue())?"On":"Off");
   _instructions.push_back(isosurfaceValue);

   _sendCommandsToXplorer();
   ClearInstructions();
}
///////////////////////////////////////////////////////////
void ScalarToolsDialog::SetCommandName(std::string newName)
{
   _commandName = newName;
}
//////////////////////////////////////////////////////////////////////////
void ScalarToolsDialog::AddInstruction(ves::open::xml::DataValuePair* newInstruct)
{
   _instructions.push_back(newInstruct);
}
//////////////////////////////////////
void ScalarToolsDialog::SendCommands()
{
   _sendCommandsToXplorer();
}
