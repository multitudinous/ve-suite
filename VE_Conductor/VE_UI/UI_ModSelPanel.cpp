/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/VE_UI/UI_ModSelPanel.h"
#include "VE_Conductor/VE_UI/UI_Frame.h"
#include "VE_Conductor/VE_UI/UI_ModelData.h"
#include <iostream>

#include <wx/radiobox.h>
#include <wx/string.h>



BEGIN_EVENT_TABLE(UI_ModSelScroll, wxScrolledWindow)
END_EVENT_TABLE()
//////////////////////////////////////////////////
//Constructor                                   //
//////////////////////////////////////////////////
UI_ModSelScroll::UI_ModSelScroll(wxWindow* parent)
:wxScrolledWindow(parent, -1, wxDefaultPosition, wxDefaultSize,
		    wxHSCROLL | wxVSCROLL)
{
   int nUnitX=10;
   int nUnitY=10;
   int nPixX = 10;
   int nPixY = 10;
   SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );

   int _numModels = ((UI_ModSelPanel *)GetParent())->_modelData->GetNumberOfModels();

   _models = new wxString[_numModels]; 

   std::cout << " Number of Models in System : " << _numModels << std::endl;

   for ( int i=0; i<_numModels; i++)
      _models[i] = wxT(((UI_ModSelPanel *)GetParent())->_modelData->GetModelName(i));
   

   _modelSelBox = new wxRadioBox(this, RBOX_MODEL_SELECT, wxT("VE Models"), 
                                 wxDefaultPosition, wxDefaultSize, _numModels, 
                                 _models, 1, wxRA_SPECIFY_COLS);
   wxBoxSizer* _col = new wxBoxSizer(wxVERTICAL);
   _col->Add(_modelSelBox,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   SetSizer(_col);
}

UI_ModSelScroll::~UI_ModSelScroll()
{
   delete _modelSelBox;
   delete [] _models;
}


BEGIN_EVENT_TABLE(UI_ModSelPanel, wxPanel)
   EVT_RADIOBOX(RBOX_MODEL_SELECT, UI_ModSelPanel::_onModSelect)
END_EVENT_TABLE()
//////////////////////////////////////////////////
//Constructor                                   //
//////////////////////////////////////////////////
UI_ModSelPanel::UI_ModSelPanel(wxWindow* parent, UI_ModelData* _model)
:wxPanel(parent)
{
   _modelData = _model;

   _modselScroll = new UI_ModSelScroll(this);

   wxBoxSizer* _col = new wxBoxSizer(wxVERTICAL);
   _col->Add(_modselScroll,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   SetAutoLayout(true);
   SetSizer(_col);
}

UI_ModSelPanel::~UI_ModSelPanel()
{
   delete _modselScroll;
}

void UI_ModSelPanel::_onModSelect(wxCommandEvent& WXUNUSED(event))
{
   ((UI_Frame *)GetParent())->activeModIndex = _modselScroll->_modelSelBox->GetSelection();
   ((UI_Frame *)GetParent())->OnChangeModel();
}

