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
#include <ves/conductor/util/UI_TransientDialog.h>
#include <ves/xplorer/environment/cfdEnum.h>
#include <ves/conductor/util/spinctld.h>
#include <ves/conductor/xpm/transientIcons/play.xpm>
#include <ves/conductor/xpm/transientIcons/next.xpm>
#include <ves/conductor/xpm/transientIcons/prev.xpm>
#include <ves/conductor/xpm/transientIcons/stop.xpm>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

#include <wx/sizer.h>
#include <wx/window.h>
#include <wx/image.h>
#include <wx/bitmap.h>
#include <wx/bmpbuttn.h>
#include <wx/stattext.h>
#include <wx/statbox.h>
#include <wx/dialog.h>

#include <iostream>
#include <string>

BEGIN_EVENT_TABLE(UI_TransientDialog, wxDialog)
   EVT_BUTTON(PLAY_BUTTON, UI_TransientDialog::_onPlay)
   EVT_BUTTON(STOP_BUTTON, UI_TransientDialog::_onStop)
   EVT_BUTTON(FORWARD_STEP_BUTTON, UI_TransientDialog::_onForwardStep)
   EVT_BUTTON(BACKWARD_STEP_BUTTON, UI_TransientDialog::_onBackwardStep)
   EVT_SPINCTRL(CURRENT_FRAME, UI_TransientDialog::_onSelectFrame)
   EVT_SPINCTRL(DURATION_CNTL_BOX, UI_TransientDialog::_onSetDuration)
END_EVENT_TABLE()

using namespace ves::conductor::util;
///////////////////////////////////////////////////////
//Constructor                                        //
///////////////////////////////////////////////////////
UI_TransientDialog::UI_TransientDialog(int numTimeSteps,
                                       wxWindow* parent, 
                                       wxWindowID id, 
                                       std::string title)
:BaseDialog(parent,id,title)
{
   //_tab = 0;
   //_nTimeSteps = numTimeSteps;
   _nTimeSteps = 100;
   _commandPrefix = "TB_";

   _playImage = new wxImage(play_xpm);
   _forwardImage = new wxImage(next_xpm);
   _backwardImage = new wxImage(prev_xpm);
   _stopImage = new wxImage(stop_xpm);
   
   _playButton = new wxBitmapButton(this, PLAY_BUTTON,
		                           wxBitmap(*_playImage),
				                      wxDefaultPosition, 
				                      wxSize(35,40)
#ifdef WIN32
                                 ,wxBU_AUTODRAW
#endif
                                  );

   _stopButton = new wxBitmapButton(this, STOP_BUTTON,
		                           wxBitmap(*_stopImage),
				                      wxDefaultPosition, 
				                      wxSize(35,40)
#ifdef WIN32
                                 ,wxBU_AUTODRAW
#endif
                                 );
   _nextButton = new wxBitmapButton(this, FORWARD_STEP_BUTTON,
		                           wxBitmap(*_forwardImage),
				                      wxDefaultPosition, 
				                      wxSize(35,40)
#ifdef WIN32
                                 ,wxBU_AUTODRAW
#endif
                                 );
   _prevButton = new wxBitmapButton(this, BACKWARD_STEP_BUTTON,
		                           wxBitmap(*_backwardImage),
				                      wxDefaultPosition, 
				                      wxSize(35,40)
#ifdef WIN32
                                 ,wxBU_AUTODRAW
#endif
                                 );
   _currentFrame = new wxSpinCtrl(this, CURRENT_FRAME,
		                  wxEmptyString, wxDefaultPosition,
				             wxDefaultSize, 0, 0, _nTimeSteps, 0);
   _currentFrame->Enable(false);
   _duration = new wxSpinCtrlDbl( *this, DURATION_CNTL_BOX, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0, 
                                   0, 100, 0, 1.0, -1, wxEmptyString);
   _buildGUI();
}
///////////////////////////////////////
void UI_TransientDialog::_buildGUI()
{
   wxBoxSizer* mainSizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* spinSizer = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* controlSizer = new wxBoxSizer(wxHORIZONTAL);
   
   wxStaticBox* timeStep = new wxStaticBox(this,-1,
                                _("Current Timestep"));
   wxStaticBoxSizer* tStepSizer = new wxStaticBoxSizer(timeStep,wxHORIZONTAL);

   wxStaticBox* duration = new wxStaticBox(this,-1,
                                _("Duration (s)"));
   wxStaticBoxSizer* dSizer = new wxStaticBoxSizer(duration,wxHORIZONTAL);
   //wxBoxSizer* dSizer = new wxBoxSizer(wxHORIZONTAL);

   tStepSizer->Add(_currentFrame,1,wxALIGN_CENTER);
   dSizer->Add(_duration,1,wxALIGN_CENTER);
   spinSizer->Add(tStepSizer,1,wxALIGN_CENTER);
   spinSizer->Add(dSizer,1,wxALIGN_CENTER);

   controlSizer->Add(_prevButton,1,wxALIGN_CENTER|wxEXPAND);
   controlSizer->Add(_stopButton,1,wxALIGN_CENTER|wxEXPAND);
   controlSizer->Add(_playButton,1,wxALIGN_CENTER|wxEXPAND);
   controlSizer->Add(_nextButton,1,wxALIGN_CENTER|wxEXPAND);

   mainSizer->Add(controlSizer,0,wxALIGN_CENTER|wxEXPAND);
   mainSizer->Add(spinSizer,1,wxALIGN_CENTER|wxEXPAND);
   _duration->Raise();
   _currentFrame->Raise();
   //_addOKButton(mainSizer);

   SetSize(200,120);
   SetAutoLayout(true);
   SetSizer(mainSizer);
}
////////////////////////////////////////////////////////////////
void UI_TransientDialog::SetCommandPrefix(std::string newPrefix)
{
   _commandPrefix = newPrefix;
}
///////////////////////////////////////////////////////
void UI_TransientDialog::_onPlay(wxCommandEvent& event )
{
   ClearInstructions();
   _commandName = std::string(_commandPrefix + "TRANSIENT_MODE_UPDATE");
   
   VE_XML::DataValuePair* updateMode = new VE_XML::DataValuePair();
   updateMode->SetData("Mode","Play");
   _instructions.push_back(updateMode);
   _sendCommandsToXplorer();
   ClearInstructions();
}
///////////////////////////////////////////////////////////////
void UI_TransientDialog::_onForwardStep(wxCommandEvent& event )
{
   ClearInstructions();
   _commandName = std::string(_commandPrefix+"TRANSIENT_MODE_UPDATE");
   
   VE_XML::DataValuePair* updateMode = new VE_XML::DataValuePair();
   updateMode->SetData("Mode","Step");
   _instructions.push_back(updateMode);

   
   VE_XML::DataValuePair* updateDirection = new VE_XML::DataValuePair();
   updateDirection->SetData("Direction","Forward");
   _instructions.push_back(updateDirection);

   _sendCommandsToXplorer();
   ClearInstructions();
}
///////////////////////////////////////////////////////////////
void UI_TransientDialog::_onBackwardStep(wxCommandEvent& event )
{
   ClearInstructions();
   _commandName = std::string(_commandPrefix + "TRANSIENT_MODE_UPDATE");
   
   VE_XML::DataValuePair* updateMode = new VE_XML::DataValuePair();
   updateMode->SetData("Mode","Step");
   _instructions.push_back(updateMode);

   
   VE_XML::DataValuePair* updateDirection = new VE_XML::DataValuePair();
   updateDirection->SetData("Direction","Backward");
   _instructions.push_back(updateDirection);

   _sendCommandsToXplorer();
   ClearInstructions();
}
////////////////////////////////////////////////////////
void UI_TransientDialog::_onStop(wxCommandEvent& event )
{
   ClearInstructions();
   _commandName = std::string(_commandPrefix + "TRANSIENT_MODE_UPDATE");
   
   VE_XML::DataValuePair* updateMode = new VE_XML::DataValuePair();
   updateMode->SetData("Mode","Stop");
   _instructions.push_back(updateMode);
   _sendCommandsToXplorer();
   ClearInstructions();
}
/////////////////////////////////////////////////////////////////////
void UI_TransientDialog::_onSetDuration(wxSpinEvent& WXUNUSED(event))
{
   ClearInstructions();
   _commandName = std::string(_commandPrefix + "TRANSIENT_DURATION_UPDATE");
  
   VE_XML::DataValuePair* duration = new VE_XML::DataValuePair();
   duration->SetData("Duration", _duration->GetValue());
   _instructions.push_back(duration);

   _sendCommandsToXplorer();
   ClearInstructions();
}
/////////////////////////////////////////////////////////////////////
void UI_TransientDialog::_onSelectFrame(wxSpinEvent& WXUNUSED(event))
{
   
}

