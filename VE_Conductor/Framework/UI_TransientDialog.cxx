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
 * File:          $RCSfile: UI_TransientDialog.cxx UI_TransientDialog.cxxUI_TransientDialog.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
//#include "VE_Conductor/VE_UI/UI_Tabs.h"
#include "VE_Conductor/Framework/UI_TransientDialog.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnum.h"
#include "VE_Builder/Utilities/gui/spinctld.h"
#include "VE_Conductor/xpm/transientIcons/play.xpm"
#include "VE_Conductor/xpm/transientIcons/next.xpm"
#include "VE_Conductor/xpm/transientIcons/prev.xpm"
#include "VE_Conductor/xpm/transientIcons/stop.xpm"

#include <wx/sizer.h>
#include <wx/window.h>

#include <iostream>

BEGIN_EVENT_TABLE(UI_TransientDialog, wxDialog)
   EVT_BUTTON(PLAY_BUTTON, UI_TransientDialog::_onPlay)
   EVT_BUTTON(STOP_BUTTON, UI_TransientDialog::_onStop)
   EVT_BUTTON(FORWARD_STEP_BUTTON, UI_TransientDialog::_onForwardStep)
   EVT_BUTTON(BACKWARD_STEP_BUTTON, UI_TransientDialog::_onBackwardStep)
   EVT_SPINCTRL(CURRENT_FRAME, UI_TransientDialog::_onSelectFrame)
   EVT_SPINCTRL(DURATION_CNTL_BOX, UI_TransientDialog::_onSetDuration)
END_EVENT_TABLE()
///////////////////////////////////////////////////////
//Constructor                                        //
///////////////////////////////////////////////////////
UI_TransientDialog::UI_TransientDialog(int numTimeSteps,
                                       wxWindow* parent, 
                                       wxWindowID id, 
                                       const wxString& title, 
                                       const wxPoint& pos ,
                                       const wxSize& size,
                                       long style, 
                                       const wxString& name)
:wxDialog(parent,id,title,pos,size,style,name)                                    
{
   //_tab = 0;
   //_nTimeSteps = numTimeSteps;
   _nTimeSteps = 19;

   _playImage = new wxImage(play_xpm);
   _forwardImage = new wxImage(next_xpm);
   _backwardImage = new wxImage(prev_xpm);
   _stopImage = new wxImage(stop_xpm);
   
   _playButton = new wxBitmapButton(this, PLAY_BUTTON,
		                           wxBitmap(_playImage),
				                      wxDefaultPosition, 
				                      wxSize(35,40)
#ifdef WIN32
                                 ,wxBU_AUTODRAW
#endif
                                  );

   _stopButton = new wxBitmapButton(this, STOP_BUTTON,
		                           wxBitmap(_stopImage),
				                      wxDefaultPosition, 
				                      wxSize(35,40)
#ifdef WIN32
                                 ,wxBU_AUTODRAW
#endif
                                 );
   _nextButton = new wxBitmapButton(this, FORWARD_STEP_BUTTON,
		                           wxBitmap(_forwardImage),
				                      wxDefaultPosition, 
				                      wxSize(35,40)
#ifdef WIN32
                                 ,wxBU_AUTODRAW
#endif
                                 );
   _prevButton = new wxBitmapButton(this, BACKWARD_STEP_BUTTON,
		                           wxBitmap(_backwardImage),
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
   _buildDialog();
}
///////////////////////////////////////
void UI_TransientDialog::_buildDialog()
{
   wxBoxSizer* mainSizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* spinSizer = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* controlSizer = new wxBoxSizer(wxHORIZONTAL);
   
   wxStaticBox* timeStep = new wxStaticBox(this,-1,
                                wxString("Current Timestep"));
   wxStaticBoxSizer* tStepSizer = new wxStaticBoxSizer(timeStep,wxHORIZONTAL);

   wxStaticBox* duration = new wxStaticBox(this,-1,
                                wxString("Duration (s)"));
   wxStaticBoxSizer* dSizer = new wxStaticBoxSizer(duration,wxHORIZONTAL);

   tStepSizer->Add(_currentFrame,1,wxALIGN_CENTER);
   dSizer->Add(_duration,1,wxALIGN_CENTER);
   spinSizer->Add(tStepSizer,1,wxALIGN_CENTER);
   spinSizer->Add(dSizer,1,wxALIGN_CENTER);

   controlSizer->Add(_prevButton,1,wxALIGN_CENTER|wxEXPAND);
   controlSizer->Add(_stopButton,1,wxALIGN_CENTER|wxEXPAND);
   controlSizer->Add(_playButton,1,wxALIGN_CENTER|wxEXPAND);
   controlSizer->Add(_nextButton,1,wxALIGN_CENTER|wxEXPAND);

   mainSizer->Add(controlSizer,1,wxALIGN_CENTER|wxEXPAND);
   mainSizer->Add(spinSizer,2,wxALIGN_CENTER|wxEXPAND);

   SetAutoLayout(true);
   SetSizer(mainSizer);
}
////////////////////////////////////////////////////
/*void UI_TransientDialog::SetTabControl(UI_Tabs* tab)
{
   //_tab = tab;
}*/
///////////////////////////////////////////////////////
void UI_TransientDialog::_onPlay(wxCommandEvent& event )
{
   /*if(!_tab)
      return;
   if(event.GetId() == PLAY_BUTTON){
      _currentFrame->Enable(false);
      _tab->cId = TRANSIENT_PLAY;
      //_tab->sendDataArrayToServer();
   }*/
}
///////////////////////////////////////////////////////////////
void UI_TransientDialog::_onForwardStep(wxCommandEvent& event )
{
   /*if(!_tab)
      return;
   if(event.GetId() == FORWARD_STEP_BUTTON){
      _currentFrame->Enable(false);
      _tab->cId = TRANSIENT_FORWARD;
      //_tab->sendDataArrayToServer();
   }*/
}
///////////////////////////////////////////////////////////////
void UI_TransientDialog::_onBackwardStep(wxCommandEvent& event )
{
   /*if(!_tab)
      return;
   if(event.GetId() == BACKWARD_STEP_BUTTON){
      _currentFrame->Enable(false);
      _tab->cId = TRANSIENT_BACKWARD;
      //_tab->sendDataArrayToServer();
   }*/
}
////////////////////////////////////////////////////////
void UI_TransientDialog::_onStop(wxCommandEvent& event )
{
   /*if(!_tab)
      return;
   if(event.GetId() == STOP_BUTTON){
      //_currentFrame->Enable(true);
      _tab->cId = TRANSIENT_STOP;
     // _tab->sendDataArrayToServer();
   }*/
}
/////////////////////////////////////////////////////////////////////
void UI_TransientDialog::_onSetDuration(wxSpinEvent& WXUNUSED(event))
{
   /*if(!_tab)
      return;
   _tab->cId = TRANSIENT_DURATION;
   _tab->cIso_value = static_cast< int >( _duration->GetValue() );
   //_tab->sendDataArrayToServer();*/
}
/////////////////////////////////////////////////////////////////////
void UI_TransientDialog::_onSelectFrame(wxSpinEvent& WXUNUSED(event))
{
   /*
   if(!_tab)
      return;
   _tab->cId = TRANSIENT_SET_FRAME;
   _tab->cIso_value = _currentFrame->GetValue();
   //_tab->sendDataArrayToServer();*/
}

