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
 * File:          $RCSfile: UI_TransientDialog.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _VE_UI_TRANSIENT_DIALOG_H_
#define _VE_UI_TRANSIENT_DIALOG_H_
#include <wx/spinctrl.h>
#include <wx/image.h>
#include <wx/bmpbuttn.h>
#include <wx/bmpbuttn.h>
#include <wx/stattext.h>
#include <wx/statbox.h>
#include <wx/dialog.h>

class wxSizer;
class wxWindow;

class UI_Tabs;
class wxSpinCtrlDbl;
//Transient control ids
enum TRANS_DIALOG_IDS
{
   PLAY_BUTTON,
   STOP_BUTTON,
   FORWARD_STEP_BUTTON,
   BACKWARD_STEP_BUTTON,
   DURATION_CNTL_BOX,
   CURRENT_FRAME
};

class UI_TransientDialog : public wxDialog {
public:
   UI_TransientDialog(int numTimeSteps,
                    wxWindow* parent, 
		               wxWindowID id = -1, 
                    const wxString& title = "Transient Controls", 
                    const wxPoint& pos = wxDefaultPosition,
                    const wxSize& size = wxSize(200,100),
                    long style = wxDEFAULT_DIALOG_STYLE, 
                    const wxString& name = "Transient Controls");
   ~UI_TransientDialog(){};

   void SetTabControl(UI_Tabs* tab);
protected:
   void _buildDialog();

   unsigned int _nTimeSteps;

   UI_Tabs* _tab;
   wxImage* _playImage;
   wxImage* _stopImage;
   wxImage* _forwardImage;
   wxImage* _backwardImage;

   wxBitmapButton* _playButton;
   wxBitmapButton* _stopButton;

   wxBitmapButton* _nextButton;
   wxBitmapButton* _prevButton;

   wxSpinCtrl* _currentFrame;
   wxSpinCtrlDbl* _duration;
   void _onBackwardStep(wxCommandEvent& event);
   void _onForwardStep(wxCommandEvent& event);
   void _onPlay(wxCommandEvent& event);
   void _onStop(wxCommandEvent& event);
   void _onSelectFrame(wxSpinEvent& event);
   void _onSetDuration(wxSpinEvent& event);
  DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_TRANSIENT_DIALOG_H_

