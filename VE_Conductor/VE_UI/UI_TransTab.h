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
#ifndef _VE_UI_TRANSIENT_TAB_H_
#define _VE_UI_TRANSIENT_TAB_H_
#ifdef WIN32
#include <winsock2.h>
#endif
#include <wx/wx.h>

//Transient tab control ids
enum TRANS_TAB_IDS
{
   TRANS_CATEGORY_RAD_BOX,
   TRANS_DIRECTION_RAD_BOX,
//include Time progress bar!!!!!!!!!!!!
   TIMEPROGRESS_GAUGE,  //check!!!!!!!!!!
   RESET_BUTTON,
   BACKWARD_BUTTON,
   START_BUTTON,
   FORWARD_BUTTON,
   PAUSE_BUTTON
};


class UI_TransTab : public wxPanel {
public:

   UI_TransTab(wxNotebook* tControl);
   ~UI_TransTab(){};

protected:
   wxNotebook* _parent;
   //the controls
   wxRadioBox* _categoryRBox;
   wxRadioBox* _directionRBox;
   //include time progress (wxStatusBar or wxGauge)
   wxGauge* _timeProgressGauge;  
   wxButton* _resetButton;
   wxButton* _backwardButton;
   wxButton* _startButton;
   wxButton* _forwardButton;
   wxButton* _pauseButton;


   //create this page
   void _buildPage();
   void createCommandId();
   //vispage control event callbacks
   void _onCategory(wxCommandEvent& event);
   void _onDirection(wxCommandEvent& event);   
   void _onTimeProgress(wxCommandEvent& event);//include time progress bar
   void _onReset(wxCommandEvent& event); 
   void _onBackward(wxCommandEvent& event);
   void _onStart(wxCommandEvent& event);
   void _onForward(wxCommandEvent& event);
   void _onPause(wxCommandEvent& event);
  DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_TRANSIENT_TAB_H_
