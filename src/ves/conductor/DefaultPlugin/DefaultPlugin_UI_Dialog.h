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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef DEFAULT_PLUGIN_UI_DIALOG_H
#define DEFAULT_PLUGIN_UI_DIALOG_H
#include <ves/conductor/UIDialog.h>
#include "ves/VEConfig.h" 
#include <vector>
#include <string>

class wxRadioBox;
class wxSlider;
class wxTextCtrl;
class wxButton;

class DefaultPlugin_UI_Dialog : public UIDialog
{
public:
  DefaultPlugin_UI_Dialog(wxWindow* parent, int id, long int* height );
  DefaultPlugin_UI_Dialog(){;}
  
  virtual ~DefaultPlugin_UI_Dialog();
  
   enum Hummer_TAB_IDS
   {
      DIRECTION_RBOX,
      ERROR_RBOX,
      HEIGHT_SLIDER,
      WIDTH_SLIDER,
      X_LOCATION_SLIDER,
      Y_LOCATION_SLIDER,
      Z_LOCATION_SLIDER,
      UPDATE_BUTTON,
      CLEAR_BUTTON,
      EXIT_BUTTON
   };

  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:

  //UI widgets variables

   wxRadioBox* _directionRBox;
   wxRadioBox* _errorRBox;
   //the controls
   wxSlider* _heightSlider;
   wxSlider* _widthSlider;
   wxSlider* _xLocSlider;
   wxSlider* _yLocSlider;
   wxSlider* _zLocSlider; 

   wxTextCtrl* _xLoc;
   wxTextCtrl* _yLoc;
   wxTextCtrl* _zLoc;

   wxButton* _sliderUpdate;
   wxButton* _clearButton;
   wxButton* _exitButton;

   long int* height;
 public:
  //GUI Variables
  void _buildPage();
  void _onDirection(wxCommandEvent& event);
  void _onError(wxCommandEvent& event);
  void _onSliderUpdate(wxCommandEvent& event);   
  void _onClear(wxCommandEvent& event);
  void _onExit(wxCommandEvent& event);
  void SliderUpdate(wxScrollEvent& event);

  DECLARE_EVENT_TABLE()
};

#endif//DEFAULT_PLUGIN_UI_DIALOG_H

