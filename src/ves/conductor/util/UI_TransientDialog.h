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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef UI_TRANSIENT_DIALOG_H_
#define UI_TRANSIENT_DIALOG_H_
/*!\file UI_TransientDialog.h
*UI_TransientDialog API
*/
/*!\class UI_TransientDialog
* 
*/
#include <ves/VEConfig.h>

#include <ves/conductor/util/BaseDialog.h>

class wxSizer;
class wxWindow;
class wxImage;
class wxBitmapButton;
#include <wx/spinctrl.h>

namespace ves
{
namespace conductor
{
namespace util
{
class wxSpinCtrlDbl;
class VE_GUIPLUGINS_EXPORTS UI_TransientDialog : public BaseDialog 
{
public:
   UI_TransientDialog(int numTimeSteps,
                    wxWindow* parent, 
		               wxWindowID id = -1, 
                     std::string title = "Transient Controls");
   virtual ~UI_TransientDialog(){}

   enum TRANS_DIALOG_IDS
   {
      PLAY_BUTTON,
      STOP_BUTTON,
      FORWARD_STEP_BUTTON,
      BACKWARD_STEP_BUTTON,
      DURATION_CNTL_BOX,
      CURRENT_FRAME
   };

   ///Set the command prefix so that we can use this control to
   ///send different commands.
   ///\param newPrefix The command prefix
   void SetCommandPrefix(std::string newPrefix);
protected:
   virtual void _buildGUI();

   unsigned int _nTimeSteps;
   std::string _commandPrefix;///<The command prefix.

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
}
}
}
#endif //UI_TRANSIENT_DIALOG_H_

