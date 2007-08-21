/*************** <auto-copyright.pl BEGIN do not edit this line> *************
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
 *************** <auto-copyright.pl END do not edit this line> **************/
#ifndef WP_DIALOG_H
#define WP_DIALOG_H


#include <string> 
#include "VE_Installer/include/VEConfig.h"
#include "VE_Conductor/Utilities/BaseDialog.h"
#include "VE_Conductor/Utilities/DualSlider.h"

#include <wx/spinctrl.h>
#include <wx/stattext.h>
#include <wx/button.h>
namespace VE_Conductor
{

namespace GUI_Utilities
{
class VE_CONDUCTOR_UTILS_EXPORTS WPDialog : public BaseDialog

{
public:
   enum SEEDPOINT_IDS
   {
      DIMENSION_SPINNER_ID
   };
   //Constructor
      WPDialog(wxWindow* parent, int id, std::string title);
   
   //Destructor
   virtual ~WPDialog();
   
   ///Set the name of the command
   void SetCommandName(std::string name);
  
   ///Send the commands to Xplorer;
   void SendCommands();

   ///Add an instruction to send. This is for access in the callbacks.
   void AddInstruction(VE_XML::DataValuePair* newInstruct);
   
   void SetCommand(std::string name){_commandName = name;}
   
   ///Get the seed point vector
   std::vector< VE_XML::DataValuePair* > GetSeedPointDVPVector( void );
   ///Callback used to set the dvp vector
   void SetVectorDVP( void );
   ///Transfer the seed points info from the window
   virtual bool TransferDataFromWindow( void );

   ///Get the slider values for the bounds
   ///\param bounds The current slider values
   void GetBounds(std::vector<double>& bounds);

   ///Get the spinner values for the dimensions
   ///\param dimensions The x y z dimensions
   void GetDimensions(std::vector<long>& dimensions);
protected:
   
   ///Update the dimensions of the seed points bbox
   ///\param event The wxCommand event.
   void _updateDimensions(wxSpinEvent& event);

   class WPMinSliderCallback: public
   	VE_Conductor::GUI_Utilities::DualSlider::SliderCallback
   {
      public:
      
      WPMinSliderCallback(WPDialog* parent, std::string direction = "X")
      
      {
         _direction = direction;
	      _wpdlg = parent;
      }
      
      virtual ~WPMinSliderCallback()
      {
         _direction.clear();
      }
      
      virtual void SliderOperation();
      
      protected:
      std::string _direction;
      WPDialog* _wpdlg;
    };


   class WPBothMoveCallback: public
   	VE_Conductor::GUI_Utilities::DualSlider::SliderCallback
   {
      public:
      
      WPBothMoveCallback(WPDialog* parent, std::string direction = "X")
      
      {
         _direction = direction;
	      _wpdlg = parent;
      }
      
      virtual ~WPBothMoveCallback()
      {
         _direction.clear();
      }
      
      virtual void SliderOperation();
      
      protected:
      std::string _direction;
      WPDialog* _wpdlg;
    };



   class WPMaxSliderCallback: public
   	VE_Conductor::GUI_Utilities::DualSlider::SliderCallback
   {
      public:
      
      WPMaxSliderCallback(WPDialog* parent, std::string direction = "X")
      
      {
         _direction = direction;
	 _wpdlg = parent;
      }
      
      virtual ~WPMaxSliderCallback()
      {
         _direction.clear();
      }
      
      virtual void SliderOperation();
      
      protected:
      std::string _direction;
      WPDialog* _wpdlg;
    };

   //Build the DualSliders for the dialog
   void _createDualSliders();

   //Add controls for dialog
   virtual void _buildGUI();
   virtual wxSizer* _buildSpecificWidgets();
   
   //
   VE_Conductor::GUI_Utilities::DualSlider* _xBounds;
   VE_Conductor::GUI_Utilities::DualSlider* _yBounds;
   VE_Conductor::GUI_Utilities::DualSlider* _zBounds;
   std::vector< VE_XML::DataValuePair* > seedPointDVP;
   wxSpinCtrl* numXPointsSpinner;
   wxSpinCtrl* numYPointsSpinner;
   wxSpinCtrl* numZPointsSpinner;
   DECLARE_EVENT_TABLE()

};
}
}
#endif  //_WP_DIALOG_H
