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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef ROI_DIALOG_H
#define ROI_DIALOG_H
/*!\file ROIDialog.h
  ROIDialog API
  */
/*!\class ROIDialog
 * GUI class to adjust Volume Visualization clip planes .
 * Bounds are handled as %'s of the bounding box.
 */

#include <string>
#include "VE_Installer/include/VEConfig.h"
#include "VE_Conductor/Utilities/BaseDialog.h"
#include "VE_Conductor/Utilities/DualSlider.h"
namespace VE_Conductor
{
namespace GUI_Utilities
{
class VE_CONDUCTOR_UTILS_EXPORTS ROIDialog : public BaseDialog 
{
public:
   ///Constructor
   ROIDialog(wxWindow* parent, int id,std::string title);
   ///Destructor
   virtual ~ROIDialog();

   ///Set the name of the command
   ///\param name The name of the command.
   void SetCommandName(std::string name);
  
   ///Send the commands to Xplorer;
   void SendCommands();

   ///Add an instruction to send. This is for access in the callbacks.
   ///\param newInstruct The instruction to add to the Command.
   void AddInstruction(VE_XML::DataValuePair* newInstruct);
protected:
   /*!\class ROIMinSliderCallback
    *Class that allows the user to do operations based on the min slider events
    */
   class ROIMinSliderCallback:
          public VE_Conductor::GUI_Utilities::DualSlider::SliderCallback
   {
      public:
        ///Constructors
        ROIMinSliderCallback(ROIDialog* parent,std::string direction = "X")
        {
            _direction = direction;
            _roidlg = parent;
        }
        ///Destructor
        virtual ~ROIMinSliderCallback(){_direction.clear();}
        
        ///The operation to do for the slider
        virtual void SliderOperation();      
      protected:
         std::string _direction;
         ROIDialog* _roidlg;
         
   };
   /*!\class ROIBothMoveCallback
    *Class that allows the user to do operations based on both sliders moving, i.e.
    *This is caused by the slider buffer being reached.
    */
   class ROIBothMoveCallback:
          public VE_Conductor::GUI_Utilities::DualSlider::SliderCallback
   {
      public:
        ///Constructors
        ROIBothMoveCallback(ROIDialog* parent,std::string direction = "X")
        {
           _direction = direction;
           _roidlg = parent;
        }
        ///Destructor
        virtual ~ROIBothMoveCallback(){_direction.clear();}
        
        ///The operation to do for the slider
        virtual void SliderOperation();      
      protected:
         std::string _direction;
         ROIDialog* _roidlg;
   };
   /*!\class ROIMaxSliderCallback
    *Class that allows the user to do operations based on the max slider events
    */
   class ROIMaxSliderCallback:
          public VE_Conductor::GUI_Utilities::DualSlider::SliderCallback
   {
      public:
        ///Constructors
        ROIMaxSliderCallback(ROIDialog* parent,std::string direction = "X")
        { 
           _direction = direction;
           _roidlg = parent;
        }
        ///Destructor
        virtual ~ROIMaxSliderCallback(){_direction.clear();}
        
        ///The operation to do for the slider
        virtual void SliderOperation();      
      protected:
         std::string _direction;
         ROIDialog* _roidlg;
   };
   ///Build the DualSlider s for this dialog
   void _createDualSliders();
   ///Add the controls to the dialog
   virtual void _buildGUI();
   virtual wxSizer* _buildSpecificWidgets();

   VE_Conductor::GUI_Utilities::DualSlider* _xBounds;///<DualSlider for x bounds
   VE_Conductor::GUI_Utilities::DualSlider* _yBounds;///<DualSlider for y bounds
   VE_Conductor::GUI_Utilities::DualSlider* _zBounds;///<DualSlider for z bounds
};
}
}
#endif// ROI_DIALOG_H
