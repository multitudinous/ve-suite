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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef CAD_OPACITY_SLIDER_H
#define CAD_OPACITY_SLIDER_H
#include "VE_Open/skel/VjObsC.h"
#include "VE_Installer/include/VEConfig.h"

#include <string>
#include <vector>

namespace VE_CAD
{
   class CADMaterial;
}
namespace VE_XML
{
   class DataValuePair;
}
#include <wx/dialog.h>
class wxSlider;


/*!\file CADOpacitySliderDlg.h
  CADNodeManagerDialog API
  */
/*!\class CADOpacitySliderDlg
 * GUI class to manipulate opacity of CADMaterial.
 */


namespace VE_Conductor
{
namespace GUI_Utilities
{

class VE_CONDUCTOR_UTILS_EXPORTS CADOpacitySliderDlg:public wxDialog
{
public:
   enum OPACITY_DLG_IDS
   {
      OPACITY_SLIDER
   };
   ///Constructor
   ///\param parent The parent wxWindow.
   ///\param id The unique id for this window.
   CADOpacitySliderDlg(wxWindow* parent, int id,unsigned int cadID,VE_CAD::CADMaterial* material);

   ///Destructor 
   virtual ~CADOpacitySliderDlg();
   
   ///Set the current vjObjs ptr for data passing.
   ///\param xplorerCom The communication interface w/ xplorer.
   void SetVjObsPtr(VjObs_ptr xplorerCom);

   ///Set the value of the slider
   ///\param value The new opacity value.
   void SetSliderValue(double value);

   ///Get the current opacity value
   double GetOpacity();
protected:
   ///The wxSlider callback to update opacity.
   ///\param event The wxScrollEvent.
   void _onSlider(wxScrollEvent& event);

   ///Clear out the current list of commands.
   void _clearInstructions();

   ///Build the dialog internally.
   void _buildDialog();

   unsigned int _cadID;///<The id of the CADNode the material belongs to.
   std::vector<VE_XML::DataValuePair*> _instructions;///<The DataValuePair s for the current command.
   std::string _commandName;///<The command name.
   VE_CAD::CADMaterial* _material;///<The CADMaterial we are updating.

   ///Send CAD commands back to VE-Xplorer
   void _sendCommandsToXplorer();

   VjObs_var _vjObsPtr;///<The VjObj ptr.
   wxSlider* _opacitySlider;///<The wxSlider.

   DECLARE_EVENT_TABLE()
};
}
}
#endif// CAD_OPACITY_SLIDER_H
