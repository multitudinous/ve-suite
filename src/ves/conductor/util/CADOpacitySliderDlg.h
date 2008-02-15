/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef CAD_OPACITY_SLIDER_H
#define CAD_OPACITY_SLIDER_H
/*!\file CADOpacitySliderDlg.h
CADOpacitySliderDlg API
*/
/*!\class ves::conductor::util::CADOpacitySliderDlg
*
*/
#include <ves/open/VjObsC.h>
#include <ves/VEConfig.h>

#include <ves/open/xml/DataValuePairPtr.h>

#include <string>
#include <vector>

namespace ves
{
namespace open
{
namespace xml
{
namespace cad
{
class CADMaterial;
}
}
}
}

#include <wx/dialog.h>
class wxSlider;


/*!\file CADOpacitySliderDlg.h
  CADNodeManagerDialog API
  */
/*!\class CADOpacitySliderDlg
 * GUI class to manipulate opacity of CADMaterial.
 */

namespace ves
{
namespace conductor
{
namespace util
{
class VE_CONDUCTOR_UTILS_EXPORTS CADOpacitySliderDlg: public wxDialog
{
public:
    enum OPACITY_DLG_IDS
    {
        OPACITY_SLIDER
    };
    ///Constructor
    ///\param parent The parent wxWindow.
    ///\param id The unique id for this window.
    CADOpacitySliderDlg( wxWindow* parent, int id, std::string cadID, ves::open::xml::cad::CADMaterial* material );

    ///Destructor
    virtual ~CADOpacitySliderDlg();

    ///Set the value of the slider
    ///\param value The new opacity value.
    void SetSliderValue( double value );

    ///Get the current opacity value
    double GetOpacity();
protected:
    ///The wxSlider callback to update opacity.
    ///\param event The wxScrollEvent.
    void _onSlider( wxScrollEvent& event );

    ///Clear out the current list of commands.
    void _clearInstructions();

    ///Build the dialog internally.
    void _buildDialog();
    std::string _cadID;///<The id of the CADNode the material belongs to.
    std::vector<ves::open::xml::DataValuePairPtr> _instructions;///<The DataValuePair s for the current command.
    std::string _commandName;///<The command name.
    ves::open::xml::cad::CADMaterial* _material;///<The CADMaterial we are updating.

    ///Send CAD commands back to VE-Xplorer
    void _sendCommandsToXplorer();

    wxSlider* _opacitySlider;///<The wxSlider.

    DECLARE_EVENT_TABLE()
};
}
}
}
#endif// CAD_OPACITY_SLIDER_H
