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

#ifndef TextureBasedIsosurfaceDlg_H_
#define TextureBasedIsosurfaceDlg_H_
#include "VE_Conductor/Utilities/BaseDialog.h"

#include "VE_Open/skel/VjObsC.h"
#include "VE_Conductor/Framework/UI_TransientDialog.h"
#include "VE_Installer/include/VEConfig.h"

#include <xercesc/dom/DOM.hpp>
#include <vector>
XERCES_CPP_NAMESPACE_USE
namespace VE_XML
{
   class Command;
   class DOMDocumentManager;
}

class wxRadioButton;
class wxCheckBox;
class wxSlider;
class wxButton;
class wxStaticBox;
class wxComboBox;

#define ID_DIALOG 10000
#define SYMBOL_ISOSURFACES_STYLE wxCAPTION|wxRESIZE_BORDER|wxSYSTEM_MENU|wxCLOSE_BOX
#define SYMBOL_ISOSURFACES_TITLE _T("Isosurfaces")
#define SYMBOL_ISOSURFACES_IDNAME ID_DIALOG
#define SYMBOL_ISOSURFACES_SIZE wxSize(400, 300)
#define SYMBOL_ISOSURFACES_POSITION wxDefaultPosition

enum TBISOSURFACE_IDS
{
   TBISOSURFACE_RBUTTON,
   TBPRECOMPUTED_ISO_CHK,
   TBISOSURFACE_PLANE_SLIDER,
   TBADD_ISOSURFACE_BUTTON,
   TBADVANCED_ISOSURFACE_BUTTON
};

class VE_CONDUCTOR_UTILS_EXPORTS TextureBasedIsosurfaceDlg: public VE_Conductor::GUI_Utilities::BaseDialog
{    
    DECLARE_EVENT_TABLE()

public:
    /// Constructors
    TextureBasedIsosurfaceDlg(  wxWindow* parent, 
                  wxWindowID id =-1, 
                  std::string title = "wxDialog" );

    ///The name of the available scalars.
    ///\param scalarNames all the scalars in this dataset
    void SetAvailableScalars(wxArrayString scalarNames);
    void SetActiveScalar(std::string activeScalar);
       
protected:
   wxArrayString _scalarNames;///<The available scalars.
   wxComboBox* _availableScalars;///The widget for the available scalars;
   wxSlider* _isoSurfaceSlider;///<Set the value of the iso-surface
   wxButton* _advancedButton;
   std::string _colorByScalarName;
   std::string _activeScalar;
   
   void _onUpdateIsoSurface( wxCommandEvent& event );
   void _buildGUI( void );
   ///wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON1
   void _onAdvanced(wxCommandEvent& event);
};
#endif // TextureBasedIsosurfaceDlg_H_
