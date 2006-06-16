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
 * File:          $RCSfile: GlobalParamDialog.h,v $
 * Date modified: $Date: 2006-03-23 17:47:31 -0600 (Thu, 23 Mar 2006) $
 * Version:       $Rev: 3957 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _ISOSURFACES_H_
#define _ISOSURFACES_H_

#if defined(__GNUG__) && !defined(NO_GCC_PRAGMA)
#pragma interface "isosurfaces.h"
#endif

#include "VE_Open/skel/VjObsC.h"
#include "VE_Conductor/VE_UI/UI_TransientDialog.h"
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

#define ID_DIALOG 10000
#define SYMBOL_ISOSURFACES_STYLE wxCAPTION|wxRESIZE_BORDER|wxSYSTEM_MENU|wxCLOSE_BOX
#define SYMBOL_ISOSURFACES_TITLE _T("Isosurfaces")
#define SYMBOL_ISOSURFACES_IDNAME ID_DIALOG
#define SYMBOL_ISOSURFACES_SIZE wxSize(400, 300)
#define SYMBOL_ISOSURFACES_POSITION wxDefaultPosition

enum ISOSURFACE_IDS
{
   ISOSURFACE_RBUTTON,
   PRECOMPUTED_ISO_CHK,
   ISOSURFACE_PLANE_SLIDER,
   ADD_ISOSURFACE_BUTTON,
   ADVANCED_ISOSURFACE_BUTTON
};

class VE_CONDUCTOR_UTILS_EXPORTS Isosurfaces: public BaseDialog
{    
    DECLARE_EVENT_TABLE()

public:
    /// Constructors
    TextureBasedIsosurfaceDlg(  wxWindow* parent, 
                  wxWindowID id =-1, 
                  std::string title);

    ///The name of the available scalars.
    ///\param scalarNames all the scalars in this dataset
    void SetAvailableScalars(wxArrayString scalarNames);
 
protected:
   wxArrayString _scalarNames;///<The available scalars.
   wxComboBox* _availableScalars;///The widget for the available scalars;
   wxSlider* _isoSurfaceSlider;///<Set the value of the iso-surface
   
   void _onIsosurface( wxCommandEvent& event );

   ///wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON1
   void _onAdvanced(wxCommandEvent& event);
};

#endif
    // _ISOSURFACES_H_
