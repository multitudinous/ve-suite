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

#ifndef _POLYDATA_H_
#define _POLYDATA_H_
/*!\file polydata.h
*polydata API
*/
/*!\class Polydata
*
*/
#include <vector>
#include <string>

#include <wx/dialog.h>

#include <ves/VEConfig.h>

class wxRadioButton;
class wxCheckBox;
class wxSlider;
class wxButton;
class wxStaticBox;

#define ID_DIALOG 10000
#define SYMBOL_POLYDATA_STYLE wxCAPTION|wxRESIZE_BORDER|wxSYSTEM_MENU|wxCLOSE_BOX
#define SYMBOL_POLYDATA_TITLE _T("Polydata")
#define SYMBOL_POLYDATA_IDNAME ID_DIALOG
#define SYMBOL_POLYDATA_SIZE wxSize(400, 300)
#define SYMBOL_POLYDATA_POSITION wxDefaultPosition

namespace ves
{
namespace conductor
{
class VE_GUIPLUGINS_EXPORTS Polydata: public wxDialog
{
//    DECLARE_DYNAMIC_CLASS( Isosurfaces )
    DECLARE_EVENT_TABLE()

public:
    /// Constructors
    Polydata( );
    Polydata( wxWindow* parent,
              wxWindowID id = SYMBOL_POLYDATA_IDNAME,
              const wxString& caption = SYMBOL_POLYDATA_TITLE,
              const wxPoint& pos = SYMBOL_POLYDATA_POSITION,
              const wxSize& size = SYMBOL_POLYDATA_SIZE,
              long style = SYMBOL_POLYDATA_STYLE );

    enum POLYDATA_IDS
    {
        POLYDATA_RBUTTON,
        WARPED_SURFACE_CHK,
        POLYDATA_PLANE_SLIDER,
        ADD_POLYDATA_BUTTON,
        ADVANCED_POLYDATA_BUTTON
    };

    void SendCommandsToXplorer( void );
    /// Creation
    bool Create( wxWindow* parent,
                 wxWindowID id = SYMBOL_POLYDATA_IDNAME,
                 const wxString& caption = SYMBOL_POLYDATA_TITLE,
                 const wxPoint& pos = SYMBOL_POLYDATA_POSITION,
                 const wxSize& size = SYMBOL_POLYDATA_SIZE,
                 long style = SYMBOL_POLYDATA_STYLE );

    /// Creates the controls and sizers
    void CreateControls();

    ///The name of the available scalars.
    ///\param scalarNames all the scalars in this dataset
    void SetAvailableScalars( wxArrayString scalarNames );

    ///Set the active scalar
    ///\param activeScalarName The active scalar name
    void SetActiveScalar( std::string activeScalarName );


    /// Retrieves bitmap resources
    wxBitmap GetBitmapResource( const wxString& name );

    /// Retrieves icon resources
    wxIcon GetIconResource( const wxString& name );

    /// Should we show tooltips?
    static bool ShowToolTips();

protected:
    std::string _colorByScalarName;///<The name of the scalar to color by.
    std::string _activeScalar;///The scalar that is active on the vistab
    wxArrayString _scalarNames;///<The available scalars.
    wxCheckBox* _useWarpedSurfaceCheckBox;
    wxSlider* _polydataSlider;///<Set the value of the iso-surface
    wxButton* _advancedButton;///<Display the color by scalar dialog
    wxButton* _computeButton;///<Compute the iso-surface

    void _onPolydata( wxCommandEvent& event );

    /// wxEVT_COMMAND_CHECKBOX_CLICKED event handler for ID_CHECKBOX
    void _onWarpedSurface( wxCommandEvent& event );

    /// wxEVT_COMMAND_SLIDER_UPDATED event handler for ID_SLIDER
    void _onPolydataPlane( wxCommandEvent& event );

    /// wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON1
    void _onAddPolydata( wxCommandEvent& event );

    ///wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON1
    void _onAdvanced( wxCommandEvent& event );

    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }
};
}
}
#endif
// _POLYDATA_H_
