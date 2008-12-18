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
#ifndef _ISOSURFACES_H_
#define _ISOSURFACES_H_
/*!\file isosurfaces.h
*isosurfaces API
*/
/*!\class isosurfaces
*
*/
#include <wx/dialog.h>

#include <ves/VEConfig.h>

#include <vector>
#include <map>

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

namespace ves
{
namespace conductor
{
namespace util
{
class wxSpinCtrlDbl;
}
class Vistab;
class Network;
class AdvancedIsosurface;
class VE_GUIPLUGINS_EXPORTS Isosurfaces: public wxDialog
{
//    DECLARE_DYNAMIC_CLASS( Isosurfaces )
    DECLARE_EVENT_TABLE()

public:
    /// Constructors
    Isosurfaces( );
    Isosurfaces( wxWindow* parent,
                 wxWindowID id = SYMBOL_ISOSURFACES_IDNAME,
                 const wxString& caption = SYMBOL_ISOSURFACES_TITLE,
                 const wxPoint& pos = SYMBOL_ISOSURFACES_POSITION,
                 const wxSize& size = SYMBOL_ISOSURFACES_SIZE,
                 long style = SYMBOL_ISOSURFACES_STYLE );
    virtual ~Isosurfaces();

    void SendCommandsToXplorer( void );
    /// Creation
    bool Create( wxWindow* parent,
                 wxWindowID id = SYMBOL_ISOSURFACES_IDNAME,
                 const wxString& caption = SYMBOL_ISOSURFACES_TITLE,
                 const wxPoint& pos = SYMBOL_ISOSURFACES_POSITION,
                 const wxSize& size = SYMBOL_ISOSURFACES_SIZE,
                 long style = SYMBOL_ISOSURFACES_STYLE );

    /// Creates the controls and sizers
    void CreateControls();

    ///The name of the available scalars.
    ///\param scalarNames all the scalars in this dataset
    void SetAvailableScalars( wxArrayString scalarNames );

    /// Retrieves bitmap resources
    wxBitmap GetBitmapResource( const wxString& name );

    /// Retrieves icon resources
    wxIcon GetIconResource( const wxString& name );

    /// Should we show tooltips?
    static bool ShowToolTips();

    /// Retrieves active scalar range
    void SetScalarRange( std::string, std::vector<double> );

    void SetScalarList( std::map<std::string, std::vector<double> > colorScalarRanges );

    AdvancedIsosurface* advancediso;

protected:
    std::string _colorByScalarName;///<The name of the scalar to color by.
    std::string _activeScalar;///The scalar that is active on the vistab
    std::vector<double> _scalarRange;///The scalar range that is active on vistab
    wxArrayString _scalarNames;///<The available scalars.
    wxCheckBox* _useNearestPreComputedCheckBox;
    wxSlider* _isoSurfaceSlider;///<Set the value of the iso-surface
    wxButton* _advancedButton;///<Display the color by scalar dialog
    wxButton* _computeButton;///<Compute the iso-surface
    util::wxSpinCtrlDbl* _isoSpinner;///<Display real scalar value

    void _onIsosurface( wxCommandEvent& event );

    /// wxEVT_COMMAND_CHECKBOX_CLICKED event handler for ID_CHECKBOX
    void _onPrecomputedIsosurface( wxCommandEvent& event );

    /// wxEVT_COMMAND_SLIDER_UPDATED event handler for ID_SLIDER
    void _onIsosurfacePlane( wxCommandEvent& event );

    /// wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON1
    void _onAddIsosurface( wxCommandEvent& event );

    ///wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON1
    void _onAdvanced( wxCommandEvent& event );

    ///wxEVT_COMMAND_SPINCTRL_UPDATED event handler for ID_SPINNER
    void _onSpinner( wxScrollEvent& WXUNUSED( event ) );

    ///wxEVT_COMMAND_TEXT_UPDATED event handler for ID_SPINNER
    void UpdateSlider( wxCommandEvent& event );

    double _minValue;
    double _maxValue;

    std::map<std::string, std::vector<double> > scalarlist;

    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }
};
}
}
#endif
// _ISOSURFACES_H_
