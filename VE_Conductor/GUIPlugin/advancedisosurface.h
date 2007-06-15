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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _ADVANCEDISOSURFACES_H_
#define _ADVANCEDISOSURFACES_H_

#include "VE_Conductor/Utilities/DualSlider.h"
#include "VE_Builder/Utilities/gui/spinctld.h"

////@end includes
#include <vector>

#include <string>

#include <wx/dialog.h>

#include "VE_Installer/include/VEConfig.h"

class wxListBox;
class wxSpinCtrlDbl;

#define ID_ADVANCEDISOSURFACES 10000
//#define ID_LISTBOX1 10001
#define SYMBOL_ADVANCEDISOSURFACES_STYLE wxCAPTION|wxRESIZE_BORDER|wxSYSTEM_MENU|wxCLOSE_BOX
#define SYMBOL_ADVANCEDISOSURFACES_TITLE _("Advanced Isosurfaces")
#define SYMBOL_ADVANCEDISOSURFACES_IDNAME ID_ADVANCEDISOSURFACES
#define SYMBOL_ADVANCEDISOSURFACES_SIZE wxSize(400, 300)
#define SYMBOL_ADVANCEDISOSURFACES_POSITION wxDefaultPosition

#ifndef wxCLOSE_BOX
#define wxCLOSE_BOX 0x1000
#endif

class VE_GUIPLUGINS_EXPORTS AdvancedIsosurface: public wxDialog
{    
    DECLARE_DYNAMIC_CLASS( AdvancedIsosurface )
    DECLARE_EVENT_TABLE()

public:
    AdvancedIsosurface( );
    AdvancedIsosurface( wxWindow* parent,
						wxWindowID id = SYMBOL_ADVANCEDISOSURFACES_IDNAME,
						const wxString& caption = SYMBOL_ADVANCEDISOSURFACES_TITLE,
						const wxPoint& pos = SYMBOL_ADVANCEDISOSURFACES_POSITION,
						const wxSize& size = SYMBOL_ADVANCEDISOSURFACES_SIZE,
						long style = SYMBOL_ADVANCEDISOSURFACES_STYLE );
   virtual ~AdvancedIsosurface( void ){ ; }

   enum ADVANCEDISO_IDS
   {
      MIN_SPINCTRL,
      MAX_SPINCTRL,
      MIN_SLIDER,
      MAX_SLIDER,
	  SELECT_SCALAR
   };

    bool Create( wxWindow* parent,
				 wxWindowID id = SYMBOL_ADVANCEDISOSURFACES_IDNAME,
				 const wxString& caption = SYMBOL_ADVANCEDISOSURFACES_TITLE,
				 const wxPoint& pos = SYMBOL_ADVANCEDISOSURFACES_POSITION,
				 const wxSize& size = SYMBOL_ADVANCEDISOSURFACES_SIZE,
				 long style = SYMBOL_ADVANCEDISOSURFACES_STYLE );

    /// Creates the controls and sizers
    void CreateControls();
	/// Callback for the scalar selection
	void OnScalarSelection( wxCommandEvent& WXUNUSED(event) );
	/// Callback for the minimum scalar spinner
	void OnMinSpinCtrl( wxScrollEvent& WXUNUSED(event) );
	/// Callback for the maximum scalar spinner
	void OnMaxSpinCtrl( wxScrollEvent& WXUNUSED(event) );
	/// Callback for the minimum scalar slider
	void OnMinSlider( wxScrollEvent& WXUNUSED(event) );
	/// Callback for the maximum scalar slider
	void OnMaxSlider( wxScrollEvent& WXUNUSED(event) );
	/// Callback for text on minimum spinner
	void UpdateMinSlider( wxCommandEvent& event );
	/// Callback for text on maximum spinner	
	void UpdateMaxSlider( wxCommandEvent& event );

    wxBitmap GetBitmapResource( const wxString& name );

    wxIcon GetIconResource( const wxString& name );

    static bool ShowToolTips();
    void PopulateList( wxArrayString scalarlist );
    void SetScalarList( std::map<std::string,std::vector<double> > colorScalarRanges );
    void SetActiveScalar( std::string activeScalar );
    void SetScalarRange( void );
    bool _ensureSliders( int activeSliderID );
    double GetMinScalarValue( void );
    double GetMaxScalarValue( void );
    std::string GetScalarName( void );
protected:

	wxListBox* scalarSelection;
	wxSpinCtrlDbl* _minSpinner;
    wxSpinCtrlDbl* _maxSpinner;
    wxSlider* _minSlider;
    wxSlider* _maxSlider;

	std::map<std::string,wxArrayString> _availableSolutions;
	std::map<std::string,std::vector<double> > _colorScalarRanges;///<The scalar range for the active scalar
    std::vector<double> _colorScalarRange;///<The active scalars range.
	std::string _colorScalarName;///<The selected scalar
	std::string _activeScalar;

   std::string ConvertUnicode( const wxChar* data )
   {
      std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
      return tempStr;
   }
};

#endif
    // _ADVANCEDISOSURFACES_H_
