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

#ifndef _ADVANCEDCONTOURS_H_
#define _ADVANCEDCONTOURS_H_


/*!
 * Includes
 */

////@begin includes
#include "VE_Open/skel/VjObsC.h"
#include "VE_Conductor/VE_UI/UI_TransientDialog.h"
#include <xercesc/dom/DOM.hpp>
#include <vector>
////@end includes

/*!
 * Forward declarations
 */

////@begin forward declarations
XERCES_CPP_NAMESPACE_USE
namespace VE_XML
{
   class Command;
   class DOMDocumentManager;
}

class wxSlider;
////@end forward declarations

/*!
 * Control identifiers
 */

////@begin control identifiers
#define ID_DIALOG 10000
#define SYMBOL_ADVANCEDCONTOURS_STYLE wxCAPTION|wxRESIZE_BORDER|wxSYSTEM_MENU|wxCLOSE_BOX
#define SYMBOL_ADVANCEDCONTOURS_TITLE _T("AdvancedContours")
#define SYMBOL_ADVANCEDCONTOURS_IDNAME ID_DIALOG
#define SYMBOL_ADVANCEDCONTOURS_SIZE wxSize(400, 300)
#define SYMBOL_ADVANCEDCONTOURS_POSITION wxDefaultPosition
#define ID_SLIDER 10001
#define ID_SLIDER1 10002
#define ID_SLIDER2 10003

enum ADVANCED_CONTOUR_IDS
{
   OPACITY_SLIDER,
   WARPED_SCALE_SLIDER,
   LOD_SLIDER
};
////@end control identifiers

/*!
 * AdvancedContoursApp class declaration
 */
/*
class AdvancedContoursApp: public wxApp
{    
    DECLARE_CLASS( AdvancedContoursApp )
    DECLARE_EVENT_TABLE()

public:
    /// Constructor
    AdvancedContoursApp();

    /// Initialises the application
    virtual bool OnInit();

    /// Called on exit
    virtual int OnExit();

////@begin AdvancedContoursApp event handler declarations
////@end AdvancedContoursApp event handler declarations

////@begin AdvancedContoursApp member function declarations
////@end AdvancedContoursApp member function declarations

////@begin AdvancedContoursApp member variables
////@end AdvancedContoursApp member variables
};
*/
/*!
 * Application instance declaration 
 */

////@begin declare app
//DECLARE_APP(AdvancedContoursApp)
////@end declare app

/*!
 * AdvancedContours class declaration
 */

class AdvancedContours: public wxDialog
{    
//    DECLARE_DYNAMIC_CLASS( AdvancedContours )
    DECLARE_EVENT_TABLE()

public:
    /// Constructors
    AdvancedContours( );
    AdvancedContours( wxWindow* parent, wxWindowID id = SYMBOL_ADVANCEDCONTOURS_IDNAME, const wxString& caption = SYMBOL_ADVANCEDCONTOURS_TITLE, const wxPoint& pos = SYMBOL_ADVANCEDCONTOURS_POSITION, const wxSize& size = SYMBOL_ADVANCEDCONTOURS_SIZE, long style = SYMBOL_ADVANCEDCONTOURS_STYLE );
//    AdvancedContours(VjObs_ptr veEngine, VE_XML::DOMDocumentManager* domManagerIn);
    void SendCommandsToXplorer( void );
    void SetCommInstance( VjObs_ptr veEngine );
    /// Creation
    bool Create( wxWindow* parent, 
       wxWindowID id = SYMBOL_ADVANCEDCONTOURS_IDNAME,
       const wxString& caption = SYMBOL_ADVANCEDCONTOURS_TITLE,
       const wxPoint& pos = SYMBOL_ADVANCEDCONTOURS_POSITION, 
       const wxSize& size = SYMBOL_ADVANCEDCONTOURS_SIZE, 
       long style = SYMBOL_ADVANCEDCONTOURS_STYLE );

   /// Creates the controls and sizers
   void CreateControls();
   
   ///The opacity setting.
   ///\param opacity The opacity setting
   void SetOpacity(double opacity);
   ///The warped scale.
   ///\param warpScale The scale for warping on the contours
   void SetWarpedScale(double warpScale);
   ///The Level of Detail
   ///\param LOD The LOD setting
   void SetLOD(double LOD);

    /// Retrieves bitmap resources
    wxBitmap GetBitmapResource( const wxString& name );

    /// Retrieves icon resources
    wxIcon GetIconResource( const wxString& name );

    /// Should we show tooltips?
    static bool ShowToolTips();

    ///The opacity setting.
    double GetOpacity();
    ///The warped scale.
    double GetWarpedScale();
    ///The Level of Detail
    double GetLOD();

protected:
   ////@begin AdvancedContours event handler declarations
    /// wxEVT_COMMAND_SLIDER_UPDATED event handler for ID_SLIDER
    void _onContourOpacity( wxCommandEvent& event );

    /// wxEVT_COMMAND_SLIDER_UPDATED event handler for ID_SLIDER1
    void _onWarpedContour( wxCommandEvent& event );

    /// wxEVT_COMMAND_SLIDER_UPDATED event handler for ID_SLIDER2
    void _onContourLOD( wxCommandEvent& event );
////@end AdvancedContours event handler declarations

   wxSlider* _opacitySlider;///<Opacity slider.
   wxSlider* _warpedScaleSlider;///<Warped scale slider.
   wxSlider* _LODSlider;///<Level Of Detail slider.
   
   double _opacity;///<The opacity setting.
   double _warpedScale;///<The warped contour scale setting.
   double _LOD;///<The Level Of Detail setting.

   std::vector< VE_XML::Command* > commands;
   VjObs_ptr xplorerPtr;
   int cId, cIso_value, cMin, cMax, cSc;
   std::vector< long > commandInputs;
   DOMDocument* doc;
   VE_XML::DOMDocumentManager* domManager;
};

#endif
    // _ADVANCEDCONTOURS_H_
