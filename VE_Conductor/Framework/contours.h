/////////////////////////////////////////////////////////////////////////////
// Name:        contours.h
// Purpose:     
// Author:      Jared Abodeely
// Modified by: 
// Created:     Thu 20 Apr 2006 19:49:24 CDT
// RCS-ID:      
// Copyright:   
// Licence:     
/////////////////////////////////////////////////////////////////////////////

// Generated by DialogBlocks, Thu 20 Apr 2006 19:49:24 CDT

#ifndef _CONTOURS_H_
#define _CONTOURS_H_


/*!
 * Includes
 */

////@begin includes
#include "VE_Open/skel/VjObsC.h"
#include "VE_Conductor/VE_UI/UI_TransientDialog.h"
#include "VE_Conductor/Framework/advancedcontours.h"
#include <xercesc/dom/DOM.hpp>
#include <vector>
////@end includes

/*!
 * Forward declarations
 */

////@begin forward declarations
enum CONTOUR_IDS
{
   CONTOUR_DIR_RBOX,
   CONTOUR_TYPE_RBOX,
   MULTIPLE_PRECONTOUR_RBUTTON,
   MULTIPLE_PRECONTOUR_CHK,
   SINGLE_PRECONTOUR_RBUTTON,
   SINGLE_PRECONTOUR_CHK,
   CONTOUR_PLANE_SLIDER,
   ADD_CONTOUR_PLANE_BUTTON,
   ADVANCED_CONTOUR_BUTTON
};
////@end forward declarations
XERCES_CPP_NAMESPACE_USE
namespace VE_XML
{
   class Command;
   class DOMDocumentManager;
}
/*!
 * Control identifiers
 */

////@begin control identifiers
#define ID_DIALOG 10000
#define SYMBOL_CONTOURS_STYLE wxCAPTION|wxRESIZE_BORDER|wxSYSTEM_MENU|wxCLOSE_BOX
#define SYMBOL_CONTOURS_TITLE _T("Contours")
#define SYMBOL_CONTOURS_IDNAME ID_DIALOG
#define SYMBOL_CONTOURS_SIZE wxSize(400, 300)
#define SYMBOL_CONTOURS_POSITION wxDefaultPosition
#define ID_RADIOBOX 10001
#define ID_RADIOBOX1 10007
#define ID_RADIOBUTTON 10002
#define ID_CHECKBOX 10003
#define ID_RADIOBUTTON1 10004
#define ID_CHECKBOX1 10005
#define ID_SLIDER 10008
#define ID_BUTTON 10006
#define ID_BUTTON1 10009
////@end control identifiers

/*!
 * Compatibility
 */

#ifndef wxCLOSE_BOX
#define wxCLOSE_BOX 0x1000
#endif

/*!
 * Contours class declaration
 */

class Contours: public wxDialog
{    
//    DECLARE_DYNAMIC_CLASS( Contours )
    DECLARE_EVENT_TABLE()

public:
    /// Constructors
//    Contours( );
//    Contours( wxWindow* parent, wxWindowID id = SYMBOL_CONTOURS_IDNAME, const wxString& caption = SYMBOL_CONTOURS_TITLE, const wxPoint& pos = SYMBOL_CONTOURS_POSITION, const wxSize& size = SYMBOL_CONTOURS_SIZE, long style = SYMBOL_CONTOURS_STYLE );
    Contours(VjObs_ptr veEngine, VE_XML::DOMDocumentManager* domManagerIn);
    void SendCommandsToXplorer( void );
    void SetCommInstance( VjObs_ptr veEngine );
    /// Creation
    bool Create( wxWindow* parent, wxWindowID id = SYMBOL_CONTOURS_IDNAME, const wxString& caption = SYMBOL_CONTOURS_TITLE, const wxPoint& pos = SYMBOL_CONTOURS_POSITION, const wxSize& size = SYMBOL_CONTOURS_SIZE, long style = SYMBOL_CONTOURS_STYLE );

    /// Creates the controls and sizers
    void CreateControls();

////@begin Contours event handler declarations
    /// wxEVT_COMMAND_RADIOBOX_SELECTED event handler for ID_RADIOBOX
    void _onDirection( wxCommandEvent& event );

    /// wxEVT_COMMAND_RADIOBOX_SELECTED event handler for ID_RADIOBOX1
    void _onContourType( wxCommandEvent& event );

    /// wxEVT_COMMAND_RADIOBUTTON_SELECTED event handler for ID_RADIOBUTTON
    void _onMultiplePlanes( wxCommandEvent& event );

    /// wxEVT_COMMAND_CHECKBOX_CLICKED event handler for ID_CHECKBOX
    void _onCyclePlanes( wxCommandEvent& event );

    /// wxEVT_COMMAND_RADIOBUTTON_SELECTED event handler for ID_RADIOBUTTON1
    void _onSinglePlane( wxCommandEvent& event );

    /// wxEVT_COMMAND_CHECKBOX_CLICKED event handler for ID_CHECKBOX1
    void _onPrecomputedPlane( wxCommandEvent& event );

    /// wxEVT_COMMAND_SLIDER_UPDATED event handler for ID_SLIDER
    void _onPlane( wxCommandEvent& event );

    /// wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON1
    void _onAddPlane( wxCommandEvent& event );

    /// wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON
    void _onAdvanced( wxCommandEvent& event );
////@end Contours event handler declarations

////@begin Contours member function declarations

    /// Retrieves bitmap resources
    wxBitmap GetBitmapResource( const wxString& name );

    /// Retrieves icon resources
    wxIcon GetIconResource( const wxString& name );
////@end Contours member function declarations

    /// Should we show tooltips?
    static bool ShowToolTips();

////@begin Contours member variables
////@end Contours member variables

protected:
   std::vector< VE_XML::Command* > commands;
   VjObs_ptr xplorerPtr;
   int cId, cIso_value, cMin, cMax, cSc;
   std::vector< long > commandInputs;
   DOMDocument* doc;
   VE_XML::DOMDocumentManager* domManager;

   AdvancedContours* adContour;
};

#endif
    // _CONTOURS_H_
