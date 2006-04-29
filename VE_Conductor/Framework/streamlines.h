/////////////////////////////////////////////////////////////////////////////
// Name:        streamlines.h
// Purpose:     
// Author:      Jared Abodeely
// Modified by: 
// Created:     Fri 21 Apr 2006 10:45:04 CDT
// RCS-ID:      
// Copyright:   
// Licence:     
/////////////////////////////////////////////////////////////////////////////

#ifndef _STREAMLINES_H_
#define _STREAMLINES_H_

#if defined(__GNUG__) && !defined(NO_GCC_PRAGMA)
#pragma interface "streamlines.h"
#endif

/*!
 * Includes
 */

////@begin includes
#include "VE_Open/skel/VjObsC.h"
#include "VE_Conductor/VE_UI/UI_TransientDialog.h"
#include "VE_Conductor/Framework/advancedstreamlines.h"
#include <xercesc/dom/DOM.hpp>
#include <vector>
////@end includes

/*!
 * Forward declarations
 */

////@begin forward declarations
enum STREAMLINE_IDS
{
   CURSOR_RBOX,
   DIRECTION_RBOX,
   INTEGRATION_DIR_RBOX,
   PLANE_SIZE_SLIDER,
   NUMBER_PTS_SLIDER,
   ADVANCED_STREAMLINE_BUTTON
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
#define SYMBOL_STREAMLINES_STYLE wxCAPTION|wxRESIZE_BORDER|wxSYSTEM_MENU|wxCLOSE_BOX
#define SYMBOL_STREAMLINES_TITLE _("Streamlines")
#define SYMBOL_STREAMLINES_IDNAME ID_DIALOG
#define SYMBOL_STREAMLINES_SIZE wxSize(400, 300)
#define SYMBOL_STREAMLINES_POSITION wxDefaultPosition
#define ID_RADIOBOX 10001
#define ID_RADIOBOX1 10002
#define ID_RADIOBOX2 10003
#define ID_SLIDER 10004
#define ID_SLIDER1 10005
#define ID_BUTTON 10006
#define ID_BUTTON1 10007
////@end control identifiers

/*!
 * StreamlinesApp class declaration
 */
/*
class StreamlinesApp: public wxApp
{    
    DECLARE_CLASS( StreamlinesApp )
    DECLARE_EVENT_TABLE()

public:
    /// Constructor
    StreamlinesApp();

    /// Initialises the application
    virtual bool OnInit();

    /// Called on exit
    virtual int OnExit();

////@begin StreamlinesApp event handler declarations
////@end StreamlinesApp event handler declarations

////@begin StreamlinesApp member function declarations
////@end StreamlinesApp member function declarations

////@begin StreamlinesApp member variables
////@end StreamlinesApp member variables
};
*/
/*!
 * Application instance declaration 
 */

////@begin declare app
//DECLARE_APP(StreamlinesApp)
////@end declare app

/*!
 * Streamlines class declaration
 */

class Streamlines: public wxDialog
{    
//    DECLARE_DYNAMIC_CLASS( Streamlines )
    DECLARE_EVENT_TABLE()

public:
    /// Constructors
//    Streamlines( );
//    Streamlines( wxWindow* parent, wxWindowID id = SYMBOL_STREAMLINES_IDNAME, const wxString& caption = SYMBOL_STREAMLINES_TITLE, const wxPoint& pos = SYMBOL_STREAMLINES_POSITION, const wxSize& size = SYMBOL_STREAMLINES_SIZE, long style = SYMBOL_STREAMLINES_STYLE );
    Streamlines(VjObs_ptr veEngine, VE_XML::DOMDocumentManager* domManagerIn);
    void SendCommandsToXplorer( void );
    void SetCommInstance( VjObs_ptr veEngine );
    /// Creation
    bool Create( wxWindow* parent, wxWindowID id = SYMBOL_STREAMLINES_IDNAME, const wxString& caption = SYMBOL_STREAMLINES_TITLE, const wxPoint& pos = SYMBOL_STREAMLINES_POSITION, const wxSize& size = SYMBOL_STREAMLINES_SIZE, long style = SYMBOL_STREAMLINES_STYLE );

    /// Creates the controls and sizers
    void CreateControls();

////@begin Streamlines event handler declarations
    void _onCursorSelect(wxCommandEvent& );
    void _onDirection(wxCommandEvent& );
    void _onIntegrateDir(wxCommandEvent& );
    void _onnPointsSlider(wxScrollEvent& );
    void _onAdvanced(wxCommandEvent& );
////@end Streamlines event handler declarations

////@begin Streamlines member function declarations

    /// Retrieves bitmap resources
    wxBitmap GetBitmapResource( const wxString& name );

    /// Retrieves icon resources
    wxIcon GetIconResource( const wxString& name );
////@end Streamlines member function declarations

    /// Should we show tooltips?
    static bool ShowToolTips();

////@begin Streamlines member variables
////@end Streamlines member variables
protected:
   std::vector< VE_XML::Command* > commands;
   VjObs_ptr xplorerPtr;
   int cId, cIso_value, cMin, cMax, cSc;
   std::vector< long > commandInputs;
   DOMDocument* doc;
   VE_XML::DOMDocumentManager* domManager;

   AdvancedStreamlines* adStreamline;
};

#endif
    // _STREAMLINES_H_
