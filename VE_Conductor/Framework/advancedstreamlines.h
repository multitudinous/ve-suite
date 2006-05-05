/////////////////////////////////////////////////////////////////////////////
// Name:        advancedstreamlines.h
// Purpose:     
// Author:      Jared Abodeely
// Modified by: 
// Created:     Fri 21 Apr 2006 10:59:27 CDT
// RCS-ID:      
// Copyright:   
// Licence:     
/////////////////////////////////////////////////////////////////////////////

#ifndef _ADVANCEDSTREAMLINES_H_
#define _ADVANCEDSTREAMLINES_H_

/*
#if defined(__GNUG__) && !defined(NO_GCC_PRAGMA)
#pragma interface "advancedstreamlines.h"
#endif
*/

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
enum ADVANCED_STREAMLINE_IDS
{
   PARTICLE_TRACKING_BUTTON,
   USE_SEED_POINT_CHK,
   PROPOGATION_SLIDER,
   INTEGRATION_STEP_SLIDER,
   STEP_SIZE_SLIDER,
   ARROWS_CHK,
   SPHERE_SIZE_SLIDER,
   LINE_DIAMETER_SLIDER
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
#define SYMBOL_ADVANCEDSTREAMLINES_STYLE wxCAPTION|wxRESIZE_BORDER|wxSYSTEM_MENU|wxCLOSE_BOX
#define SYMBOL_ADVANCEDSTREAMLINES_TITLE _T("Advanced Streamlines")
#define SYMBOL_ADVANCEDSTREAMLINES_IDNAME ID_DIALOG
#define SYMBOL_ADVANCEDSTREAMLINES_SIZE wxSize(400, 300)
#define SYMBOL_ADVANCEDSTREAMLINES_POSITION wxDefaultPosition
#define ID_SLIDER 10001
#define ID_SLIDER1 10002
#define ID_SLIDER2 10003
#define ID_SLIDER3 10004
#define ID_SLIDER4 10005
#define ID_CHECKBOX 10006
#define ID_CHECKBOX1 10007
#define ID_BUTTON 10008
////@end control identifiers

/*!
 * AdvancedStreamlinesApp class declaration
 */
/*
class AdvancedStreamlinesApp: public wxApp
{    
    DECLARE_CLASS( AdvancedStreamlinesApp )
    DECLARE_EVENT_TABLE()

public:
    /// Constructor
    AdvancedStreamlinesApp();

    /// Initialises the application
    virtual bool OnInit();

    /// Called on exit
    virtual int OnExit();

////@begin AdvancedStreamlinesApp event handler declarations
////@end AdvancedStreamlinesApp event handler declarations

////@begin AdvancedStreamlinesApp member function declarations
////@end AdvancedStreamlinesApp member function declarations

////@begin AdvancedStreamlinesApp member variables
////@end AdvancedStreamlinesApp member variables
};
*/
/*!
 * Application instance declaration 
 */

////@begin declare app
//DECLARE_APP(AdvancedStreamlinesApp)
////@end declare app

/*!
 * AdvancedStreamlines class declaration
 */

class AdvancedStreamlines: public wxDialog
{    
//    DECLARE_DYNAMIC_CLASS( AdvancedStreamlines )
    DECLARE_EVENT_TABLE()

public:
    /// Constructors
//    AdvancedStreamlines( );
//    AdvancedStreamlines( wxWindow* parent, wxWindowID id = SYMBOL_ADVANCEDSTREAMLINES_IDNAME, const wxString& caption = SYMBOL_ADVANCEDSTREAMLINES_TITLE, const wxPoint& pos = SYMBOL_ADVANCEDSTREAMLINES_POSITION, const wxSize& size = SYMBOL_ADVANCEDSTREAMLINES_SIZE, long style = SYMBOL_ADVANCEDSTREAMLINES_STYLE );
    AdvancedStreamlines(VjObs_ptr veEngine, VE_XML::DOMDocumentManager* domManagerIn);
    void SendCommandsToXplorer( void );
    void SetCommInstance( VjObs_ptr veEngine );
    /// Creation
    bool Create( wxWindow* parent, wxWindowID id = SYMBOL_ADVANCEDSTREAMLINES_IDNAME, const wxString& caption = SYMBOL_ADVANCEDSTREAMLINES_TITLE, const wxPoint& pos = SYMBOL_ADVANCEDSTREAMLINES_POSITION, const wxSize& size = SYMBOL_ADVANCEDSTREAMLINES_SIZE, long style = SYMBOL_ADVANCEDSTREAMLINES_STYLE );

    /// Creates the controls and sizers
    void CreateControls();

////@begin AdvancedStreamlines event handler declarations
    void _onCheck(wxCommandEvent& event);
    void _onArrowCheck( wxCommandEvent& event);
    void _oniStepSlider(wxScrollEvent& event);
    void _onPropSlider(wxScrollEvent& event);
    void _onStepSlider(wxScrollEvent& event);
    void _onDiameterSlider(wxScrollEvent& event);
    void _onScaleSlider( wxScrollEvent& event);
    void _onParticleTrack(wxCommandEvent& event);
////@end AdvancedStreamlines event handler declarations

////@begin AdvancedStreamlines member function declarations

    /// Retrieves bitmap resources
    wxBitmap GetBitmapResource( const wxString& name );

    /// Retrieves icon resources
    wxIcon GetIconResource( const wxString& name );
////@end AdvancedStreamlines member function declarations

    /// Should we show tooltips?
    static bool ShowToolTips();

////@begin AdvancedStreamlines member variables
////@end AdvancedStreamlines member variables
protected:
   std::vector< VE_XML::Command* > commands;
   VjObs_ptr xplorerPtr;
   int cId, cIso_value, cMin, cMax, cSc;
   std::vector< long > commandInputs;
   DOMDocument* doc;
   VE_XML::DOMDocumentManager* domManager;
};

#endif
    // _ADVANCEDSTREAMLINES_H_
