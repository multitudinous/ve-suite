/////////////////////////////////////////////////////////////////////////////
// Name:        advancedcontours.h
// Purpose:     
// Author:      Jared Abodeely
// Modified by: 
// Created:     Thu 20 Apr 2006 20:30:15 CDT
// RCS-ID:      
// Copyright:   
// Licence:     
/////////////////////////////////////////////////////////////////////////////

#ifndef _ADVANCEDCONTOURS_H_
#define _ADVANCEDCONTOURS_H_

#if defined(__GNUG__) && !defined(NO_GCC_PRAGMA)
#pragma interface "advancedcontours.h"
#endif

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
#define SYMBOL_ADVANCEDCONTOURS_STYLE wxCAPTION|wxRESIZE_BORDER|wxSYSTEM_MENU|wxCLOSE_BOX
#define SYMBOL_ADVANCEDCONTOURS_TITLE _("AdvancedContours")
#define SYMBOL_ADVANCEDCONTOURS_IDNAME ID_DIALOG
#define SYMBOL_ADVANCEDCONTOURS_SIZE wxSize(400, 300)
#define SYMBOL_ADVANCEDCONTOURS_POSITION wxDefaultPosition
#define ID_SLIDER 10001
#define ID_SLIDER1 10002
#define ID_SLIDER2 10003
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
//    AdvancedContours( );
//    AdvancedContours( wxWindow* parent, wxWindowID id = SYMBOL_ADVANCEDCONTOURS_IDNAME, const wxString& caption = SYMBOL_ADVANCEDCONTOURS_TITLE, const wxPoint& pos = SYMBOL_ADVANCEDCONTOURS_POSITION, const wxSize& size = SYMBOL_ADVANCEDCONTOURS_SIZE, long style = SYMBOL_ADVANCEDCONTOURS_STYLE );
    AdvancedContours(VjObs_ptr veEngine, VE_XML::DOMDocumentManager* domManagerIn);
    void SendCommandsToXplorer( void );
    void SetCommInstance( VjObs_ptr veEngine );
    /// Creation
    bool Create( wxWindow* parent, wxWindowID id = SYMBOL_ADVANCEDCONTOURS_IDNAME, const wxString& caption = SYMBOL_ADVANCEDCONTOURS_TITLE, const wxPoint& pos = SYMBOL_ADVANCEDCONTOURS_POSITION, const wxSize& size = SYMBOL_ADVANCEDCONTOURS_SIZE, long style = SYMBOL_ADVANCEDCONTOURS_STYLE );

    /// Creates the controls and sizers
    void CreateControls();

////@begin AdvancedContours event handler declarations

////@end AdvancedContours event handler declarations

////@begin AdvancedContours member function declarations

    /// Retrieves bitmap resources
    wxBitmap GetBitmapResource( const wxString& name );

    /// Retrieves icon resources
    wxIcon GetIconResource( const wxString& name );
////@end AdvancedContours member function declarations

    /// Should we show tooltips?
    static bool ShowToolTips();

////@begin AdvancedContours member variables
////@end AdvancedContours member variables
protected:
   std::vector< VE_XML::Command* > commands;
   VjObs_ptr xplorerPtr;
   int cId, cIso_value, cMin, cMax, cSc;
   std::vector< long > commandInputs;
   DOMDocument* doc;
   VE_XML::DOMDocumentManager* domManager;
};

#endif
    // _ADVANCEDCONTOURS_H_
