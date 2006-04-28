/////////////////////////////////////////////////////////////////////////////
// Name:        advancedvectors.h
// Purpose:     
// Author:      Jared Abodeely
// Modified by: 
// Created:     Thu 20 Apr 2006 21:08:23 CDT
// RCS-ID:      
// Copyright:   
// Licence:     
/////////////////////////////////////////////////////////////////////////////

#ifndef _ADVANCEDVECTORS_H_
#define _ADVANCEDVECTORS_H_

#if defined(__GNUG__) && !defined(NO_GCC_PRAGMA)
#pragma interface "advancedvectors.h"
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
#define SYMBOL_ADVANCEDVECTORS_STYLE wxCAPTION|wxRESIZE_BORDER|wxSYSTEM_MENU|wxCLOSE_BOX
#define SYMBOL_ADVANCEDVECTORS_TITLE _("Advanced Vectors")
#define SYMBOL_ADVANCEDVECTORS_IDNAME ID_DIALOG
#define SYMBOL_ADVANCEDVECTORS_SIZE wxSize(400, 300)
#define SYMBOL_ADVANCEDVECTORS_POSITION wxDefaultPosition
#define ID_SLIDER 10001
#define ID_SLIDER1 10002
#define ID_SLIDER2 10003
#define ID_SLIDER3 10004
#define ID_CHECKBOX 10005
////@end control identifiers

/*!
 * AdvancedVectorsApp class declaration
 */
/*
class AdvancedVectorsApp: public wxApp
{    
//    DECLARE_CLASS( AdvancedVectorsApp )
    DECLARE_EVENT_TABLE()

public:
    /// Constructor
    AdvancedVectorsApp();

    /// Initialises the application
    virtual bool OnInit();

    /// Called on exit
    virtual int OnExit();

////@begin AdvancedVectorsApp event handler declarations
////@end AdvancedVectorsApp event handler declarations

////@begin AdvancedVectorsApp member function declarations
////@end AdvancedVectorsApp member function declarations

////@begin AdvancedVectorsApp member variables
////@end AdvancedVectorsApp member variables


};
*/
/*!
 * Application instance declaration 
 */

////@begin declare app
/////////////////////////////////DECLARE_APP(AdvancedVectorsApp)
////@end declare app

/*!
 * AdvancedVectors class declaration
 */

class AdvancedVectors: public wxDialog
{    
//    DECLARE_DYNAMIC_CLASS( Vectors )
    DECLARE_EVENT_TABLE()

public:
    /// Constructors
//    AdvancedVectors( );
//    AdvancedVectors( wxWindow* parent, wxWindowID id = SYMBOL_ADVANCEDVECTORS_IDNAME, const wxString& caption = SYMBOL_ADVANCEDVECTORS_TITLE, const wxPoint& pos = SYMBOL_ADVANCEDVECTORS_POSITION, const wxSize& size = SYMBOL_ADVANCEDVECTORS_SIZE, long style = SYMBOL_ADVANCEDVECTORS_STYLE );
    AdvancedVectors(VjObs_ptr veEngine, VE_XML::DOMDocumentManager* domManagerIn);
    void SendCommandsToXplorer( void );
    void SetCommInstance( VjObs_ptr veEngine );
    /// Creation
    bool Create( wxWindow* parent, wxWindowID id = SYMBOL_ADVANCEDVECTORS_IDNAME, const wxString& caption = SYMBOL_ADVANCEDVECTORS_TITLE, const wxPoint& pos = SYMBOL_ADVANCEDVECTORS_POSITION, const wxSize& size = SYMBOL_ADVANCEDVECTORS_SIZE, long style = SYMBOL_ADVANCEDVECTORS_STYLE );

    /// Creates the controls and sizers
    void CreateControls();

////@begin AdvancedVectors event handler declarations

////@end AdvancedVectors event handler declarations

////@begin AdvancedVectors member function declarations

    /// Retrieves bitmap resources
    wxBitmap GetBitmapResource( const wxString& name );

    /// Retrieves icon resources
    wxIcon GetIconResource( const wxString& name );
////@end AdvancedVectors member function declarations

    /// Should we show tooltips?
    static bool ShowToolTips();

////@begin AdvancedVectors member variables
////@end AdvancedVectors member variables

protected:
   std::vector< VE_XML::Command* > commands;
   VjObs_ptr xplorerPtr;
   int cId, cIso_value, cMin, cMax, cSc;
   std::vector< long > commandInputs;
   DOMDocument* doc;
   VE_XML::DOMDocumentManager* domManager;
};

#endif
    // _VECTORS_H_
