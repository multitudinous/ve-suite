/////////////////////////////////////////////////////////////////////////////
// Name:        vectors.h
// Purpose:     
// Author:      Jared Abodeely
// Modified by: 
// Created:     Thu 20 Apr 2006 21:08:23 CDT
// RCS-ID:      
// Copyright:   
// Licence:     
/////////////////////////////////////////////////////////////////////////////

#ifndef _VECTORS_H_
#define _VECTORS_H_

#if defined(__GNUG__) && !defined(NO_GCC_PRAGMA)
#pragma interface "vectors.h"
#endif

/*!
 * Includes
 */

////@begin includes
#include "VE_Open/skel/VjObsC.h"
#include "VE_Conductor/VE_UI/UI_TransientDialog.h"
#include "VE_Conductor/Framework/advancedvectors.h"
#include <xercesc/dom/DOM.hpp>
#include <vector>
////@end includes

/*!
 * Forward declarations
 */

////@begin forward declarations
enum VECTOR_IDS
{
   ADVANCED_VECTOR_BUTTON
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
#define SYMBOL_VECTORS_STYLE wxCAPTION|wxRESIZE_BORDER|wxSYSTEM_MENU|wxCLOSE_BOX
#define SYMBOL_VECTORS_TITLE _("Vectors")
#define SYMBOL_VECTORS_IDNAME ID_DIALOG
#define SYMBOL_VECTORS_SIZE wxSize(400, 300)
#define SYMBOL_VECTORS_POSITION wxDefaultPosition
#define ID_RADIOBOX 10002
#define ID_RADIOBUTTON 10001
#define ID_CHECKBOX 10003
#define ID_RADIOBUTTON1 10004
#define ID_CHECKBOX1 10005
#define ID_SLIDER 10007
#define ID_BUTTON 10006
#define ID_BUTTON1 10008
////@end control identifiers

/*!
 * VectorsApp class declaration
 */
/*
class VectorsApp: public wxApp
{    
//    DECLARE_CLASS( VectorsApp )
    DECLARE_EVENT_TABLE()

public:
    /// Constructor
    VectorsApp();

    /// Initialises the application
    virtual bool OnInit();

    /// Called on exit
    virtual int OnExit();

////@begin VectorsApp event handler declarations
////@end VectorsApp event handler declarations

////@begin VectorsApp member function declarations
////@end VectorsApp member function declarations

////@begin VectorsApp member variables
////@end VectorsApp member variables
};
*/
/*!
 * Application instance declaration 
 */

////@begin declare app
/////////////////////////////////DECLARE_APP(VectorsApp)
////@end declare app

/*!
 * Vectors class declaration
 */

class Vectors: public wxDialog
{    
//    DECLARE_DYNAMIC_CLASS( Vectors )
    DECLARE_EVENT_TABLE()

public:
    /// Constructors
//    Vectors( );
//    Vectors( wxWindow* parent, wxWindowID id = SYMBOL_VECTORS_IDNAME, const wxString& caption = SYMBOL_VECTORS_TITLE, const wxPoint& pos = SYMBOL_VECTORS_POSITION, const wxSize& size = SYMBOL_VECTORS_SIZE, long style = SYMBOL_VECTORS_STYLE );
    Vectors(VjObs_ptr veEngine, VE_XML::DOMDocumentManager* domManagerIn);
    void SendCommandsToXplorer( void );
    void SetCommInstance( VjObs_ptr veEngine );
    /// Creation
    bool Create( wxWindow* parent, wxWindowID id = SYMBOL_VECTORS_IDNAME, const wxString& caption = SYMBOL_VECTORS_TITLE, const wxPoint& pos = SYMBOL_VECTORS_POSITION, const wxSize& size = SYMBOL_VECTORS_SIZE, long style = SYMBOL_VECTORS_STYLE );

    /// Creates the controls and sizers
    void CreateControls();

////@begin Vectors event handler declarations
    void _onAdvanced(wxCommandEvent& );
////@end Vectors event handler declarations

////@begin Vectors member function declarations
   
    /// Retrieves bitmap resources
    wxBitmap GetBitmapResource( const wxString& name );

    /// Retrieves icon resources
    wxIcon GetIconResource( const wxString& name );
////@end Vectors member function declarations

    /// Should we show tooltips?
    static bool ShowToolTips();

////@begin Vectors member variables
////@end Vectors member variables
protected:
   std::vector< VE_XML::Command* > commands;
   VjObs_ptr xplorerPtr;
   int cId, cIso_value, cMin, cMax, cSc;
   std::vector< long > commandInputs;
   DOMDocument* doc;
   VE_XML::DOMDocumentManager* domManager;

   AdvancedVectors* adVector;
};

#endif
    // _VECTORS_H_
