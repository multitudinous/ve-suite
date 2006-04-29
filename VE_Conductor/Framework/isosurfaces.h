/////////////////////////////////////////////////////////////////////////////
// Name:        isosurfaces.h
// Purpose:     
// Author:      Jared Abodeely
// Modified by: 
// Created:     Fri 21 Apr 2006 10:14:42 CDT
// RCS-ID:      
// Copyright:   
// Licence:     
/////////////////////////////////////////////////////////////////////////////

#ifndef _ISOSURFACES_H_
#define _ISOSURFACES_H_

#if defined(__GNUG__) && !defined(NO_GCC_PRAGMA)
#pragma interface "isosurfaces.h"
#endif

/*!
 * Includes
 */

////@begin includes
#include "VE_Open/skel/VjObsC.h"
#include "VE_Conductor/VE_UI/UI_TransientDialog.h"
//#include "VE_Conductor/Framework/advancedisosurfaces.h"
#include <xercesc/dom/DOM.hpp>
#include <vector>
////@end includes

/*!
 * Forward declarations
 */

////@begin forward declarations

enum ISOSURFACE_IDS
{

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
#define SYMBOL_ISOSURFACES_STYLE wxCAPTION|wxRESIZE_BORDER|wxSYSTEM_MENU|wxCLOSE_BOX
#define SYMBOL_ISOSURFACES_TITLE _("Isosurfaces")
#define SYMBOL_ISOSURFACES_IDNAME ID_DIALOG
#define SYMBOL_ISOSURFACES_SIZE wxSize(400, 300)
#define SYMBOL_ISOSURFACES_POSITION wxDefaultPosition
#define ID_RADIOBUTTON 10001
#define ID_CHECKBOX 10002
#define ID_SLIDER 10003
#define ID_BUTTON 10004
#define ID_BUTTON1 10005
////@end control identifiers

/*!
 * IsosurfacesApp class declaration
 */
/*
class IsosurfacesApp: public wxApp
{    
    DECLARE_CLASS( IsosurfacesApp )
    DECLARE_EVENT_TABLE()

public:
    /// Constructor
    IsosurfacesApp();

    /// Initialises the application
    virtual bool OnInit();

    /// Called on exit
    virtual int OnExit();

////@begin IsosurfacesApp event handler declarations
////@end IsosurfacesApp event handler declarations

////@begin IsosurfacesApp member function declarations
////@end IsosurfacesApp member function declarations

////@begin IsosurfacesApp member variables
////@end IsosurfacesApp member variables
};
*/
/*!
 * Application instance declaration 
 */

////@begin declare app
//DECLARE_APP(IsosurfacesApp)
////@end declare app

/*!
 * Isosurfaces class declaration
 */

class Isosurfaces: public wxDialog
{    
//    DECLARE_DYNAMIC_CLASS( Isosurfaces )
    DECLARE_EVENT_TABLE()

public:
    /// Constructors
//    Isosurfaces( );
//    Isosurfaces( wxWindow* parent, wxWindowID id = SYMBOL_ISOSURFACES_IDNAME, const wxString& caption = SYMBOL_ISOSURFACES_TITLE, const wxPoint& pos = SYMBOL_ISOSURFACES_POSITION, const wxSize& size = SYMBOL_ISOSURFACES_SIZE, long style = SYMBOL_ISOSURFACES_STYLE );
    Isosurfaces(VjObs_ptr veEngine, VE_XML::DOMDocumentManager* domManagerIn);
    void SendCommandsToXplorer( void );
    void SetCommInstance( VjObs_ptr veEngine );
    /// Creation
    bool Create( wxWindow* parent, wxWindowID id = SYMBOL_ISOSURFACES_IDNAME, const wxString& caption = SYMBOL_ISOSURFACES_TITLE, const wxPoint& pos = SYMBOL_ISOSURFACES_POSITION, const wxSize& size = SYMBOL_ISOSURFACES_SIZE, long style = SYMBOL_ISOSURFACES_STYLE );

    /// Creates the controls and sizers
    void CreateControls();

////@begin Isosurfaces event handler declarations

////@end Isosurfaces event handler declarations

////@begin Isosurfaces member function declarations

    /// Retrieves bitmap resources
    wxBitmap GetBitmapResource( const wxString& name );

    /// Retrieves icon resources
    wxIcon GetIconResource( const wxString& name );
////@end Isosurfaces member function declarations

    /// Should we show tooltips?
    static bool ShowToolTips();

////@begin Isosurfaces member variables
////@end Isosurfaces member variables
protected:
   std::vector< VE_XML::Command* > commands;
   VjObs_ptr xplorerPtr;
   int cId, cIso_value, cMin, cMax, cSc;
   std::vector< long > commandInputs;
   DOMDocument* doc;
   VE_XML::DOMDocumentManager* domManager;
};

#endif
    // _ISOSURFACES_H_
