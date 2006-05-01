/////////////////////////////////////////////////////////////////////////////
// Name:        vistab.h
// Purpose:     
// Author:      Jared Abodeely
// Modified by: 
// Created:     17/04/2006 16:26:41
// RCS-ID:      
// Copyright:   
// Licence:     
/////////////////////////////////////////////////////////////////////////////

// Generated by DialogBlocks, 17/04/2006 16:26:41

#ifndef _VISTAB_H_
#define _VISTAB_H_

/*!
 * Includes
 */

////@begin includes
#include "wx/toolbar.h"
#include "VE_Open/skel/VjObsC.h"
#include "VE_Conductor/VE_UI/UI_TransientDialog.h"
#include "VE_Conductor/Framework/vectors.h"
#include "VE_Conductor/Framework/contours.h"
#include "VE_Conductor/Framework/streamlines.h"
#include "VE_Conductor/Framework/isosurfaces.h"

#include <xercesc/dom/DOM.hpp>
#include <vector>
////@end includes
XERCES_CPP_NAMESPACE_USE

class TextureBasedToolBar;

namespace VE_XML
{
   class Command;
   class DOMDocumentManager;
}
/*!
 * Forward declarations
 */

////@begin forward declarations
enum VISTAB_IDS
{
   CONTOUR_BUTTON,
   VECTOR_BUTTON,
   STREAMLINE_BUTTON,
   ISOSURFACE_BUTTON,
   TEXTURE_BASED_BUTTON
};
////@end forward declarations

/*!
 * Control identifiers
 */

////@begin control identifiers
#define ID_DIALOG 10000
#define SYMBOL_VISTAB_STYLE wxCAPTION|wxRESIZE_BORDER|wxSYSTEM_MENU|wxCLOSE_BOX
#define SYMBOL_VISTAB_TITLE _T("VE-Suite")
#define SYMBOL_VISTAB_IDNAME ID_DIALOG
#define SYMBOL_VISTAB_SIZE wxSize(400, 300)
#define SYMBOL_VISTAB_POSITION wxDefaultPosition
#define ID_TOOLBAR 10001
#define ID_TOOL 10002
#define ID_TOOL1 10003
#define ID_TOOL2 10004
#define ID_TOOL3 10005
#define ID_TOOL4 10006
#define ID_TOOL5 10007
#define ID_COMBOBOX 10010
#define ID_LISTBOX 10011
#define ID_LISTBOX1 10012
#define ID_SLIDER 10013
#define ID_SLIDER1 10014
#define ID_BUTTON 10015
#define ID_COMBOBOX1 10016
////@end control identifiers

/*!
 * Compatibility
 */

#ifndef wxCLOSE_BOX
#define wxCLOSE_BOX 0x1000
#endif

/*!
 * Vistab class declaration
 */
/*
class Vistab: public wxDialog
{    
    DECLARE_DYNAMIC_CLASS( Vistab )
    DECLARE_EVENT_TABLE()

public:
    /// Constructors
    Vistab( );
    Vistab( wxWindow* parent, wxWindowID id = SYMBOL_VISTAB_IDNAME, const wxString& caption = SYMBOL_VISTAB_TITLE, const wxPoint& pos = SYMBOL_VISTAB_POSITION, const wxSize& size = SYMBOL_VISTAB_SIZE, long style = SYMBOL_VISTAB_STYLE );

    /// Creation
    bool Create( wxWindow* parent, wxWindowID id = SYMBOL_VISTAB_IDNAME, const wxString& caption = SYMBOL_VISTAB_TITLE, const wxPoint& pos = SYMBOL_VISTAB_POSITION, const wxSize& size = SYMBOL_VISTAB_SIZE, long style = SYMBOL_VISTAB_STYLE );

    /// Creates the controls and sizers
    void CreateControls();

////@begin Vistab event handler declarations

////@end Vistab event handler declarations

////@begin Vistab member function declarations

    /// Retrieves bitmap resources
    wxBitmap GetBitmapResource( const wxString& name );

    /// Retrieves icon resources
    wxIcon GetIconResource( const wxString& name );
////@end Vistab member function declarations

    /// Should we show tooltips?
    static bool ShowToolTips();

////@begin Vistab member variables
////@end Vistab member variables
};

/*!
 * Vistab class declaration
 */
/*
class Vistab: public wxFrame
{    
    DECLARE_CLASS( Vistab )
    DECLARE_EVENT_TABLE()

public:
    /// Constructors
    Vistab( );
    Vistab( wxWindow* parent, wxWindowID id = SYMBOL_VISTAB_IDNAME, const wxString& caption = SYMBOL_VISTAB_TITLE, const wxPoint& pos = SYMBOL_VISTAB_POSITION, const wxSize& size = SYMBOL_VISTAB_SIZE, long style = SYMBOL_VISTAB_STYLE );

    /// Creation
    bool Create( wxWindow* parent, wxWindowID id = SYMBOL_VISTAB_IDNAME, const wxString& caption = SYMBOL_VISTAB_TITLE, const wxPoint& pos = SYMBOL_VISTAB_POSITION, const wxSize& size = SYMBOL_VISTAB_SIZE, long style = SYMBOL_VISTAB_STYLE );

    /// Creates the controls and sizers
    void CreateControls();

////@begin Vistab event handler declarations
////@end Vistab event handler declarations

////@begin Vistab member function declarations
    /// Retrieves bitmap resources
    wxBitmap GetBitmapResource( const wxString& name );

    /// Retrieves icon resources
    wxIcon GetIconResource( const wxString& name );
////@end Vistab member function declarations

    /// Should we show tooltips?
    static bool ShowToolTips();

////@begin Vistab member variables
////@end Vistab member variables
};

/*!
 * Vistab class declaration
 */

class Vistab: public wxDialog
{    
//    DECLARE_DYNAMIC_CLASS( Vistab )
    DECLARE_EVENT_TABLE()

public:
    /// Constructors
//    Vistab( );
//    Vistab( wxWindow* parent, wxWindowID id = SYMBOL_VISTAB_IDNAME, const wxString& caption = SYMBOL_VISTAB_TITLE, const wxPoint& pos = SYMBOL_VISTAB_POSITION, const wxSize& size = SYMBOL_VISTAB_SIZE, long style = SYMBOL_VISTAB_STYLE );
    Vistab(VjObs_ptr veEngine, VE_XML::DOMDocumentManager* domManagerIn);
    void SendCommandsToXplorer( void );
    void SetCommInstance( VjObs_ptr veEngine );
    /// Creation
    bool Create( wxWindow* parent, wxWindowID id = SYMBOL_VISTAB_IDNAME, const wxString& caption = SYMBOL_VISTAB_TITLE, const wxPoint& pos = SYMBOL_VISTAB_POSITION, const wxSize& size = SYMBOL_VISTAB_SIZE, long style = SYMBOL_VISTAB_STYLE );

    /// Creates the controls and sizers
    void CreateControls();

////@begin Vistab event handler declarations
    void _onContour(wxCommandEvent& );
    void _onVector(wxCommandEvent& );
    void _onStreamline(wxCommandEvent& );
    void _onIsosurface(wxCommandEvent& );
    void _onTextureBased(wxCommandEvent& );
////@end Vistab event handler declarations

////@begin Vistab member function declarations

    /// Retrieves bitmap resources
    wxBitmap GetBitmapResource( const wxString& name );

    /// Retrieves icon resources
    wxIcon GetIconResource( const wxString& name );
////@end Vistab member function declarations

    /// Should we show tooltips?
    static bool ShowToolTips();

protected:
   std::vector< VE_XML::Command* > commands;
   VjObs_ptr xplorerPtr;
   int cId, cIso_value, cMin, cMax, cSc;
   std::vector< long > commandInputs;
   DOMDocument* doc;
   VE_XML::DOMDocumentManager* domManager;
   std::string dataValueName;

   Vectors* vector;
   Contours* contour;
   Streamlines* streamline;
   Isosurfaces* isosurface;
   TextureBasedToolBar* _tbTools;///<TextureBasedToolBar.
////@begin Vistab member variables
////@end Vistab member variables
};

#endif
    // _VE-SUITE_H_
