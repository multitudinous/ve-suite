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

/*!
 * Includes
 */

////@begin includes
#include "VE_Open/skel/VjObsC.h"
#include "VE_Conductor/Framework/UI_TransientDialog.h"
#include "VE_Conductor/Framework/advancedvectors.h"
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

class wxRadioBox;
class wxRadioButton;
class wxCheckBox;
class wxSlider;
class wxButton;
////@end forward declarations

/*!
 * Control identifiers
 */

////@begin control identifiers
#define ID_DIALOG 10000
#define SYMBOL_VECTORS_STYLE wxCAPTION|wxRESIZE_BORDER|wxSYSTEM_MENU|wxCLOSE_BOX
#define SYMBOL_VECTORS_TITLE _T("Vectors")
#define SYMBOL_VECTORS_IDNAME ID_DIALOG
#define SYMBOL_VECTORS_SIZE wxSize(400, 300)
#define SYMBOL_VECTORS_POSITION wxDefaultPosition
/*#define ID_BUTTON 10006
#define ID_BUTTON1 10008*/

enum VECTOR_IDS
{
   ID_V_RADIOBOX,
   ID_V_RADIOBUTTON,
   ID_V_CHECKBOX,
   ID_V_RADIOBUTTON1,
   ID_V_CHECKBOX1,
   ID_V_SLIDER,
   VECTOR_DIR_RBOX,
   MULTIPLE_PREVECTOR_RBUTTON,
   MULTIPLE_PREVECTOR_CHK,
   SINGLE_PREVECTOR_RBUTTON,
   SINGLE_PREVECTOR_CHK,
   VECTOR_PLANE_SLIDER,
   ADD_VECTOR_PLANE_BUTTON,
   ADVANCED_VECTOR_BUTTON
};
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
    Vectors( );
    Vectors( wxWindow* parent, wxWindowID id = SYMBOL_VECTORS_IDNAME, const wxString& caption = SYMBOL_VECTORS_TITLE, const wxPoint& pos = SYMBOL_VECTORS_POSITION, const wxSize& size = SYMBOL_VECTORS_SIZE, long style = SYMBOL_VECTORS_STYLE );
//    Vectors(VjObs_ptr veEngine, VE_XML::DOMDocumentManager* domManagerIn);
    void SendCommandsToXplorer( void );
    void SetCommInstance( VjObs_ptr veEngine );
    /// Creation
    bool Create( wxWindow* parent, wxWindowID id = SYMBOL_VECTORS_IDNAME, const wxString& caption = SYMBOL_VECTORS_TITLE, const wxPoint& pos = SYMBOL_VECTORS_POSITION, const wxSize& size = SYMBOL_VECTORS_SIZE, long style = SYMBOL_VECTORS_STYLE );

    /// Creates the controls and sizers
    void CreateControls();

////@begin Vectors event handler declarations
    /// wxEVT_COMMAND_RADIOBOX_SELECTED event handler for ID_RADIOBOX
    void _onDirection( wxCommandEvent& event );

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
   wxRadioBox*    itemRadioBox5;
   wxRadioButton* itemRadioButton8;
   wxCheckBox*    itemCheckBox9;
   wxRadioButton* itemRadioButton11;
   wxCheckBox*    itemCheckBox12;
   wxSlider*      itemSlider14;
   wxButton*      itemButton16;
   wxButton*      itemButton17;
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
