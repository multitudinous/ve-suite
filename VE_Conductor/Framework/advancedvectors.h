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

#include "VE_Open/skel/VjObsC.h"
#include "VE_Conductor/VE_UI/UI_TransientDialog.h"
#include "VE_Conductor/Utilities/DualSlider.h"
#include <xercesc/dom/DOM.hpp>
#include <vector>

XERCES_CPP_NAMESPACE_USE
namespace VE_XML
{
   class Command;
   class DOMDocumentManager;
}

class wxSlider;
class wxCheckBox;


#define ID_DIALOG 10000
#define SYMBOL_ADVANCEDVECTORS_STYLE wxCAPTION|wxRESIZE_BORDER|wxSYSTEM_MENU|wxCLOSE_BOX
#define SYMBOL_ADVANCEDVECTORS_TITLE _T("Advanced Vectors")
#define SYMBOL_ADVANCEDVECTORS_IDNAME ID_DIALOG
#define SYMBOL_ADVANCEDVECTORS_SIZE wxSize(400, 300)
#define SYMBOL_ADVANCEDVECTORS_POSITION wxDefaultPosition


enum ADVANCED_VECTOR_IDS
{
   VECTOR_MAX_SLIDER,
   VECTOR_MIN_SLIDER,
   VECTOR_SCALE_SLIDER,
   VECTOR_RATIO_SLIDER,
   SCALAR_BY_VECTOR_CHK
};

class AdvancedVectors: public wxDialog
{    
//    DECLARE_DYNAMIC_CLASS( Vectors )
    DECLARE_EVENT_TABLE()

public:
   /// Constructors
   AdvancedVectors( );
   AdvancedVectors( wxWindow* parent, wxWindowID id = SYMBOL_ADVANCEDVECTORS_IDNAME, 
                  const wxString& caption = SYMBOL_ADVANCEDVECTORS_TITLE,
                  const wxPoint& pos = SYMBOL_ADVANCEDVECTORS_POSITION, 
                  const wxSize& size = SYMBOL_ADVANCEDVECTORS_SIZE,
                  long style = SYMBOL_ADVANCEDVECTORS_STYLE );
   bool Create( wxWindow* parent, wxWindowID id = SYMBOL_ADVANCEDVECTORS_IDNAME, 
              const wxString& caption = SYMBOL_ADVANCEDVECTORS_TITLE, 
              const wxPoint& pos = SYMBOL_ADVANCEDVECTORS_POSITION, 
              const wxSize& size = SYMBOL_ADVANCEDVECTORS_SIZE, 
              long style = SYMBOL_ADVANCEDVECTORS_STYLE );

   /// Creates the controls and sizers
   void CreateControls();

    ///Set the vector threshold
   ///\param range The vector threshold to set.
   void SetVectorThreshold(std::vector<double> range);

   ///Get the vector threshold
   ///\param range The vector threshold to fill in.
   void GetVectorThreshold(std::vector<double>& range);

   ///Set the vector scale.
   ///\param value The vector scale.
   void SetVectorScale(double value);

   ///Set the vector ratio.
   ///\param value The vector ratio.
   void SetVectorRatio(double value);

   ///Set the scale by magnitude flag.
   ///\param value Flag determining how to scale vectors.
   void SetScaleByMagFlag(bool value);

   ///Get the vector scale
   double GetVectorScale();

   ///Get the vector ratio.
   double GetVectorRatio();

   ///Get the scale by magnitude flag.
   bool GetScaleByMagFlag();

   /// Retrieves bitmap resources
   wxBitmap GetBitmapResource( const wxString& name );

   /// Retrieves icon resources
   wxIcon GetIconResource( const wxString& name );

   /// Should we show tooltips?
   static bool ShowToolTips();

protected:
   /// wxEVT_COMMAND_SLIDER_UPDATED event handler for ID_SLIDER
   void _onVectorMax( wxCommandEvent& event );

   /// wxEVT_COMMAND_SLIDER_UPDATED event handler for ID_SLIDER1
   void _onVectorMin( wxCommandEvent& event );

   /// wxEVT_COMMAND_SLIDER_UPDATED event handler for ID_SLIDER2
   void _onVectorScale( wxCommandEvent& event );

   /// wxEVT_COMMAND_SLIDER_UPDATED event handler for ID_SLIDER3
   void _onVectorRatio( wxCommandEvent& event );

   /// wxEVT_COMMAND_CHECKBOX_CLICKED event handler for ID_CHECKBOX
   void _onScalarByVectorMag( wxCommandEvent& event );

   VE_Conductor::GUI_Utilities::DualSlider* vectorRange;
   wxSlider* _vectorScaleSlider;///<Slider widget for vector scale.
   wxSlider*   _vectorRatioSlider;///<Slider widget for vector ratio.
   wxCheckBox* _scaleByMagCheck;///<Check box widget for scale option.
   
};

#endif
    // _VECTORS_H_
