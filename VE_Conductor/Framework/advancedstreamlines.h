/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * File:          $RCSfile: GlobalParamDialog.h,v $
 * Date modified: $Date: 2006-03-23 17:47:31 -0600 (Thu, 23 Mar 2006) $
 * Version:       $Rev: 3957 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

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
XERCES_CPP_NAMESPACE_USE
namespace VE_XML
{
   class Command;
   class DOMDocumentManager;
}

class wxSlider;
class wxCheckBox;
class wxButton;
////@end forward declarations

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
/*#define ID_SLIDER 10001
#define ID_SLIDER1 10002
#define ID_SLIDER2 10003
#define ID_SLIDER3 10004
#define ID_SLIDER4 10005
#define ID_CHECKBOX 10006
#define ID_CHECKBOX1 10007
#define ID_BUTTON 10008*/

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
////@end control identifiers


class AdvancedStreamlines: public wxDialog
{    
//    DECLARE_DYNAMIC_CLASS( AdvancedStreamlines )
    DECLARE_EVENT_TABLE()

public:
    /// Constructors
    AdvancedStreamlines( );
    AdvancedStreamlines( wxWindow* parent, wxWindowID id = SYMBOL_ADVANCEDSTREAMLINES_IDNAME, const wxString& caption = SYMBOL_ADVANCEDSTREAMLINES_TITLE, const wxPoint& pos = SYMBOL_ADVANCEDSTREAMLINES_POSITION, const wxSize& size = SYMBOL_ADVANCEDSTREAMLINES_SIZE, long style = SYMBOL_ADVANCEDSTREAMLINES_STYLE );

    void SendCommandsToXplorer( void );
    void SetCommInstance( VjObs_ptr veEngine );
    /// Creation
    bool Create( wxWindow* parent, wxWindowID id = SYMBOL_ADVANCEDSTREAMLINES_IDNAME, const wxString& caption = SYMBOL_ADVANCEDSTREAMLINES_TITLE, const wxPoint& pos = SYMBOL_ADVANCEDSTREAMLINES_POSITION, const wxSize& size = SYMBOL_ADVANCEDSTREAMLINES_SIZE, long style = SYMBOL_ADVANCEDSTREAMLINES_STYLE );

    /// Creates the controls and sizers
    void CreateControls();

   ///Set the propagation slider value
   ///\param value The size
   void SetPropagationSize(double value);

   ///Set the integration step size.
   ///\param value The size
   void SetIntegrationStepSize(double value);

   ///Set the step(?) size
   ///\param value The size
   void SetStep(double value);
  
   ///Set the sphere/arrow/particle(?) size
   ///\param value The size
   void SetSphereArrowParticleSize(double value);

   ///Set the stream arrow flag
   ///\param value The stream arrow flag
   void SetStreamArrow(bool value);

   ///Set the use last seed pt flag
   ///\param value The use last seed pt flag
   void SetUseLastSeedPt(bool value);

   ///Set the line diameter
   ///\param value The size
   void SetLineDiameter(double value);

   ///Get the propagation step size.
   double GetPropagationSize();
   ///Get the integration step size
   double GetIntegrationStepSize();
   ///Get the (?)step
   double GetStep();
   ///Get the Sphere/Arrow/Particle(?) size
   double GetSphereArrowParticleSize();

   ///Get the line diameter.
   double GetLineDiameter();

   ///Get the stream arrow.
   bool GetStreamArrow();

   ///Get the use last seed point flag
   bool GetUseLastSeedPoint();


   /// Retrieves bitmap resources
   wxBitmap GetBitmapResource( const wxString& name );

   /// Retrieves icon resources
   wxIcon GetIconResource( const wxString& name );

   /// Should we show tooltips?
   static bool ShowToolTips();

protected:
   void _onLastSeedPtCheck(wxCommandEvent& event);
   void _onArrowCheck( wxCommandEvent& event);
   void _oniStepSlider(wxScrollEvent& event);
   void _onPropSlider(wxScrollEvent& event);
   void _onStepSlider(wxScrollEvent& event);
   void _onDiameterSlider(wxScrollEvent& event);
   void _onScaleSlider( wxScrollEvent& event);
   void _onParticleTrack(wxCommandEvent& event);

   double _propagationTime;///<The propagation time.
   double _integrationStepSize;///<The integration step size
   double _stepSize;///<The (?) step size.
   double _sphereArrowParticleSize;///<The (?)size.
   double _lineDiameter;///<The line diameter.

   bool _useLastSeedPoint;///<Use last seed point.
   bool _useStreamArrows;///<Use stream arrows.

   wxSlider* _propagationSlider;  
   wxSlider* _integrationSlider; 
   wxSlider* _stepSlider; 
   wxSlider* _sphereArrowParticleSlider; 
   wxSlider* _diameterSlider; 
   wxCheckBox* _lastSeedPtCheck;
   wxCheckBox* _streamArrowCheck;

   //wxButton*   itemButton29;


   std::vector< VE_XML::Command* > commands;
   VjObs_ptr xplorerPtr;
   std::vector< long > commandInputs;
   DOMDocument* doc;
   VE_XML::DOMDocumentManager* domManager;
};

#endif
    // _ADVANCEDSTREAMLINES_H_
