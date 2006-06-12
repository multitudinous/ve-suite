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

#ifndef _STREAMLINES_H_
#define _STREAMLINES_H_

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
XERCES_CPP_NAMESPACE_USE
namespace VE_XML
{
   class Command;
   class DOMDocumentManager;
   class DataValuePair;
}

class wxRadioBox;
class wxSlider;
class wxButton;
////@end forward declarations

/*!
 * Control identifiers
 */

////@begin control identifiers
#define ID_DIALOG 10000
#define SYMBOL_STREAMLINES_STYLE wxCAPTION|wxRESIZE_BORDER|wxSYSTEM_MENU|wxCLOSE_BOX
#define SYMBOL_STREAMLINES_TITLE _T("Streamlines")
#define SYMBOL_STREAMLINES_IDNAME ID_DIALOG
#define SYMBOL_STREAMLINES_SIZE wxSize(400, 300)
#define SYMBOL_STREAMLINES_POSITION wxDefaultPosition
/*#define ID_RADIOBOX 10001
#define ID_RADIOBOX1 10002
#define ID_RADIOBOX2 10003
#define ID_SLIDER 10004
#define ID_SLIDER1 10005
#define ID_BUTTON 10006
#define ID_BUTTON1 10007*/

enum STREAMLINE_IDS
{
   CURSOR_RBOX,
   DIRECTION_RBOX,
   INTEGRATION_DIR_RBOX,
   PLANE_SIZE_SLIDER,
   NUMBER_PTS_SLIDER,
   ADVANCED_STREAMLINE_BUTTON,
   COMPUTE_STREAMLINE_BUTTON
};


class Streamlines: public wxDialog
{    
//    DECLARE_DYNAMIC_CLASS( Streamlines )
    DECLARE_EVENT_TABLE()

public:
    /// Constructors
    Streamlines( );
    Streamlines( wxWindow* parent, wxWindowID id = SYMBOL_STREAMLINES_IDNAME, const wxString& caption = SYMBOL_STREAMLINES_TITLE, const wxPoint& pos = SYMBOL_STREAMLINES_POSITION, const wxSize& size = SYMBOL_STREAMLINES_SIZE, long style = SYMBOL_STREAMLINES_STYLE );
//    Streamlines(VjObs_ptr veEngine, VE_XML::DOMDocumentManager* domManagerIn);
    void SendCommandsToXplorer( void );
    void SetCommInstance( VjObs_ptr veEngine );
    /// Creation
    bool Create( wxWindow* parent, wxWindowID id = SYMBOL_STREAMLINES_IDNAME, const wxString& caption = SYMBOL_STREAMLINES_TITLE, const wxPoint& pos = SYMBOL_STREAMLINES_POSITION, const wxSize& size = SYMBOL_STREAMLINES_SIZE, long style = SYMBOL_STREAMLINES_STYLE );

    /// Creates the controls and sizers
    void CreateControls();



    /// Retrieves bitmap resources
    wxBitmap GetBitmapResource( const wxString& name );

    /// Retrieves icon resources
    wxIcon GetIconResource( const wxString& name );
////@end Streamlines member function declarations

    /// Should we show tooltips?
    static bool ShowToolTips();

////@begin Streamlines member variables


  
   wxButton*   itemButton14;
////@end Streamlines member variables
protected: 
   void _onCursorSelect(wxCommandEvent& );
   void _onDirection(wxCommandEvent& );
   void _onIntegrateDir(wxCommandEvent& );
   void _onPointsSlider(wxScrollEvent& );
   void _onAdvanced(wxCommandEvent& );
   void _onSizeSlider(wxScrollEvent& event);
   void _onCompute(wxCommandEvent& event);

   ///Update the streamlines information
   void _updateStreamlineInformation();
   ///Update the advanced settings
   void _updateAdvancedSettings();

   wxRadioBox* _cursorRBox;
   wxRadioBox* _directionRBox;
   wxRadioBox* _integrationRBox;
   wxSlider* _sizeSlider;
   wxSlider* _nPointsSlider;
   wxButton* itemButton13;

   std::vector<VE_XML::DataValuePair*> _advancedSettings;///<The advanced settings.
   std::vector<VE_XML::DataValuePair*> _streamlineInformation;///<The streamline setting data

   double _streamSize;///<The size of the streamlines.
   unsigned int _nPointsPerPlane;///<The number of seed points.
   std::string _streamlineDirection;///<Store the value of the direction.
   std::string _cursorType;///<The contour type.
   std::string _integrationDirection;///<Single or Multiple planes.

   double _lastIntegrationStepSize;///<Integration step size from advanced settings.
   double _lastPropagationSize;///<Propagation size from advanced settings.
   double _lastLineDiameter;///<Line diameter from advanced settings.
   double _lastSphereArrowParticleSize;///<Sphere arrow particles from advanced settings.
   double _lastStep;///<Step from advanced settings.
   bool _lastSeedPtFlag;///<Seed pt flat from advanced settings.
   bool _lastStreamArrow;///<Stream arrow from advanced settings.

   int cId, cIso_value, cMin, cMax, cSc;
   std::vector< long > commandInputs;
   DOMDocument* doc;
   VE_XML::DOMDocumentManager* domManager;

   //AdvancedStreamlines* adStreamline;
};

#endif
    // _STREAMLINES_H_
