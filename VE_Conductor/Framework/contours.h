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
#ifndef _CONTOURS_H_
#define _CONTOURS_H_


/*!
 * Includes
 */

////@begin includes
#include "VE_Open/skel/VjObsC.h"
#include "VE_Conductor/Framework/UI_TransientDialog.h"
#include "VE_Conductor/Framework/advancedcontours.h"
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
#define SYMBOL_CONTOURS_STYLE wxCAPTION|wxRESIZE_BORDER|wxSYSTEM_MENU|wxCLOSE_BOX
#define SYMBOL_CONTOURS_TITLE _T("Contours")
#define SYMBOL_CONTOURS_IDNAME ID_DIALOG
#define SYMBOL_CONTOURS_SIZE wxSize(400, 300)
#define SYMBOL_CONTOURS_POSITION wxDefaultPosition


enum CONTOUR_IDS
{
   CONTOUR_DIR_RBOX,
   MULTIPLE_PRECONTOUR_RBUTTON,
   MULTIPLE_PRECONTOUR_CHK,
   SINGLE_PRECONTOUR_RBUTTON,
   SINGLE_PRECONTOUR_CHK,
   CONTOUR_PLANE_SLIDER,
   ADD_CONTOUR_PLANE_BUTTON,
   ADVANCED_CONTOUR_BUTTON
};

#ifndef wxCLOSE_BOX
#define wxCLOSE_BOX 0x1000
#endif



class Contours: public wxDialog
{    
    DECLARE_EVENT_TABLE()

public:
    /// Constructors
   Contours( );
   Contours( wxWindow* parent, wxWindowID id = SYMBOL_CONTOURS_IDNAME, 
            const wxString& caption = SYMBOL_CONTOURS_TITLE, 
            const wxPoint& pos = SYMBOL_CONTOURS_POSITION, 
            const wxSize& size = SYMBOL_CONTOURS_SIZE, 
            long style = SYMBOL_CONTOURS_STYLE, std::string type="SCALAR" );

   virtual ~Contours();   
   
   /// Creation
   bool Create( wxWindow* parent, wxWindowID id = SYMBOL_CONTOURS_IDNAME, const wxString& caption = SYMBOL_CONTOURS_TITLE, const wxPoint& pos = SYMBOL_CONTOURS_POSITION, const wxSize& size = SYMBOL_CONTOURS_SIZE, long style = SYMBOL_CONTOURS_STYLE );

   /// Creates the controls and sizers
   void CreateControls();

   ///Set the data type\n
   ///\param type The type of data\n
   ///Valid types are:\n
   ///SCALAR\n
   ///VECTOR
   void SetDataType(std::string type = "SCALAR");
   
   ///Get the data type being displayed
   std::string GetDataType();

   /// Retrieves bitmap resources
   wxBitmap GetBitmapResource( const wxString& name );

   /// Retrieves icon resources
   wxIcon GetIconResource( const wxString& name );

   /// Should we show tooltips?
   static bool ShowToolTips();
protected:
   ///Update the contour with the current settings.
   void _updateContourInformation();
   ///Update the advanced settings
   void _updateAdvancedSettings();
   
   /// wxEVT_COMMAND_RADIOBOX_SELECTED event handler for ID_RADIOBOX
   void _onDirection( wxCommandEvent& event );

   /// wxEVT_COMMAND_RADIOBOX_SELECTED event handler for ID_RADIOBOX1
   void _onContourType( wxCommandEvent& event );

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

   std::string _dataType;///<Scalar or vector data

   wxRadioBox*    _directionRBox;
   //wxRadioBox*    _contourTypeRBox;
   wxRadioButton* _allPrecomputedRButton;
   wxCheckBox*    _cyclePrecomputedCBox;
   wxRadioButton* _singlePlaneRButton;
   wxCheckBox*    _nearestPrecomputedCBox;
   wxSlider*      _planePositonSlider;
   wxButton*      itemButton16;
   wxButton*      itemButton17;

   std::vector<VE_XML::DataValuePair*> _advancedSettings;///<The advanced settings.
   std::vector<VE_XML::DataValuePair*> _contourInformation;///<The countour setting data

   std::string _planeDirection;///<Store the value of the direction.
   std::string _planeType;///<The contour type.
   std::string _numberOfPlanesOption;///<Single or Multiple planes.
   std::string _planeOption;///<Once single or multiple is selected, the plane option corresponds to the checkbox.
   double _planePosition;///<The position of the plane.
   double _lastLOD;///<The last LOD setting from the advanced panel.
   double _lastWarpedScale;///<The last warped scale setting from the advanced panel.
   double _lastOpacity;///<The last opacity setting from the advanced panel.
   std::vector<double> _lastVectorThreshold;///<The min and max values for the vector threshold.
   double _lastVectorScale;///<The vector scale.
   double _lastVectorRatio;///<The vector ratio.
   bool _lastScaleByMagnitude;///<Flag for scaling.
   bool _warpOption;///<Flag for warp contour option


};

#endif
    // _CONTOURS_H_
