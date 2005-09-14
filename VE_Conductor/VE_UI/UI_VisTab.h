/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _VE_UI_VISUALIZATION_TAB_H_
#define _VE_UI_VISUALIZATION_TAB_H_
#ifdef WIN32
#include <winsock2.h>
#endif

#include <wx/scrolwin.h>
#include <wx/msgdlg.h>
#include <wx/panel.h>

class wxRadioButton;
class wxCheckBox;
class wxRadioBox;
class wxButton;
class wxComboBox;
class wxListBox;
class wxTextCtrl;
class wxSlider;
class wxString;
class wxNotebook;
class wxStaticText;
class wxStaticBox;
class wxSizer;

class UI_TransientDialog;
//Visualizaton tab control ids
enum VIS_TAB_IDS
{
   CATEGORY_RAD_BOX,
   CONTOUR_RAD_BOX,
   DIRECTION_RBOX,
   PRE_COMP_SURF_BUTTON,
   POLYDATA_WARPED_DURFACE,
   SINGLE_PLANE_BUTTON,
   CYCLE_CHECK_BOX,
   NEAREST_PLANE_CHECK_BOX,
   VIS_SLIDER,
   UPDATE_BUTTON,
   SCALAR_BAR_CHECK_BOX,
   RECORD_BUTTON,
   CLEAR_BUTTON,
   EXIT_BUTTON,
   CUSTOM_VIS_BUTTON,
   TRANSIENT_CHECK_BOX,
   CFD_VIS_OPTION,
   MIRROR_CHECK_BOX,
   TRANSIENT_DIALOG,
   GEOM_PICK_CBOX
};

class UI_VisualizationTab : public wxScrolledWindow
{
   public:

      UI_VisualizationTab(wxNotebook* tControl);
      ~UI_VisualizationTab(){;}

      void _onExit(wxCommandEvent& event);

   protected:
      wxNotebook* _parent;

      //the controls
      wxRadioBox* _categoryRBox;
      wxRadioBox* _contourRBox;
      wxRadioBox* _directionRBox;
      wxRadioButton* _pcsButton;
      wxRadioButton* _spButton;
      wxCheckBox* _cycleCBox;
      wxCheckBox* _nearestCBox;
      wxSlider* _slider;
      wxButton* _sliderUpdate;
      wxCheckBox* _scalarBarCBox;
      wxCheckBox* _transientCheckBox;
      wxCheckBox* _visOptionCheckBox;
      wxCheckBox* mirrorOptionCheckBox;
      wxCheckBox* _geomPickingCBox;
      wxButton* _recordButton;
      wxButton* _clearButton;
      wxButton* _exitButton;
      wxButton* _customVisButton;

      UI_TransientDialog* _transientControls;
      //create this page
      void _buildPage( void );
      void createCommandId( void );
      void createTransientCommandId( void );
      //vispage control event callbacks
      void _onCategory(wxCommandEvent& event);
      void _onContour(wxCommandEvent& event);
      void _onDirection(wxCommandEvent& event);
      void _onPreComp(wxCommandEvent& event);
      void _onSingle(wxCommandEvent& event);
      void _onNearest(wxCommandEvent& event);
      void _onUpdate(wxCommandEvent& event);
      void _onScalarBar(wxCommandEvent& event);
      void _onRecord(wxCommandEvent& event);
   
      void _onSlider(wxScrollEvent& event);
      void _onClear(wxCommandEvent& event);
      void _onCustomVis(wxCommandEvent& event);
      void _onTextureBasedVisual(wxCommandEvent& event);
      void _onMirrorVisualization(wxCommandEvent& event);
      void _onTransientChecked(wxCommandEvent& event);
      void _onGeometryPickingChecked(wxCommandEvent& event);
  
   DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_VISUALIZATION_TAB_H_
