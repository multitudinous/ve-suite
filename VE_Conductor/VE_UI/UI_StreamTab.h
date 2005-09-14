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
#ifndef _VE_UI_STREAMLINE_TAB_H_
#define _VE_UI_STREAMLINE_TAB_H_
#ifdef WIN32
#include <winsock2.h>
#endif

#include <wx/notebook.h>
#include <wx/scrolwin.h>
#include <wx/stattext.h>
#include <wx/statbox.h>

class wxSlider;
class wxRadioBox;
class wxCheckBox;
class wxButton;
class wxSizer;

enum STREAMLINE_TAB_IDS
{
   CURSOR_SELECT_RBOX,
   DIR_RBOX,
   INTEGRATE_DIR_RBOX,
   COMP_STREAMLINE_BUTTON,
   PARTICLE_TRACK_BUTTON,
   SEED_POINTS_CHK,
   NUM_PTS_SLIDER,
   SIZE_SLIDER,
   PROP_SLIDER,
   INT_STEP_SLIDER,
   STEP_SLIDER,
   DIAMETER_SLIDER,
   ARROW_POINTS_CHK,
   PARTICLE_DIALOG,
   SPHERE_SCALE_SLIDER
};

class UI_TransientDialog;

class UI_StreamlineTab : public wxScrolledWindow
{
public:
   UI_StreamlineTab(wxNotebook* tControl);
protected:
   void _buildPage();

   wxNotebook* _parent;
   //the event controls
   wxSlider* _propSlider;
   wxSlider* _iStepSlider;
   wxSlider* _stepSlider;
   wxSlider* _nPtsSlider;
   wxSlider* _sizePerSlider;
   wxSlider* _diameterSlider;
   wxSlider* sphereScaleSlider;
   wxRadioBox* _cursorRBox;
   wxRadioBox* _directionRBox;
   wxRadioBox* _integrationDirRBox;
   wxButton* _compStreamButton;
   wxButton* _parTrackingButton;
   wxCheckBox* _lastSeedPtChk;
   wxCheckBox* arrowPointsChk;

   UI_TransientDialog* particleControls;

   //event handling callbacks
   void _onCursorSelect(wxCommandEvent& );
   void _onDirection(wxCommandEvent& );
   void _onIntegrateDir(wxCommandEvent& );
   void _onParticleTrack(wxCommandEvent& );
   void _onCompStreamline(wxCommandEvent& );
   void _onCheck(wxCommandEvent& );
   void OnArrowCheck( wxCommandEvent& );
   void _oniStepSlider(wxScrollEvent& );
   void _onPropSlider(wxScrollEvent& );
   void _onStepSlider(wxScrollEvent& );
   void _onnPointsSlider(wxScrollEvent& );
   void _onDiameterSlider(wxScrollEvent& );
   void onScaleSlider( wxScrollEvent& );

   void ConstructCommandId( void );

   DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_STREAMLINE_TAB_H_
