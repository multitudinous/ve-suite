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
#ifndef _VE_UI_VECTOR_TAB_H_
#define _VE_UI_VECTOR_TAB_H_

#ifdef WIN32
#include <winsock2.h>
#endif

#include <wx/wx.h>

//Vector tab control ids
enum VECTOR_TAB_IDS
{
   VECTOR_UPDATE_BUTTON,
   SCALE_VEC_MAG_CHK,
   MIN_THRESH_SLIDER,
   MAX_THRESH_SLIDER,
   RATIO_SLIDER,
   SCALE_SLIDER,
   WARP_SCALE_SLIDER,
   CONTOUR_OPACITY_SLIDER,
   CONTOUR_LOD_SLIDER
};

class UI_VectorTab : public wxScrolledWindow
{
   public:
      UI_VectorTab(wxNotebook* tControl);

   protected:
      void _buildPage();

      wxNotebook* _parent;
      //the controls
      wxSlider* _vThresholdMinSlider;
      wxSlider* _vThresholdMaxSlider;
      wxSlider* _vRatioSlider;
      wxSlider* _vScaleSlider;
      wxSlider* wrapContourScaleSlider;
      wxSlider* contourOpacitySlider;
      wxSlider* contourLODSlider;

      //wxRadioBox* _vectorRBox;
      wxCheckBox* _scaleVecMagChk;
      wxButton* _updateButton;

      //event callback fucntions
      void _onUpdate(wxCommandEvent& event);
      void _onCheck(wxCommandEvent& event);
      void _onvRatioSlider(wxScrollEvent& event);
      void _onvScaleSlider(wxScrollEvent& event);
      void _onThresholdSlider(wxScrollEvent& event);
      void OnContourSliders( wxScrollEvent& event );

      DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_VECTOR_TAB_H_
