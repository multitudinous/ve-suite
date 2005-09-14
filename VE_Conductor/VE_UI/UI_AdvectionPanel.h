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
#ifndef UI_ADVECTION_PANEL_H
#define UI_ADVECTION_PANEL_H
#ifdef WIN32
#include <winsock2.h>
#endif

#include <wx/panel.h>
class wxCheckBox;
class wxStaticBox;
class wxSlider;
class wxComboBox;

enum ADVECTION_IDS
{
   X_DYE_POS,
   Y_DYE_POS,
   Z_DYE_POS,
   DYE_GROUP,
   DYE_SLIDER_GROUP,
   DYE_CHECK,
   MATERIAL_GROUP,
   MATERIAL_COMBO,
   MATERIAL_DENSITY,
   MATERIAL_INJECTION,
   MATERIAL_DECAY,
   ENABLE_CHECK,
   BBOX_CHECK
};

class UI_AdvectionPanel: public wxPanel
{
public:
   UI_AdvectionPanel(wxNotebook* tControl);
   ~UI_AdvectionPanel();
protected:
   void _buildPage();
   void _setGroupVisibility(bool onOff = true);
   void _setDyeVisibility(bool onOff);
   void _setMaterialVisibility(bool onOff);

   void _onSlider(wxScrollEvent& event);
   void _onEnableCheck(wxCommandEvent& event);
   void _onShowBBoxCheck(wxCommandEvent& event);
   void _onMaterialSwitch(wxCommandEvent& event);

   wxCheckBox* _enableCheck; 
   wxCheckBox* _enableBBox;

   wxStaticBox* _dyeGroup;
   wxStaticBox* _dyeSliderBox;  
   wxCheckBox* _dyeInjectCheck;
   wxSlider* _xDyeLocation;
   wxSlider* _yDyeLocation;
   wxSlider* _zDyeLocation;

   wxStaticBox* _materialGroup;
   wxComboBox* _materialCBox;
   wxSlider* _noiseDensityCtrl;
   wxSlider* _injectionStrengthCtrl;
   wxSlider* _decayStrengthCtrl;

   DECLARE_EVENT_TABLE()
};
#endif //UI_ADVECTION_PANEL_H
