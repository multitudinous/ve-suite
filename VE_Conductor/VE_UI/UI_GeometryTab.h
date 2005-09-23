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
 * File:          $RCSfile: UI_GeometryTab.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _VE_UI_GEOMETRY_TAB_H_
#define _VE_UI_GEOMETRY_TAB_H_
#ifdef WIN32
#include <winsock2.h>
#endif

#include <wx/panel.h>
#include <wx/notebook.h>

class wxRadioBox;
class wxCheckListBox;
class wxButton;
class wxSlider;
class wxComboBox;
class wxSizer;

#include <vector>

enum GEOMETRY_TAB_IDS
{
   GEOMETRY_RBOX,
   GEOMETRY_CBOX,
   GEOMETRY_UPDATE_BUTTON,
   GEOMETRY_OPACITY_SLIDER,
   GEOMETRY_LOD_SLIDER,
   GEOMETRY_SELECT_COMBO
};



class UI_GeometryTab : public wxPanel
{
   public:
      UI_GeometryTab(wxNotebook* tControl);
   protected:
      void _buildPage();
   
      //the controls
      wxRadioBox* _geometryRBox;
      wxCheckListBox* _geometryCBox;
      wxButton*   _updateButton;
      wxNotebook* _parent;
      wxSlider*   geomOpacitySlider;
      wxSlider*   geomLODSlider;
      wxComboBox* geometryCombo;

      //event handlers
      void ChangeOpacity( wxScrollEvent& event );
      void _onGeometry( wxScrollEvent& event );
      void _onUpdate( wxCommandEvent& event );
      void OpacityFileSelection( wxCommandEvent& event );

      std::vector< int > opacityMemory;

   DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_GEOMETRY_TAB_H_

