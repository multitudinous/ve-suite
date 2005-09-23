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
 * File:          $RCSfile: UI_VertTab.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _VE_UI_VERTEXDATA_TAB_H_
#define _VE_UI_VERTEXDATA_TAB_H_
#ifdef WIN32
#include <winsock2.h>
#endif

#include <wx/panel.h>

class wxRadioBox;
class wxButton;
class wxSlider;
class wxNotebook;
class wxStaticText;
class wxStaticBox;
class wxSizer;

enum VERTEXDATA_TAB_IDS{
   PARTICLE_OPTIONS_RBOX,   
   DISPLAY_PARTICLE_BUTTON,
   SPHERE_POINT_SIZE_SLIDER,
   SPHERE_SIZE_SLIDER,
   PARTICLE_OPTION_RBOX,
   DISPLAY_PARTICLES_BUTTON
};


class UI_VertTab : public wxPanel{
public:
   UI_VertTab(wxNotebook* tControl);
protected:
   void _buildPage();

   wxNotebook* _parent;
   //the event controls
 
   wxRadioBox* _particleOptionRBox;
   wxButton* _displayParticlesButton;
   wxSlider* _spherePointSizeSlider;
   

   //event handling callbacks
   void _onParticleOption(wxCommandEvent& event);
   void _onDisplayParticle(wxCommandEvent& event);
   void _onSpherePointSizeSlider(wxScrollEvent& event);   
   void ConstructCommandId( void );

   DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_VERTEXDATA_TAB_H_
