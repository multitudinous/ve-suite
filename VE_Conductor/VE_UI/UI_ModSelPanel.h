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
#ifndef _VE_UI_MOD_SEL_PANEL_H_
#define _VE_UI_MOD_SEL_PANEL_H_
#ifdef WIN32
#include <winsock2.h>
#endif

#include <wx/panel.h>
#include <wx/scrolwin.h>

class wxString;
class wxRadioBox;
class UI_Frame;
class UI_ModelData;

enum MODSEL_PANEL_IDS{
   RBOX_MODEL_SELECT
};
class UI_ModSelScroll: public wxScrolledWindow{
public:
   UI_ModSelScroll(wxWindow* parent);
   ~UI_ModSelScroll();
   wxString* _models;
   wxRadioBox* _modelSelBox;


   DECLARE_EVENT_TABLE()
};



class UI_ModSelPanel: public wxPanel{
public:
   UI_ModSelPanel(wxWindow* parent, UI_ModelData* _model);
   ~UI_ModSelPanel();

   UI_ModSelScroll* _modselScroll;

   UI_ModelData* _modelData;
  
   void _onModSelect(wxCommandEvent& event);

   DECLARE_EVENT_TABLE()
};

#endif //_VE_UI_MOD_SEL_PANEL_H_
