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
 * File:          $RCSfile: UI_DesignParTab.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _VE_UI_DESIGNPAR_TAB_H_
#define _VE_UI_DESIGNPAR_TAB_H_
#include <wx/panel.h>
#include <wx/notebook.h>

class wxTextCtrl;
class wxButton;
class wxSizer;
class wxStaticText;

enum DESGINPAR_TAB_IDS
{
   DESIGNPAR_UPDATE_BUTTON
};



class UI_DesignParTab : public wxPanel{
public:
   UI_DesignParTab(wxNotebook* tControl);
protected:
   void _buildPage();
   
   wxNotebook* _parent;
   //the controls
   wxTextCtrl* _param1;
   wxTextCtrl* _param2;
   wxTextCtrl* _param3;
   wxTextCtrl* _param4;
   wxTextCtrl* _param5;
   wxTextCtrl* _param6;
   wxTextCtrl* _param7;
   wxTextCtrl* _param8;
   wxButton* _updateButton;

   //event handlers
   void _onDesignPar(wxCommandEvent& event);  
   void _onUpdate(wxCommandEvent& event);  

   DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_DESIGNPAR_TAB_H_
