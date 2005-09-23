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
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _VE_UI_FRAME_H_
#define _VE_UI_FRAME_H_
#ifdef WIN32
#include <winsock2.h>
#endif

#include <wx/panel.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>

class wxString;
class wxBoxSizer;

class wxNotebookEvent;
class UI_DatasetPanel;
class UI_Tabs;
class UI_ModSelPanel;
class UI_ModelData;

#ifdef _TAO
#include "VjObsC.h"
#else
#include "VjObs.h"
#endif

////////////////////////////////////////////////////
//This is the class that is the frame.            //
//All instances of new controls should be         //
//added appropriately as members of this class.   //
////////////////////////////////////////////////////


class UI_Frame: public wxPanel{
public:
   UI_Frame (wxWindow* parent, wxWindowID id,
             const wxPoint& pos = wxDefaultPosition,
             const wxSize& size = wxDefaultSize,
             long style = 0);

   UI_Frame(VjObs_ptr ref, wxWindow* parent, wxWindowID =-1,
            const wxPoint& pos = wxDefaultPosition,
            const wxSize& size = wxDefaultSize, 
            long style = wxMAXIMIZE_BOX|wxMINIMIZE_BOX);

   ~UI_Frame();

   void buildCORBA();
   void buildFrame();

   //the events to handle
   void OnTabsEvent(wxNotebookEvent& event);
   void OnIdleEvent(wxIdleEvent& event);
   void OnChangeModel( void );

   //UI_DatasetTab* _datasetPage;
   //UI_ScalarTab* _scalartab;
   UI_DatasetPanel* _datasetPanel;
   //UI_DatasetScrollable* _datasetScrollable;
   VjObs::obj_p_var   datasetTypes;

   //the notebook control that has our tabs
   UI_Tabs* _tabs;
   UI_ModSelPanel* _modselPanel;
   wxString _appParent;

   UI_ModelData* _modelData;

   int activeModIndex;

protected:
	wxBoxSizer* _frameSizer;
	wxBoxSizer* _tabsSizer;
	wxBoxSizer* _datasetSizer;
   wxBoxSizer* _modselSizer;

   VjObs_var vjobs;
};
#endif //_VE_UI_FRAME_H_
