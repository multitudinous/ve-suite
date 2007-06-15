/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _VE_UI_TEACHER_TAB_H_
#define _VE_UI_TEACHER_TAB_H_
/*!\file UI_TeacherTab.h
UI_TeacherTab API
*/
/*!\class UI_TeacherTab
* 
*/
#include <wx/dialog.h>

class wxWindow;
class wxButton;
class wxRadioButton;
class wxSizer;
class wxRadioBox;
class wxBoxSizer;

class UI_TeacherTab : public wxDialog
{
public:
   UI_TeacherTab(wxWindow* tControl);
   virtual ~UI_TeacherTab( void ){ ; }

   enum TEACHER_TAB_IDS
   {
      TEACHER_RBOX,
      TEACHER_CLEAR_BUTTON,
      RECORD_SCENE
   };
protected:
   void _buildPage();
   
   wxWindow* _parent;
   //the controls
   wxRadioBox* _teacherRBox;
   wxButton* _clearButton;
   wxBoxSizer* teacherPanelGroup;
   //event handlers
   void _onTeacher(wxCommandEvent& event);
   void _onClear(wxCommandEvent& event);

   DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_TEACHER_TAB_H_
