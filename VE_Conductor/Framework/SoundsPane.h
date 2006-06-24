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
 * File:          $RCSfile: SoundsPane.h SoundsPane.hSoundsPane.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _VE_UI_SOUNDS_TAB_H_
#define _VE_UI_SOUNDS_TAB_H_

#include "VE_Open/skel/VjObsC.h"

#include <wx/panel.h>
#include <wx/dialog.h>

#include <xercesc/dom/DOM.hpp>
XERCES_CPP_NAMESPACE_USE

#include <vector>

class wxNotebook;
class wxButton;
class wxCheckListBox;
class wxSizer;

namespace VE_XML
{
   class Command;
   class DOMDocumentManager;
}

enum SOUNDS_TAB_IDS{
   SOUND_CBOX,
   SOUND_UPDATE_BUTTON
};

class SoundsPane : public wxDialog
{
public:
   SoundsPane( VjObs_ptr veEngine, VE_XML::DOMDocumentManager* domManagerIn );
   virtual ~SoundsPane(){;}
   void SetCommInstance( VjObs_ptr veEngine );
   //void SetDOMManager( VE_XML::DOMDocumentManager* domManagerIn );
   void SendCommandsToXplorer( void );
protected:
   void _buildPage();
   
   //the controls
   wxCheckListBox* _soundCBox;
   wxButton* _updateButton;

   std::vector< VE_XML::Command* > commands;
   VjObs_ptr xplorerPtr;
   int cId, cIso_value;
   short num_sounds;
   VjObs::scalar_p_var   soundNameArray;
   DOMDocument* doc;
   VE_XML::DOMDocumentManager* domManager;
   std::string dataValueName;

   //event handlers
   void _onSounds(wxCommandEvent& event);  
   void _onUpdate(wxCommandEvent& event);  

   DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_SOUNDS_TAB_H_
