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
#ifndef DEVICES_PROPERTIES
#define DEVICES_PROPERTIES
/*!\file DeviceProperties.h
DeviceProperties API
*/
/*!\class DeviceProperties
* 
*/
#include <vector>
#include <string>

#include <wx/dialog.h>

class wxSplitterWindow;
class wxCheckBox;

namespace VE_XML
{
   class DataValuePair;
}

namespace VE_Conductor
{
   class CORBAServiceList;
}

class DeviceProperties:public wxDialog 
{
   public:
      DeviceProperties( wxWindow* parent );

      virtual ~DeviceProperties();

   enum DEVICE_IDS 
   {
      DEVICE_SPLITTERWINDOW,
      DEVICE_LISTBOX,
      DEVICE_TRACKBALL_PANEL,
      DEVICE_WAND_PANEL,

      ANIMATE_CHECKBOX,
   };

   protected:
      void BuildGUI();

      wxSplitterWindow* device_splitter;
      wxCheckBox* animate_check_box;

      void OnAnimate(wxCommandEvent& event);

      bool animate;

      std::vector<VE_XML::DataValuePair*> instructions;        //The DataValuePairs for the current command

      void SendCommandsToXplorer();
      void ClearInstructions();

      VE_Conductor::CORBAServiceList* serviceList;

      DECLARE_EVENT_TABLE()
};
#endif
