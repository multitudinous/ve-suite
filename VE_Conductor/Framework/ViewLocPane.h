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
 * File:          $RCSfile: Command.h,v $
 * Date modified: $Date: 2006-01-21 10:25:02 -0600 (Sat, 21 Jan 2006) $
 * Version:       $Rev: 3544 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _VE_UI_VIEWLOC_H
#define _VE_UI_VIEWLOC_H
/*!\file ViewLocPane.h
  *ViewPoints/Flythrough Control Interface
  */
/*!\class VE_Conductor::
 * This class builds the user interface panel which contains all of
 * the controls for the view points and flythrough functionality
 */
#include "VE_Open/skel/VjObsC.h"

#include <wx/wx.h>
#include <wx/image.h>
#include <wx/dialog.h>
#include <wx/bmpbuttn.h>
#include <wx/stattext.h>
#include <wx/statbox.h>
#include <wx/statbmp.h>
#include <wx/scrolwin.h>

#include <xercesc/dom/DOM.hpp>
XERCES_CPP_NAMESPACE_USE

#include <vector>

/*class wxButton;
class wxComboBox;
class wxListBox;
class wxTextCtrl;
class wxSlider;
class wxString;
class wxNotebook;
class wxStaticText;
class wxStaticBox;
class wxSizer;
class wxBoxSizer;
class wxStaticBoxSizer;*/

namespace VE_XML
{
   class Command;
   class DOMDocumentManager;
}


enum VIEWLOC_TAB_IDS{
   VIEWLOC_LOAD_BUTTON,
   VIEWLOC_ACCEPTNEWVPNAME_BUTTON,
   VIEWLOC_CANCELNEWVPNAME_BUTTON,
   VIEWLOC_REMOVEVP_COMBOBOX,
   VIEWLOC_MOVETOVP_COMBOBOX,
   VIEWLOC_NEWFLY_BUTTON,
   VIEWLOC_ACCEPTNEWFLYNAME_BUTTON,
   VIEWLOC_CANCELNEWFLYNAME_BUTTON,
   VIEWLOC_ACTIVEFLYSEL_COMBOBOX,
   VIEWLOC_ADDVPTOFLYSEL_COMBOBOX,
   VIEWLOC_INSERTVPINFLYSEL_COMBOBOX,
   VIEWLOC_REMOVEVPFROMFLYSEL_COMBOBOX,
   VIEWLOC_RUNFLY_BUTTON,
   VIEWLOC_STOPFLY_BUTTON,
   VIEWLOC_FLYBUILDER_LISTBOX,
   VIEWLOC_DELETEFLYSEL_COMBOBOX,
   VIEWLOC_SPEED_CONTROL_SLIDER
};

/*class UI_ViewLocTabScroll: public wxScrolledWindow{
public:
   UI_ViewLocTabScroll(wxWindow* parent);
   ~UI_ViewLocTabScroll();


   int _numViewLocLocal;
   int _vwptsInActiveFlyLocal;
   int _numStoredFlythroughsLocal;

protected:


   DECLARE_EVENT_TABLE()
};*/

class ViewLocPane : public wxDialog
//, public wxScrolledWindow
{
public:
   ViewLocPane( VjObs_ptr veEngine, VE_XML::DOMDocumentManager* domManagerIn );
   ~ViewLocPane( void );

   unsigned int _numStoredLocations;
   unsigned int _numStoredFlythroughs;
   unsigned int _vwptsInActiveFly;
   std::vector< std::vector <int> > flyThroughList;
   wxString* _locationName;
   wxString* _flythroughName;
   wxString* _activeFlyNames;
	wxString* _locNamesLocal;
	wxString* _activeFlyNamesLocal;
	wxString* _flythroughNamesLocal;
   void SendCommandsToXplorer( void );

protected:
   void _buildPage( void );
   void _updateWithcfdQuatCamHandler( void );

   void _rebuildNameArrays( void );
   void _setUpActiveFlyThroughNames( int );
   void _rebuildPage( void );
   void _resetSelections( void );

   int _numView_LocsGlobal;
   std::vector< VE_XML::Command* > commands;
   VjObs_ptr xplorerPtr;
   int cId, cIso_value, cSc, cMin;
   DOMDocument* doc;
   VE_XML::DOMDocumentManager* domManager;
   std::string dataValueName;

   std::vector< long > commandInputs;

   wxWindow* _parent;

   //the controls
   void _onLoad(wxCommandEvent& event);
   void _onAcceptNewVPName(wxCommandEvent& event);
   void _onCancelNewVPName(wxCommandEvent& event);
   void _onRemoveVP(wxCommandEvent& event);
   void _onMoveToVP(wxCommandEvent& event);
   void _onBuildNewFlyButton(wxCommandEvent& event);
   void _onAcceptNewFlyName(wxCommandEvent& event);
   void _onCancelNewFlyName(wxCommandEvent& event);
   void _onActiveFlySel(wxCommandEvent& event);
   void _onAddVPtoFlySel(wxCommandEvent& event);
   void _onInsertVPinFlySel(wxCommandEvent& event);
   void _onRemoveVPfromFlySel(wxCommandEvent& event);
   void _onStartActiveFly(wxCommandEvent& event);
   void _onStopFly(wxCommandEvent& event);
   void _onFlyBuilderListBox(wxCommandEvent& event);
   void _onDeleteFlySel(wxCommandEvent& event);
   void _onSpeedChange(wxScrollEvent& event);

   DECLARE_EVENT_TABLE()
};
#endif// _VE_UI_VIEWLOC_H
