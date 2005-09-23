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
#ifndef _VE_UI_VIEWLOC_TAB_H_
#define _VE_UI_VIEWLOC_TAB_H_
#ifdef WIN32
#include <winsock2.h>
#endif

#include <wx/scrolwin.h>
#include <wx/panel.h>
#include <vector>

class wxButton;
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
class wxStaticBoxSizer;

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

class UI_ViewLocTabScroll: public wxScrolledWindow{
public:
   UI_ViewLocTabScroll(wxWindow* parent);
   ~UI_ViewLocTabScroll();

   wxWindow* parent;

   wxButton* _addnewviewptButton;
   wxButton* _newvwptNameOKButton;
   wxButton* _newvwptNameCancelButton;
   wxButton* _addnewflythroughButton;
   wxButton* _newflythroughNameOKButton;
   wxButton* _newflythroughNameCancelButton;
   wxButton* _runactiveflyButton;
   wxButton* _stopactiveflyButton;


   wxComboBox* _removevwptSel;
   wxComboBox* _movetovwptSel;
   wxComboBox* _activeflySel;
   wxComboBox* _addvptoflySel;
   wxComboBox* _insertvpinflySel;
   wxComboBox* _removevpfromflySel;
   wxComboBox* _deleteflySel;

   wxListBox* _flybuilderListBox;

   wxTextCtrl* _newvwptNameCtrl;
   wxTextCtrl* _newflythroughNameCtrl;

   wxStaticText* blank1;
   wxStaticText* blank2;
   wxStaticText* blank3; 
   wxStaticText* blank4; 
   wxStaticText* blank5; 
   wxStaticText* blank6; 
   wxStaticText* blank7;
   wxStaticText* _removevwptLabel;
   wxStaticText* _movetovwptLabel;
   wxStaticText* _activeflyLabel;
   wxStaticText* _removevpfromflyLabel;
   wxStaticText* _addvptoflyLabel;
   wxStaticText* _insertvpinflyLabel;
   wxStaticText* _deleteflyLabel;
   wxStaticText* _speedctrlLabel;


   wxSlider* _speedCtrlSlider;

   wxStaticBox* _newVPNameCtrlBox;
   wxStaticBox* _newFlyNameCtrlBox;
   wxStaticBox* _allFlyCtrlBox;
   wxStaticBox* _speedCtrlBox;
   wxStaticBox* _allVPCtrlBox;

   wxBoxSizer* _newVPControlsSizer;
   wxBoxSizer* _flyModCtrlsSizer;
   wxBoxSizer* viewlocPanelGroup;
   wxBoxSizer* _allFlythroughCtrls;
   wxBoxSizer* _runStopFlyButtonsSizer;
   wxBoxSizer* _newFlySizer;
   wxBoxSizer* _newFlyNameButtonsSizer;
   wxBoxSizer* _allLeftSide;
   wxBoxSizer* _newVPNameButtonsSizer;
   wxStaticBoxSizer* _allVPCtrlsGroup;
   wxStaticBoxSizer* _speedCtrlGroup;
   wxStaticBoxSizer* _newFlyNameCtrlGroup;
   wxStaticBoxSizer* _newVPNameCtrlGroup;
   wxStaticBoxSizer* _allFlyCtrlsGroup;


	wxString* _locNamesLocal;
	wxString* _activeFlyNamesLocal;
	wxString* _flythroughNamesLocal;

   void _buildPage( void );
   void _rebuildPage( void );
   void _resetSelections( void );

   int _numViewLocLocal;
   int _vwptsInActiveFlyLocal;
   int _numStoredFlythroughsLocal;

protected:


   DECLARE_EVENT_TABLE()
};

class UI_ViewLocTab : public wxPanel{
public:
   UI_ViewLocTab(wxNotebook* tControl);
   ~UI_ViewLocTab( void );

   unsigned int numStoredLocations;
   unsigned int numStoredFlythroughs;
   unsigned int _vwptsInActiveFly;
   std::vector< std::vector <int> > flyThroughList;
   wxString* _locationName;
   wxString* _flythroughName;
   wxString* _activeFlyNames;


protected:
   void _buildPage( void );
   void _updateWithcfdQuatCamHandler( void );

   void _rebuildNameArrays( void );
   void _setUpActiveFlyThroughNames( int );

   int numView_LocsGlobal;
   
   UI_ViewLocTabScroll* _viewLocScroll;
   wxNotebook* _parent;

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
#endif //_VE_UI_VIEWLOC_TAB_H_
