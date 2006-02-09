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
 * File:          $RCSfile: UI_NavTab.cpp,v $
 * Date modified: $Date: 2005-09-23 12:56:51 -0500 (Fri, 23 Sep 2005) $
 * Version:       $Rev: 3082 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Framework/ViewLocPane.h"
#include "VE_Open/XML/DOMDocumentManager.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Command.h"
#include "VE_Xplorer/cfdEnum.h"

#include "VE_Installer/installer/installerImages/ve_xplorer_banner.xpm"

#include <iostream>
#include <string>
#include <sstream>

#include <wx/utils.h>
#include <wx/button.h>
#include <wx/combobox.h>
#include <wx/listbox.h>
#include <wx/textctrl.h>
#include <wx/slider.h>
#include <wx/string.h>
#include <wx/notebook.h>
#include <wx/stattext.h>
#include <wx/statbox.h>
#include <wx/sizer.h>

BEGIN_EVENT_TABLE(ViewLocPane, wxDialog)
   EVT_BUTTON(VIEWLOC_LOAD_BUTTON,ViewLocPane::_onLoad)
   EVT_BUTTON(VIEWLOC_ACCEPTNEWVPNAME_BUTTON,ViewLocPane::_onAcceptNewVPName)
   EVT_BUTTON(VIEWLOC_CANCELNEWVPNAME_BUTTON,ViewLocPane::_onCancelNewVPName)
   EVT_COMBOBOX(VIEWLOC_REMOVEVP_COMBOBOX,ViewLocPane::_onRemoveVP)
   EVT_COMBOBOX(VIEWLOC_MOVETOVP_COMBOBOX,ViewLocPane::_onMoveToVP)
   EVT_BUTTON(VIEWLOC_NEWFLY_BUTTON,ViewLocPane::_onBuildNewFlyButton)
   EVT_BUTTON(VIEWLOC_ACCEPTNEWFLYNAME_BUTTON,ViewLocPane::_onAcceptNewFlyName)
   EVT_BUTTON(VIEWLOC_CANCELNEWFLYNAME_BUTTON,ViewLocPane::_onCancelNewFlyName)
   EVT_COMBOBOX(VIEWLOC_ACTIVEFLYSEL_COMBOBOX,ViewLocPane::_onActiveFlySel)
   EVT_COMBOBOX(VIEWLOC_ADDVPTOFLYSEL_COMBOBOX,ViewLocPane::_onAddVPtoFlySel)
   EVT_COMBOBOX(VIEWLOC_INSERTVPINFLYSEL_COMBOBOX,ViewLocPane::_onInsertVPinFlySel)
   EVT_COMBOBOX(VIEWLOC_REMOVEVPFROMFLYSEL_COMBOBOX,ViewLocPane::_onRemoveVPfromFlySel)
   EVT_COMBOBOX(VIEWLOC_DELETEFLYSEL_COMBOBOX,ViewLocPane::_onDeleteFlySel)
   EVT_BUTTON(VIEWLOC_RUNFLY_BUTTON,ViewLocPane::_onStartActiveFly)
   EVT_BUTTON(VIEWLOC_STOPFLY_BUTTON,ViewLocPane::_onStopFly)
   EVT_LISTBOX(VIEWLOC_FLYBUILDER_LISTBOX,ViewLocPane::_onFlyBuilderListBox)
   EVT_COMMAND_SCROLL(VIEWLOC_SPEED_CONTROL_SLIDER, ViewLocPane::_onSpeedChange )
END_EVENT_TABLE()

///////////////
//Constructor//
///////////////
ViewLocPane::ViewLocPane( VjObs_ptr veEngine, VE_XML::DOMDocumentManager* domManagerIn )
:wxDialog(NULL, -1, wxString("Viewing Locations Pane"),
         wxDefaultPosition, wxDefaultSize, 
		  (wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX|wxHSCROLL|wxVSCROLL) & ~ wxSTAY_ON_TOP)
{
   _numStoredLocations = 0;
   _numStoredFlythroughs = 0;
   _vwptsInActiveFly = 0;
   _locationName = 0;
   _flythroughName = 0;
   _activeFlyNames = 0;
   _numView_LocsGlobal = 0;
	_locNamesLocal = 0;
	_activeFlyNamesLocal = 0;
	_flythroughNamesLocal = 0;
   flyThroughList.clear();
   
   xplorerPtr = veEngine;
   domManager = domManagerIn;

   _buildPage();

}

ViewLocPane::~ViewLocPane( void )
{
   delete [] _locationName;
}
//////////////////////////////
//build the viewing locations tab       //
//////////////////////////////
void ViewLocPane::_buildPage()
{
   try
   {
      if ( !CORBA::is_nil( xplorerPtr ) )
      {
std::cout << "I Made It Here0 " << std::endl;
         num_viewlocs = xplorerPtr->getIsoValue();
      }
      std::cout << "number of viewing locations: "<< num_viewlocs << std::endl;

std::cout << "I Made It Here1 " << std::endl;

      if ( !CORBA::is_nil( xplorerPtr ) )
      {
std::cout << "I Made It Here2 " << std::endl;

         flyThroughArray = xplorerPtr->getDouble2D( "getFlythroughData" );
std::cout << "I Made It Here3 " << std::endl;

      }
      std::cout << "number of flythroughs: "<< flyThroughArray->length() << std::endl;
   }
   catch ( ... )
   {
      wxMessageBox( "Send data to VE-Xplorer failed. Probably need to disconnect and reconnect.", 
                     "Communication Failure", wxOK | wxICON_INFORMATION );
   }

   _numStoredLocations = num_viewlocs;

   for (CORBA::ULong j=0; j<flyThroughArray->length(); j++ )
   {
      std::vector<int> tempPts;
      for (CORBA::ULong k=0; k<flyThroughArray[j].length(); k++ )
      {
         tempPts.push_back( (int)flyThroughArray[j][k] );
      }    
      flyThroughList.push_back(tempPts);
      tempPts.clear();
   }

   _numView_LocsGlobal = _numStoredLocations;

   _rebuildNameArrays();
   if ( flyThroughList.size() > 0 )
   {
      _setUpActiveFlyThroughNames( 0 );
   }

	_vwptsInActiveFlyLocal = _vwptsInActiveFly;
	_numStoredFlythroughsLocal = _numStoredFlythroughs;
	
	if ( _locNamesLocal )
	{
		delete [] _locNamesLocal;
	}

	_locNamesLocal = new wxString[ _numViewLocLocal ];

	if ( _activeFlyNamesLocal )
	{
		delete [] _activeFlyNamesLocal;
	}

	_activeFlyNamesLocal = new wxString[ _vwptsInActiveFlyLocal ];

	if ( _flythroughNamesLocal )
	{
		delete [] _flythroughNamesLocal;
	}

	_flythroughNamesLocal = new wxString[ _numStoredFlythroughsLocal ];

	_locNamesLocal = _locationName;
	_activeFlyNamesLocal = _activeFlyNames;
	_flythroughNamesLocal = _flythroughName;

//*******Setting up the widgets for making and naming a new view point
   wxBoxSizer* mainSizer = new wxBoxSizer(wxHORIZONTAL);
 
   scrollWindow = new wxScrolledWindow( this, -1, wxDefaultPosition, wxDefaultSize, wxHSCROLL | wxVSCROLL);
   int nUnitX=20;
   int nUnitY=10;
   int nPixX = 5;
   int nPixY = 10;
   scrollWindow->SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );

   wxStaticBox* _allVPCtrlBox = new wxStaticBox(scrollWindow, -1, "View Point Controls", wxDefaultPosition,wxDefaultSize,wxCAPTION); 

	wxStaticBox* _newVPNameCtrlBox = new wxStaticBox(scrollWindow, -1, "Name the new View Point", wxDefaultPosition,wxDefaultSize,wxCAPTION);

   wxButton* _addnewviewptButton = new wxButton(scrollWindow, VIEWLOC_LOAD_BUTTON, wxT("Add New View Pt"));
   
   wxTextCtrl* _newvwptNameCtrl = new wxTextCtrl(scrollWindow, -1, wxT("Enter Name for new pt"));

   wxButton* _newvwptNameOKButton = new wxButton(scrollWindow, VIEWLOC_ACCEPTNEWVPNAME_BUTTON, wxT("OK"));

   wxButton* _newvwptNameCancelButton = new wxButton(scrollWindow, VIEWLOC_CANCELNEWVPNAME_BUTTON, wxT("Cancel"));

   wxBoxSizer* _newVPNameButtonsSizer = new wxBoxSizer(wxHORIZONTAL);
   _newVPNameButtonsSizer->Add(_newvwptNameOKButton,1,wxALIGN_LEFT);
   _newVPNameButtonsSizer->Add(_newvwptNameCancelButton,1,wxALIGN_RIGHT);

   wxStaticBoxSizer* _newVPNameCtrlGroup = new wxStaticBoxSizer(_newVPNameCtrlBox, wxVERTICAL);
   _newVPNameCtrlGroup->Add(_newvwptNameCtrl,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   _newVPNameCtrlGroup->Add(_newVPNameButtonsSizer,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
//***************************************************************************

//********Finishing off the view points controls
   wxStaticText* _removevwptLabel = new wxStaticText(scrollWindow, -1, wxT("Delete View Points "));

   _removevwptSel = new wxComboBox(scrollWindow, VIEWLOC_REMOVEVP_COMBOBOX, wxT("Select a View Point"),wxDefaultPosition, 
                                 wxDefaultSize,_numViewLocLocal, 
                                 _locNamesLocal, wxCB_READONLY);

   wxStaticText* _movetovwptLabel = new wxStaticText(scrollWindow, -1, wxT("Move to a View Point "));

   _movetovwptSel = new wxComboBox(scrollWindow, VIEWLOC_MOVETOVP_COMBOBOX, wxT("Select a View Point"),wxDefaultPosition, 
                                 wxDefaultSize,_numViewLocLocal, 
                                 _locNamesLocal, wxCB_READONLY);

   wxStaticText* blank1 = new wxStaticText(scrollWindow, -1, ""); //just a place holder
   wxStaticText* blank2 = new wxStaticText(scrollWindow, -1, ""); //just a place holder


   wxBoxSizer* _newVPControlsSizer = new wxBoxSizer(wxVERTICAL);
   _newVPControlsSizer->Add(_addnewviewptButton,1,wxALIGN_CENTER_HORIZONTAL);
   _newVPControlsSizer->Add(_newVPNameCtrlGroup,3,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   _newVPControlsSizer->Add(blank1,2,wxALIGN_CENTER_HORIZONTAL);
   _newVPControlsSizer->Add(_removevwptLabel,1,wxALIGN_LEFT|wxEXPAND);
   _newVPControlsSizer->Add(_removevwptSel,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   _newVPControlsSizer->Add(blank2,2,wxALIGN_CENTER_HORIZONTAL);
   _newVPControlsSizer->Add(_movetovwptLabel,1,wxALIGN_LEFT|wxEXPAND);
   _newVPControlsSizer->Add(_movetovwptSel,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

   wxStaticBoxSizer* _allVPCtrlsGroup = new wxStaticBoxSizer(_allVPCtrlBox, wxVERTICAL);
   _allVPCtrlsGroup->Add(_newVPControlsSizer,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

//*******Throw in the Speed Control Slider
   wxStaticBox* _speedCtrlBox = new wxStaticBox(scrollWindow, -1, "Movement Speed Control", wxDefaultPosition,wxDefaultSize,wxCAPTION); 

   wxStaticText* _speedctrlLabel = new wxStaticText(scrollWindow, -1, wxT("Approximate Linear Speed in feet/second"));

   _speedCtrlSlider = new wxSlider(scrollWindow, VIEWLOC_SPEED_CONTROL_SLIDER,10,0,100,
                                       wxDefaultPosition, wxDefaultSize,
                                       wxSL_HORIZONTAL|
                                       wxSL_LABELS );

   wxStaticBoxSizer* _speedCtrlGroup = new wxStaticBoxSizer(_speedCtrlBox, wxVERTICAL);
   _speedCtrlGroup->Add(_speedctrlLabel,1,wxALIGN_CENTER_HORIZONTAL);
   _speedCtrlGroup->Add(_speedCtrlSlider,2,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

   wxBoxSizer* _allLeftSide = new wxBoxSizer(wxVERTICAL);
   _allLeftSide->Add(_allVPCtrlsGroup,3,wxALIGN_CENTER_HORIZONTAL); 
   _allLeftSide->Add(_speedCtrlGroup,1,wxALIGN_CENTER_HORIZONTAL);


//***************************************************************************



//*******Building the Flythrough Controls
   wxStaticBox* _allFlyCtrlBox = new wxStaticBox(scrollWindow, -1, "Flythrough Controls", wxDefaultPosition,wxDefaultSize,wxCAPTION); 

   //Start with the controls for setting up a new flythrough and naming it
	wxStaticBox* _newFlyNameCtrlBox = new wxStaticBox(scrollWindow, -1, "Name the new Flythrough", wxDefaultPosition,wxDefaultSize,wxCAPTION);

   wxButton* _addnewflythroughButton = new wxButton(scrollWindow, VIEWLOC_NEWFLY_BUTTON, wxT("Add New Flythrough"));
   
   wxTextCtrl* _newflythroughNameCtrl = new wxTextCtrl(scrollWindow, -1, wxT("Enter Name for new flythrough"));

   wxButton* _newflythroughNameOKButton = new wxButton(scrollWindow, VIEWLOC_ACCEPTNEWFLYNAME_BUTTON, wxT("OK"));

   wxButton* _newflythroughNameCancelButton = new wxButton(scrollWindow, VIEWLOC_CANCELNEWFLYNAME_BUTTON, wxT("Cancel"));

   wxBoxSizer* _newFlyNameButtonsSizer = new wxBoxSizer(wxHORIZONTAL);
   _newFlyNameButtonsSizer->Add(_newflythroughNameOKButton,1,wxALIGN_LEFT);
   _newFlyNameButtonsSizer->Add(_newflythroughNameCancelButton,1,wxALIGN_RIGHT);

   wxStaticBoxSizer* _newFlyNameCtrlGroup = new wxStaticBoxSizer(_newFlyNameCtrlBox, wxVERTICAL);
   _newFlyNameCtrlGroup->Add(_newflythroughNameCtrl,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   _newFlyNameCtrlGroup->Add(_newFlyNameButtonsSizer,0,wxALIGN_CENTER_HORIZONTAL);

   wxBoxSizer* _newFlySizer = new wxBoxSizer(wxVERTICAL);
   _newFlySizer->Add(_addnewflythroughButton,1,wxALIGN_CENTER_HORIZONTAL);
   _newFlySizer->Add(_newFlyNameCtrlGroup,2,wxALIGN_CENTER_HORIZONTAL);


   //**********************************************

   //The rest of the flythrough controls
   wxStaticText* _activeflyLabel = new wxStaticText(scrollWindow, -1, wxT("Active Flythrough Selection"));

   _activeflySel = new wxComboBox(scrollWindow, VIEWLOC_ACTIVEFLYSEL_COMBOBOX, wxT("Select Active Flythrough"),wxDefaultPosition, 
                                  wxDefaultSize, _numStoredFlythroughsLocal, 
                                  _flythroughNamesLocal, wxCB_READONLY);

   wxStaticText* _addvptoflyLabel = new wxStaticText(scrollWindow, -1, wxT("Add Viewpts at the end of Flythrough"));

   _addvptoflySel = new wxComboBox(scrollWindow, VIEWLOC_ADDVPTOFLYSEL_COMBOBOX, wxT("Select a View Point"),wxDefaultPosition, 
                                   wxDefaultSize, _numViewLocLocal, 
                                   _locNamesLocal, wxCB_READONLY);

   wxStaticText* _insertvpinflyLabel = new wxStaticText(scrollWindow, -1, wxT("Insert Viewpts within Flythrough"));

   _insertvpinflySel = new wxComboBox(scrollWindow, VIEWLOC_INSERTVPINFLYSEL_COMBOBOX, wxT("Select a View Point"),wxDefaultPosition, 
                                      wxDefaultSize, _numViewLocLocal, 
                                      _locNamesLocal, wxCB_READONLY);

   wxStaticText* _removevpfromflyLabel = new wxStaticText(scrollWindow, -1, wxT("Remove Viewpts from Flythrough"));

   _removevpfromflySel = new wxComboBox(scrollWindow, VIEWLOC_REMOVEVPFROMFLYSEL_COMBOBOX, wxT("Select a View Point"),wxDefaultPosition, 
                                        wxDefaultSize, _vwptsInActiveFlyLocal,
                                        _activeFlyNamesLocal, wxCB_READONLY);

   wxStaticText* _deleteflyLabel = new wxStaticText(scrollWindow, -1, wxT("Delete Entire Flythrough"));

   _deleteflySel = new wxComboBox(scrollWindow, VIEWLOC_DELETEFLYSEL_COMBOBOX, wxT("Select a Flythrough"),wxDefaultPosition, 
                                        wxDefaultSize, _numStoredFlythroughsLocal, 
                                        _flythroughNamesLocal, wxCB_READONLY);


   _flybuilderListBox = new wxListBox(scrollWindow, VIEWLOC_FLYBUILDER_LISTBOX, wxDefaultPosition, wxDefaultSize,
                                      _vwptsInActiveFlyLocal, _activeFlyNamesLocal,  
                                       wxLB_HSCROLL|wxLB_NEEDED_SB, wxDefaultValidator, wxT("Active Flythrough Order"));    

   wxStaticText* blank3 = new wxStaticText(scrollWindow, -1, ""); //just a place holder
   wxStaticText* blank4 = new wxStaticText(scrollWindow, -1, ""); //just a place holder
   wxStaticText* blank5 = new wxStaticText(scrollWindow, -1, ""); //just a place holder
   wxStaticText* blank6 = new wxStaticText(scrollWindow, -1, ""); //just a place holder
   wxStaticText* blank7 = new wxStaticText(scrollWindow, -1, ""); //just a place holder

   wxBoxSizer* _flyModCtrlsSizer = new wxBoxSizer(wxVERTICAL);
   _flyModCtrlsSizer->Add(_activeflyLabel,1,wxALIGN_LEFT|wxEXPAND);
   _flyModCtrlsSizer->Add(_activeflySel,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   _flyModCtrlsSizer->Add(_flybuilderListBox,7,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   _flyModCtrlsSizer->Add(blank3,1,wxALIGN_LEFT|wxEXPAND);
   _flyModCtrlsSizer->Add(_addvptoflyLabel,1,wxALIGN_LEFT|wxEXPAND);
   _flyModCtrlsSizer->Add(_addvptoflySel,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   _flyModCtrlsSizer->Add(blank4,1,wxALIGN_LEFT|wxEXPAND);
   _flyModCtrlsSizer->Add(_insertvpinflyLabel,1,wxALIGN_LEFT|wxEXPAND);
   _flyModCtrlsSizer->Add(_insertvpinflySel,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   _flyModCtrlsSizer->Add(blank5,1,wxALIGN_LEFT|wxEXPAND);
   _flyModCtrlsSizer->Add(_removevpfromflyLabel,1,wxALIGN_LEFT|wxEXPAND);
   _flyModCtrlsSizer->Add(_removevpfromflySel,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   _flyModCtrlsSizer->Add(blank6,1,wxALIGN_LEFT|wxEXPAND);
   _flyModCtrlsSizer->Add(_deleteflyLabel,1,wxALIGN_LEFT|wxEXPAND);
   _flyModCtrlsSizer->Add(_deleteflySel,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   _flyModCtrlsSizer->Add(blank7,1,wxALIGN_LEFT|wxEXPAND);


   wxButton* _runactiveflyButton = new wxButton(scrollWindow, VIEWLOC_RUNFLY_BUTTON, wxT("Start Active Flythrough"));

   wxButton* _stopactiveflyButton = new wxButton(scrollWindow, VIEWLOC_STOPFLY_BUTTON, wxT("Stop Flythrough"));

   wxBoxSizer* _runStopFlyButtonsSizer = new wxBoxSizer(wxHORIZONTAL);
   _runStopFlyButtonsSizer->Add(_runactiveflyButton,1,wxALIGN_LEFT);
   _runStopFlyButtonsSizer->Add(_stopactiveflyButton,1,wxALIGN_RIGHT);

   wxBoxSizer* _allFlythroughCtrls = new wxBoxSizer(wxVERTICAL);
   _allFlythroughCtrls->Add(_newFlySizer,1,wxALIGN_CENTER_HORIZONTAL);
   _allFlythroughCtrls->Add(_flyModCtrlsSizer,4,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   _allFlythroughCtrls->Add(_runStopFlyButtonsSizer,1,wxALIGN_CENTER_HORIZONTAL);

   wxStaticBoxSizer* _allFlyCtrlsGroup = new wxStaticBoxSizer(_allFlyCtrlBox, wxVERTICAL);
   _allFlyCtrlsGroup->Add(_allFlythroughCtrls,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);


//********Now Put the entire page together
   //the main group
   wxBoxSizer* viewlocPanelGroup = new wxBoxSizer(wxHORIZONTAL);

   //add the rows to the main panel
   viewlocPanelGroup->Add(_allLeftSide,1,wxALIGN_CENTER_HORIZONTAL); 
   viewlocPanelGroup->Add(_allFlyCtrlsGroup,2,wxALIGN_CENTER_HORIZONTAL); 

   scrollWindow->SetSizer( viewlocPanelGroup ); 

   mainSizer->Add( scrollWindow,1,wxALIGN_LEFT|wxEXPAND);
 
   SetAutoLayout(true);
   this->SetIcon( wxIcon( ve_xplorer_banner_xpm ) );
   SetSizer( mainSizer );

   //Complete Hack needed to get the page to refresh properly
	// Hack because Refresh and SetSize(GetSize() ) don't work on win32 platform
   static bool test = false;
   int flag = 0;
   if ( test )
   {
      flag = 1;
	   test = false;
   }
   else
   {
      flag = -1;
	   test = true;
   }
   
   wxSize temp = GetSize();
   temp.SetHeight( temp.GetHeight()+flag );
   temp.SetWidth( temp.GetWidth()+flag );
   SetSize( temp );

   _rebuildPage();
	_resetSelections();
   _activeflySel->SetSelection( 0 );

   _newVPNameCtrlBox->Enable( false );
   _newvwptNameCtrl->Enable( false );
   _newvwptNameOKButton->Enable( false );
   _newvwptNameCancelButton->Enable( false );

   _newFlyNameCtrlBox->Enable( false );
   _newflythroughNameCtrl->Enable( false );
   _newflythroughNameOKButton->Enable( false );
   _newflythroughNameCancelButton->Enable( false );

}


void ViewLocPane::_rebuildNameArrays( void )
{
   //This will get called every time there's a change so all
   //dynamic memory allocations have to be cleaned up
   if ( _locationName )
   {
      delete [] _locationName;
   }
   if ( _flythroughName )
   {
      delete [] _flythroughName;
   }

   //Now the wxString arrays can be filled
   if( _numStoredLocations > 0 )
   {
      _locationName = new wxString[ _numStoredLocations ];

      for( unsigned int i=0; i<_numStoredLocations; i++)
      {
         std::ostringstream vwptstream;
         vwptstream << "View Location " << i ;
         _locationName[i] = vwptstream.str().c_str();
      }
   }
   else
   {
      _numStoredLocations = 1;
      _locationName = new wxString[1];
      _locationName[0] = wxT("No Stored Locations");
   }

   if( !flyThroughList.empty() )
   {
      _numStoredFlythroughs = flyThroughList.size();
      _flythroughName = new wxString[ _numStoredFlythroughs ];

      for( unsigned int i=0; i<_numStoredFlythroughs; i++)
      {
         std::ostringstream flynamestream;
         flynamestream << "Flythrough " << i ;
         _flythroughName[i] = flynamestream.str().c_str();
      }
 
   }
   else
   {
      _numStoredFlythroughs = 1;
      _flythroughName = new wxString[1];
      _flythroughName[0] = wxT("No Flythroughs Built");
      _vwptsInActiveFly = 1;
      _activeFlyNames = new wxString[ _vwptsInActiveFly ];
      _activeFlyNames[0] = wxT("No Flythroughs Built");
   }
}

void ViewLocPane::_setUpActiveFlyThroughNames( int index )
{
   if ( _activeFlyNames )
   {
      delete [] _activeFlyNames;
   }

   _vwptsInActiveFly = flyThroughList.at( index ).size();
   _activeFlyNames = new wxString[ _vwptsInActiveFly ];

   for ( unsigned int i=0; i<_vwptsInActiveFly; i++ )
   {
      std::ostringstream activeflynamestream;
      activeflynamestream << "View Location " << flyThroughList.at( index ).at( i ) ;
      _activeFlyNames[i] = activeflynamestream.str().c_str();
   }
  
}

void ViewLocPane::_updateWithcfdQuatCamHandler( void )
{
   unsigned int tempindex = 0;
   for( unsigned int i=0;i<flyThroughList.size();i++ )
   {
	   if( _activeflySel->GetValue() == _flythroughName[i])
      {
         tempindex = i;
      }
   }  
   
   flyThroughList.clear();
   VjObs::double2DArray_var  flyThroughArray;

   if ( !CORBA::is_nil( xplorerPtr ) )
   {
      VjObs::obj_pd_var tempTest;
      int tempTestlocal = 0;
      
      while ( tempTestlocal == 0 )
      {
         tempTest = xplorerPtr->getDouble1D( "getCompletionTest" );
         tempTestlocal = (int)tempTest[ 0 ];       
         wxMilliSleep( 50 );
      }
      _numStoredLocations = xplorerPtr->getIsoValue();
   }
 
   if ( !CORBA::is_nil( xplorerPtr ) )
   {
      flyThroughArray = xplorerPtr->getDouble2D( "getFlythroughData" );
   }

   for (CORBA::ULong j=0; j<flyThroughArray->length(); j++ )
   {
      std::vector<int> tempPts;
      for (CORBA::ULong k=0; k<flyThroughArray[j].length(); k++ )
      {
         tempPts.push_back( (int)flyThroughArray[j][k] );
      }    
      flyThroughList.push_back(tempPts);
      tempPts.clear();
   }

   _numView_LocsGlobal = _numStoredLocations;
 
   _rebuildNameArrays();
   if ( flyThroughList.size() > tempindex && flyThroughList.size() != 0 )
   {
      _setUpActiveFlyThroughNames( tempindex );
   }
   else if ( flyThroughList.size() != 0 )
   {
      tempindex = 0;
      _setUpActiveFlyThroughNames( tempindex );
   }
   else
   {
      tempindex = 0;
   }
 
   _rebuildPage();
   _activeflySel->SetValue( _flythroughName[ tempindex ] );

}
//////////////////
//event handling//
///////////////////

//////////////////////////////////////////////////
void ViewLocPane::_onLoad(wxCommandEvent& WXUNUSED(event))
{
   //((UI_Tabs *)_parent)->cId = LOAD_NEW_VIEWPT;
   //((UI_Tabs *)_parent)->sendDataArrayToServer();
   dataValueName = "LOAD_NEW_VIEWPT";
   //cId = navScroll->headRotationChk->GetValue();
   SendCommandsToXplorer();

   _updateWithcfdQuatCamHandler();
   _resetSelections();
}

void ViewLocPane::_onAcceptNewVPName(wxCommandEvent& WXUNUSED(event))
{
   //This will be used once user defined names are implemented
}

void ViewLocPane::_onCancelNewVPName(wxCommandEvent& WXUNUSED(event))
{
   //This will be used once user defined names are implemented
}

void ViewLocPane::_onRemoveVP(wxCommandEvent& WXUNUSED(event))
{
   if ( _numView_LocsGlobal > 0 )
   {
      for( unsigned int i=0;i<_numStoredLocations;i++ )
	   {
		   //if( _removevwptSel->GetStringSelection() == _locationName[i])
         if( _removevwptSel->GetValue() == _locationName[i])
         {
            //((UI_Tabs *)_parent)->cIso_value = i;
            //((UI_Tabs *)_parent)->cId = REMOVE_SELECTED_VIEWPT;
            //((UI_Tabs *)_parent)->sendDataArrayToServer();
            dataValueName = "REMOVE_SELECTED_VIEWPT";
            //cIso_value = i;
            commandInputs.push_back( i );
            SendCommandsToXplorer();

            _updateWithcfdQuatCamHandler();
         }
	   } 
      _resetSelections();
   }
}

void ViewLocPane::_onMoveToVP(wxCommandEvent& WXUNUSED(event))
{
   if ( _numView_LocsGlobal > 0 )
   {
      for( unsigned int i=0;i<_numStoredLocations;i++ )
	   {
		   if( _movetovwptSel->GetValue() == _locationName[i])
         {
            //((UI_Tabs *)_parent)->cIso_value = i;
            //((UI_Tabs *)_parent)->cId = MOVE_TO_SELECTED_LOCATION;
            //((UI_Tabs *)_parent)->sendDataArrayToServer();
            dataValueName = "MOVE_TO_SELECTED_LOCATION";
            //cIso_value = i;
            commandInputs.push_back( i );
            SendCommandsToXplorer();
         }
	   }
      _resetSelections();
   }
}

void ViewLocPane::_onBuildNewFlyButton(wxCommandEvent& WXUNUSED(event))
{
   if ( _numView_LocsGlobal > 0 )
   {
      //((UI_Tabs *)_parent)->cId = ADD_NEW_FLYTHROUGH;
      //((UI_Tabs *)_parent)->sendDataArrayToServer();
      dataValueName = "ADD_NEW_FLYTHROUGH";
      //cIso_value = i;
      SendCommandsToXplorer();

      _updateWithcfdQuatCamHandler();
      _setUpActiveFlyThroughNames( flyThroughList.size() - 1 );
      _rebuildPage();
      _resetSelections();
      _activeflySel->SetSelection( flyThroughList.size() - 1 );
   }
}

void ViewLocPane::_onAcceptNewFlyName(wxCommandEvent& WXUNUSED(event))
{
   //This will be used once user defined names are implemented
}

void ViewLocPane::_onCancelNewFlyName(wxCommandEvent& WXUNUSED(event))
{
   //This will be used once user defined names are implemented
}

void ViewLocPane::_onActiveFlySel(wxCommandEvent& WXUNUSED(event))
{
   if ( _flythroughName[ 0 ] != wxT("No Flythroughs Built") )
   {
      int tempindex = 0;
      for( unsigned int i=0; i<flyThroughList.size(); i++ )
	   {
		   if( _activeflySel->GetValue() == _flythroughName[ i ])
         {
            _setUpActiveFlyThroughNames( i );
            tempindex = i;
         }
	   }
      _rebuildPage();
      _activeflySel->SetValue( _flythroughName[ tempindex ] );
      _resetSelections();
   }
}

void ViewLocPane::_onAddVPtoFlySel(wxCommandEvent& WXUNUSED(event))
{
   if ( _numView_LocsGlobal > 0 )
   {
      if ( flyThroughList.size() > 0 )
      {
         for( unsigned int i=0; i<flyThroughList.size(); i++ )
	      {
		      if( _activeflySel->GetValue() == _flythroughName[i])
            {
               for ( unsigned int j=0; j<_numStoredLocations; j++ )
               {
                  if( _addvptoflySel->GetValue() == _locationName[j])
                  {
                     //((UI_Tabs *)_parent)->cIso_value = i;
                     //((UI_Tabs *)_parent)->cSc = j;
                     //((UI_Tabs *)_parent)->cId = ADD_NEW_POINT_TO_FLYTHROUGH;
                     //((UI_Tabs *)_parent)->sendDataArrayToServer();
                     dataValueName = "ADD_NEW_POINT_TO_FLYTHROUGH";
                     //cIso_value = i;
                     //cSc = j;
                     commandInputs.push_back( i );
                     commandInputs.push_back( j );
                     SendCommandsToXplorer();
                     _updateWithcfdQuatCamHandler();
                  }
               }
            }
	      }
      }
      _resetSelections();
   }
   
}

void ViewLocPane::_onInsertVPinFlySel(wxCommandEvent& WXUNUSED(event))
{
   if ( _numView_LocsGlobal > 0 )
   {
      if ( flyThroughList.size() > 0 )
      {
         for( unsigned int i=0; i<flyThroughList.size(); i++ )
	      {
		      if( _activeflySel->GetValue() == _flythroughName[i])
            {
               for ( unsigned int j=0; j<flyThroughList.at( i ).size(); j++ )
               {
                  if( _flybuilderListBox->IsSelected( j ) )
                  {
                     for ( unsigned int k=0; k<_numStoredLocations; k++ )
                     {
                        if( _insertvpinflySel->GetValue() == _locationName[k])
                        {
                           //((UI_Tabs *)_parent)->cIso_value = i;
                           //((UI_Tabs *)_parent)->cSc = j;
                           //((UI_Tabs *)_parent)->cMin = k;
                           //((UI_Tabs *)_parent)->cId = INSERT_NEW_POINT_IN_FLYTHROUGH;
                           //((UI_Tabs *)_parent)->sendDataArrayToServer();
                           dataValueName = "INSERT_NEW_POINT_IN_FLYTHROUGH";
                           //cIso_value = i;
                           //cSc = j;
                           //cMin = k;
                           commandInputs.push_back( i );
                           commandInputs.push_back( j );
                           commandInputs.push_back( k );
                           SendCommandsToXplorer();
                           _updateWithcfdQuatCamHandler();
                        }
                     }
                  }
               }
            }
	      }
      }
      _resetSelections();
   }
}

void ViewLocPane::_onRemoveVPfromFlySel(wxCommandEvent& WXUNUSED(event))
{
   if ( _numView_LocsGlobal > 0 )
   {
      if ( flyThroughList.size() > 0 )
      {
         for( unsigned int i=0; i<flyThroughList.size(); i++ )
	      {
		      if( _activeflySel->GetValue() == _flythroughName[i])
            {
               for ( unsigned int j=0; j<flyThroughList.at( i ).size(); j++ )
               {
                  if( _removevpfromflySel->GetValue() == _activeFlyNames[j])
                  {
                     //((UI_Tabs *)_parent)->cIso_value = i;
                     //((UI_Tabs *)_parent)->cSc = j;
                     //((UI_Tabs *)_parent)->cId = REMOVE_POINT_FROM_FLYTHROUGH;
                     //((UI_Tabs *)_parent)->sendDataArrayToServer();
                     dataValueName = "REMOVE_POINT_FROM_FLYTHROUGH";
                     //cIso_value = i;
                     //cSc = j;
                     commandInputs.push_back( i );
                     commandInputs.push_back( j );
                     SendCommandsToXplorer();
                     _updateWithcfdQuatCamHandler();
                  }
               }
            }
	      }
      }
      _resetSelections();
   }

}

void ViewLocPane::_onDeleteFlySel(wxCommandEvent& WXUNUSED(event))
{
   if ( flyThroughList.size() > 0 )
   {
      for( unsigned int i=0; i<flyThroughList.size(); i++ )
	   {
		   if( _deleteflySel->GetValue() == _flythroughName[i])
         {
            //((UI_Tabs *)_parent)->cIso_value = i;
            //((UI_Tabs *)_parent)->cId = DELETE_ENTIRE_FLYTHROUGH;
            //((UI_Tabs *)_parent)->sendDataArrayToServer();
            dataValueName = "DELETE_ENTIRE_FLYTHROUGH";
            //cIso_value = i;
            commandInputs.push_back( i );
            SendCommandsToXplorer();
            _updateWithcfdQuatCamHandler();
         }
	   }
      _resetSelections();
   }

}

void ViewLocPane::_onStartActiveFly(wxCommandEvent& WXUNUSED(event))
{
   if ( _numView_LocsGlobal > 0 )
   {
      if ( flyThroughList.size() > 0 )
      {
         for( unsigned int i=0; i<flyThroughList.size(); i++ )
	      {
		      if( ( _activeflySel->GetValue() == _flythroughName[i] ) && ( flyThroughList.at( i ).size() > 1 ) )
            {
               //((UI_Tabs *)_parent)->cIso_value = i;
               //((UI_Tabs *)_parent)->cId = RUN_ACTIVE_FLYTHROUGH;
               //((UI_Tabs *)_parent)->sendDataArrayToServer();
               dataValueName = "RUN_ACTIVE_FLYTHROUGH";
               //cIso_value = i;
               commandInputs.push_back( i );
               SendCommandsToXplorer();
            }
	      }
      }
   }
}

void ViewLocPane::_onStopFly(wxCommandEvent& WXUNUSED(event))
{
   //((UI_Tabs *)_parent)->cId = STOP_ACTIVE_FLYTHROUGH;
   //((UI_Tabs *)_parent)->sendDataArrayToServer();
   dataValueName = "STOP_ACTIVE_FLYTHROUGH";
   SendCommandsToXplorer();
}

void ViewLocPane::_onFlyBuilderListBox(wxCommandEvent& WXUNUSED(event))
{
   
}

void ViewLocPane::_onSpeedChange( wxScrollEvent& WXUNUSED(event) )
{
   //((UI_Tabs *)_parent)->cIso_value = _speedCtrlSlider->GetValue();;
   //((UI_Tabs *)_parent)->cId = CHANGE_MOVEMENT_SPEED;
   //((UI_Tabs *)_parent)->sendDataArrayToServer();
   dataValueName = "CHANGE_MOVEMENT_SPEED";
   //cIso_value = _speedCtrlSlider->GetValue();
   commandInputs.push_back( _speedCtrlSlider->GetValue() );
   SendCommandsToXplorer();
}


///////////////////////////////////////

void ViewLocPane::_rebuildPage( void )
{ 
   _removevwptSel->Clear();
   _movetovwptSel->Clear();
   _addvptoflySel->Clear();
   _insertvpinflySel->Clear();
   _activeflySel->Clear();
   _deleteflySel->Clear();
   _removevpfromflySel->Clear();
   _flybuilderListBox->Clear();

   _removevwptSel->Insert( wxT("Select a View Point"), 0 );
   _movetovwptSel->Insert( wxT("Select a View Point"), 0 );
   _addvptoflySel->Insert( wxT("Select a View Point"), 0 );
   _insertvpinflySel->Insert( wxT("Select a View Point"), 0 );
   _deleteflySel->Insert( wxT("Select a Flythrough"), 0 );   
   _removevpfromflySel->Insert( ("Select a View Point"), 0 ); 

   for ( unsigned int i=0; i<_numStoredLocations; i++ )
   {  
      _removevwptSel->Insert( _locationName[ i ], ( i + 1 ) );
      _movetovwptSel->Insert( _locationName[ i ], ( i + 1 ) );
      _addvptoflySel->Insert( _locationName[ i ], ( i + 1 ) );
      _insertvpinflySel->Insert( _locationName[ i ], ( i + 1 ) );
   }

   for ( unsigned int i=0; i<_numStoredFlythroughs; i++ )
   {
      _activeflySel->Insert( _flythroughName[ i ], ( i ) );
      _deleteflySel->Insert( _flythroughName[ i ], ( i + 1 ) );
   }

   for ( unsigned int i=0; i<_vwptsInActiveFly; i++ )
   {
      _removevpfromflySel->Insert( _activeFlyNames[ i ], ( i + 1 ) );
      _flybuilderListBox->Insert( _activeFlyNames[ i ], ( i ) );
   }


}

void ViewLocPane::_resetSelections( void )
{
   _removevwptSel->SetSelection( 0 );
   _movetovwptSel->SetSelection( 0 );
   _addvptoflySel->SetSelection( 0 );
   _insertvpinflySel->SetSelection( 0 );
   _deleteflySel->SetSelection( 0 );   
   _removevpfromflySel->SetSelection( 0 ); 

}

void ViewLocPane::SetCommInstance( VjObs_ptr veEngine )
{
   xplorerPtr = veEngine;
}

void ViewLocPane::SendCommandsToXplorer( void )
{
   // Now need to construct domdocument and populate it with the new vecommand
   domManager->CreateCommandDocument();
   doc = domManager->GetCommandDocument();

   // Create the command and data value pairs
   VE_XML::DataValuePair* dataValuePair = new VE_XML::DataValuePair( std::string("LONG") );
   //VE_XML::OneDIntArray* dataValueArray = new VE_XML::OneDIntArray( commandInputs.size() )
   dataValuePair->SetDataName( dataValueName );
   //dataValuePair->SetDataValue( static_cast<double>(cIso_value) );
   dataValuePair->SetData( dataValueName, commandInputs );
   VE_XML::Command* veCommand = new VE_XML::Command();
   veCommand->SetCommandName( std::string("ViewLoc_Data") );
   veCommand->AddDataValuePair( dataValuePair );
   doc->getDocumentElement()->appendChild( veCommand->GetXMLData( "vecommand" ) );

   // New need to destroy document and send it
   std::string commandData = domManager->WriteAndReleaseCommandDocument();
   char* tempDoc = new char[ commandData.size() + 1 ];
   tempDoc = CORBA::string_dup( commandData.c_str() );

   if ( !CORBA::is_nil( xplorerPtr ) && !commandData.empty() )
   {
      try
      {
         // CORBA releases the allocated memory so we do not have to
         xplorerPtr->SetCommandString( tempDoc );
      }
      catch ( ... )
      {
         wxMessageBox( "Send data to VE-Xplorer failed. Probably need to disconnect and reconnect.", 
                        "Communication Failure", wxOK | wxICON_INFORMATION );
         delete [] tempDoc;
      }
   }
   else
   {
      delete [] tempDoc;
   }
   //Clean up memory
   delete veCommand;
   commandInputs.clear();
}
