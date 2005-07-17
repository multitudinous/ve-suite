#include "VE_Conductor/VE_UI/UI_ViewLocTab.h"
#include "VE_Conductor/VE_UI/UI_Tabs.h"
#include "VE_Xplorer/cfdEnum.h"
#include <iostream>
#include <string>
#include <sstream>
#include <wx/utils.h>


BEGIN_EVENT_TABLE(UI_ViewLocTabScroll, wxScrolledWindow)
END_EVENT_TABLE()
UI_ViewLocTabScroll::UI_ViewLocTabScroll(wxWindow* parent)
:wxScrolledWindow(parent, -1, wxDefaultPosition, wxDefaultSize,
          wxHSCROLL | wxVSCROLL)
{
   parent = parent;

   int nUnitX=20;
   int nUnitY=10;
   int nPixX = 5;
   int nPixY = 10;
   SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );

//*******Setting up the widgets for making and naming a new view point 
   wxStaticBox* _allVPCtrlBox = new wxStaticBox(this, -1, "View Point Controls", wxDefaultPosition,wxDefaultSize,wxCAPTION); 

	_newVPNameCtrlBox = new wxStaticBox(this, -1, "Name the new View Point", wxDefaultPosition,wxDefaultSize,wxCAPTION);

   _addnewviewptButton = new wxButton(this, VIEWLOC_LOAD_BUTTON, wxT("Add New View Pt"));
   
   _newvwptNameCtrl = new wxTextCtrl(this, -1, wxT("Enter Name for new pt"));

   _newvwptNameOKButton = new wxButton(this, VIEWLOC_ACCEPTNEWVPNAME_BUTTON, wxT("OK"));

   _newvwptNameCancelButton = new wxButton(this, VIEWLOC_CANCELNEWVPNAME_BUTTON, wxT("Cancel"));

   wxBoxSizer* _newVPNameButtonsSizer = new wxBoxSizer(wxHORIZONTAL);
   _newVPNameButtonsSizer->Add(_newvwptNameOKButton,1,wxALIGN_LEFT);
   _newVPNameButtonsSizer->Add(_newvwptNameCancelButton,1,wxALIGN_RIGHT);

   _newVPNameCtrlGroup = new wxStaticBoxSizer(_newVPNameCtrlBox, wxVERTICAL);
   _newVPNameCtrlGroup->Add(_newvwptNameCtrl,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   _newVPNameCtrlGroup->Add(_newVPNameButtonsSizer,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
//***************************************************************************

//********Finishing off the view points controls
   _removevwptLabel = new wxStaticText(this, -1, wxT("Delete View Points "));

   _removevwptSel = new wxComboBox(this, VIEWLOC_REMOVEVP_COMBOBOX, wxT("Select a View Pt to delete"),wxDefaultPosition, 
                                 wxDefaultSize,((UI_ViewLocTab *)GetParent())->numStoredLocations, 
                                 ((UI_ViewLocTab *)GetParent())->_locationName, wxCB_READONLY);

   _movetovwptLabel = new wxStaticText(this, -1, wxT("Move to a View Point "));

   _movetovwptSel = new wxComboBox(this, VIEWLOC_MOVETOVP_COMBOBOX, wxT("Select a View Point"),wxDefaultPosition, 
                                 wxDefaultSize,((UI_ViewLocTab *)GetParent())->numStoredLocations, 
                                 ((UI_ViewLocTab *)GetParent())->_locationName, wxCB_READONLY);

   blank1 = new wxStaticText(this, -1, ""); //just a place holder
   blank2 = new wxStaticText(this, -1, ""); //just a place holder


   _newVPControlsSizer = new wxBoxSizer(wxVERTICAL);
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
   wxStaticBox* _speedCtrlBox = new wxStaticBox(this, -1, "Movement Speed Control", wxDefaultPosition,wxDefaultSize,wxCAPTION); 

   _speedctrlLabel = new wxStaticText(this, -1, wxT("Approximate Linear Speed in feet/second"));

   _speedCtrlSlider = new wxSlider(this, VIEWLOC_SPEED_CONTROL_SLIDER,10,0,100,
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
   wxStaticBox* _allFlyCtrlBox = new wxStaticBox(this, -1, "Flythrough Controls", wxDefaultPosition,wxDefaultSize,wxCAPTION); 

   //Start with the controls for setting up a new flythrough and naming it
	_newFlyNameCtrlBox = new wxStaticBox(this, -1, "Name the new Flythrough", wxDefaultPosition,wxDefaultSize,wxCAPTION);

   _addnewflythroughButton = new wxButton(this, VIEWLOC_NEWFLY_BUTTON, wxT("Add New Flythrough"));
   
   _newflythroughNameCtrl = new wxTextCtrl(this, -1, wxT("Enter Name for new flythrough"));

   _newflythroughNameOKButton = new wxButton(this, VIEWLOC_ACCEPTNEWFLYNAME_BUTTON, wxT("OK"));

   _newflythroughNameCancelButton = new wxButton(this, VIEWLOC_CANCELNEWFLYNAME_BUTTON, wxT("Cancel"));

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
   _activeflyLabel = new wxStaticText(this, -1, wxT("Active Flythrough Selection"));

   _activeflySel = new wxComboBox(this, VIEWLOC_ACTIVEFLYSEL_COMBOBOX, wxT("Select Active Flythrough"),wxDefaultPosition, 
                                  wxDefaultSize, ((UI_ViewLocTab *)GetParent())->numStoredFlythroughs, 
                                  ((UI_ViewLocTab *)GetParent())->_flythroughName, wxCB_READONLY);

   _addvptoflyLabel = new wxStaticText(this, -1, wxT("Add Viewpts at the end of Flythrough"));

   _addvptoflySel = new wxComboBox(this, VIEWLOC_ADDVPTOFLYSEL_COMBOBOX, wxT("Select a View Point"),wxDefaultPosition, 
                                   wxDefaultSize, ((UI_ViewLocTab *)GetParent())->numStoredLocations, 
                                   ((UI_ViewLocTab *)GetParent())->_locationName, wxCB_READONLY);

   _insertvpinflyLabel = new wxStaticText(this, -1, wxT("Insert Viewpts within Flythrough"));

   _insertvpinflySel = new wxComboBox(this, VIEWLOC_INSERTVPINFLYSEL_COMBOBOX, wxT("Select a View Point"),wxDefaultPosition, 
                                      wxDefaultSize, ((UI_ViewLocTab *)GetParent())->numStoredLocations, 
                                      ((UI_ViewLocTab *)GetParent())->_locationName, wxCB_READONLY);

   _removevpfromflyLabel = new wxStaticText(this, -1, wxT("Remove Viewpts from Flythrough"));

   _removevpfromflySel = new wxComboBox(this, VIEWLOC_REMOVEVPFROMFLYSEL_COMBOBOX, wxT("Select a View Point"),wxDefaultPosition, 
                                        wxDefaultSize, ((UI_ViewLocTab *)parent)->_vwptsInActiveFly, 
                                        ((UI_ViewLocTab *)GetParent())->_activeFlyNames, wxCB_READONLY);

   _deleteflyLabel = new wxStaticText(this, -1, wxT("Delete Entire Flythrough"));

   _deleteflySel = new wxComboBox(this, VIEWLOC_DELETEFLYSEL_COMBOBOX, wxT("Select a Flythrough"),wxDefaultPosition, 
                                        wxDefaultSize, ((UI_ViewLocTab *)GetParent())->numStoredFlythroughs, 
                                        ((UI_ViewLocTab *)GetParent())->_flythroughName, wxCB_READONLY);


   _flybuilderListBox = new wxListBox(this, VIEWLOC_FLYBUILDER_LISTBOX, wxDefaultPosition, wxDefaultSize,
                                      ((UI_ViewLocTab *)GetParent())->_vwptsInActiveFly, ((UI_ViewLocTab *)GetParent())->_activeFlyNames, 
                                       wxLB_HSCROLL|wxLB_NEEDED_SB, wxDefaultValidator, wxT("Active Flythrough Order"));    

   blank3 = new wxStaticText(this, -1, ""); //just a place holder
   blank4 = new wxStaticText(this, -1, ""); //just a place holder
   blank5 = new wxStaticText(this, -1, ""); //just a place holder
   blank6 = new wxStaticText(this, -1, ""); //just a place holder
   blank7 = new wxStaticText(this, -1, ""); //just a place holder

   _flyModCtrlsSizer = new wxBoxSizer(wxVERTICAL);
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


   _runactiveflyButton = new wxButton(this, VIEWLOC_RUNFLY_BUTTON, wxT("Start Active Flythrough"));

   _stopactiveflyButton = new wxButton(this, VIEWLOC_STOPFLY_BUTTON, wxT("Stop Flythrough"));

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

   SetAutoLayout(true);

   //assign the group to the panel
   SetSizer(viewlocPanelGroup);

   _activeflySel->SetValue( ((UI_ViewLocTab *)GetParent())->_flythroughName[ 0 ] );

}

///////////////////////////////////////
UI_ViewLocTabScroll::~UI_ViewLocTabScroll()
{

}

void UI_ViewLocTabScroll::_rebuildPage( void )
{
   _newVPControlsSizer->Detach(_removevwptSel);
   _newVPControlsSizer->Detach(_movetovwptSel);

   if ( _removevwptSel )
   {
      delete _removevwptSel;
      _removevwptSel = 0;
   }
   
   if ( _movetovwptSel )
   {
      delete _movetovwptSel;
      _movetovwptSel = 0;
   }

   _removevwptSel = new wxComboBox(this, VIEWLOC_REMOVEVP_COMBOBOX, wxT("Select a View Pt to delete"),wxDefaultPosition, 
                                 wxDefaultSize,((UI_ViewLocTab *)GetParent())->numStoredLocations, 
                                 ((UI_ViewLocTab *)GetParent())->_locationName, wxCB_READONLY);

   _movetovwptSel = new wxComboBox(this, VIEWLOC_MOVETOVP_COMBOBOX, wxT("Select a View Point"),wxDefaultPosition, 
                                 wxDefaultSize,((UI_ViewLocTab *)GetParent())->numStoredLocations, 
                                 ((UI_ViewLocTab *)GetParent())->_locationName, wxCB_READONLY);

   _newVPControlsSizer->Detach(_addnewviewptButton);
   _newVPControlsSizer->Detach(_newVPNameCtrlGroup);
   _newVPControlsSizer->Detach(blank1);
   _newVPControlsSizer->Detach(_removevwptLabel);
   _newVPControlsSizer->Detach(blank2);
   _newVPControlsSizer->Detach(_movetovwptLabel);

   _newVPControlsSizer->Add(_addnewviewptButton,1,wxALIGN_CENTER_HORIZONTAL);
   _newVPControlsSizer->Add(_newVPNameCtrlGroup,3,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   _newVPControlsSizer->Add(blank1,2,wxALIGN_CENTER_HORIZONTAL);
   _newVPControlsSizer->Add(_removevwptLabel,1,wxALIGN_LEFT|wxEXPAND);
   _newVPControlsSizer->Add(_removevwptSel,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   _newVPControlsSizer->Add(blank2,2,wxALIGN_CENTER_HORIZONTAL);
   _newVPControlsSizer->Add(_movetovwptLabel,1,wxALIGN_LEFT|wxEXPAND);
   _newVPControlsSizer->Add(_movetovwptSel,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

//*****************************Now rebuild flythrough controls
   _flyModCtrlsSizer->Detach(_activeflySel);
   _flyModCtrlsSizer->Detach(_addvptoflySel);
   _flyModCtrlsSizer->Detach(_insertvpinflySel);
   _flyModCtrlsSizer->Detach(_removevpfromflySel);
   _flyModCtrlsSizer->Detach(_deleteflySel);
   _flyModCtrlsSizer->Detach(_flybuilderListBox);

   if ( _activeflySel )
   {
      delete _activeflySel;
      _activeflySel = 0;
   }
   
   if ( _addvptoflySel )
   {
      delete _addvptoflySel;
      _addvptoflySel = 0;
   }

   if ( _insertvpinflySel )
   {
      delete _insertvpinflySel;
      _insertvpinflySel = 0;
   }
   
   if ( _removevpfromflySel )
   {
      delete _removevpfromflySel;
      _removevpfromflySel = 0;
   }

   if ( _deleteflySel )
   {
      delete _deleteflySel;
      _deleteflySel = 0;
   }
   
   if ( _flybuilderListBox )
   {
      delete _flybuilderListBox;
      _flybuilderListBox = 0;
   }
  
   _activeflySel = new wxComboBox(this, VIEWLOC_ACTIVEFLYSEL_COMBOBOX, wxT("Select Active Flythrough"),wxDefaultPosition, 
                                  wxDefaultSize, ((UI_ViewLocTab *)GetParent())->numStoredFlythroughs, 
                                  ((UI_ViewLocTab *)GetParent())->_flythroughName, wxCB_READONLY);


   _addvptoflySel = new wxComboBox(this, VIEWLOC_ADDVPTOFLYSEL_COMBOBOX, wxT("Select a View Point"),wxDefaultPosition, 
                                   wxDefaultSize, ((UI_ViewLocTab *)GetParent())->numStoredLocations, 
                                   ((UI_ViewLocTab *)GetParent())->_locationName, wxCB_READONLY);


   _insertvpinflySel = new wxComboBox(this, VIEWLOC_INSERTVPINFLYSEL_COMBOBOX, wxT("Select a View Point"),wxDefaultPosition, 
                                      wxDefaultSize, ((UI_ViewLocTab *)GetParent())->numStoredLocations, 
                                      ((UI_ViewLocTab *)GetParent())->_locationName, wxCB_READONLY);


   _removevpfromflySel = new wxComboBox(this, VIEWLOC_REMOVEVPFROMFLYSEL_COMBOBOX, wxT("Select a View Point"),wxDefaultPosition, 
                                        wxDefaultSize, ((UI_ViewLocTab *)GetParent())->_vwptsInActiveFly, 
                                        ((UI_ViewLocTab *)GetParent())->_activeFlyNames, wxCB_READONLY);

   _deleteflySel = new wxComboBox(this, VIEWLOC_DELETEFLYSEL_COMBOBOX, wxT("Select a Flythrough"),wxDefaultPosition, 
                                        wxDefaultSize, ((UI_ViewLocTab *)GetParent())->numStoredFlythroughs, 
                                        ((UI_ViewLocTab *)GetParent())->_flythroughName, wxCB_READONLY);

   _flybuilderListBox = new wxListBox(this, VIEWLOC_FLYBUILDER_LISTBOX, wxDefaultPosition, wxDefaultSize,
                                      ((UI_ViewLocTab *)GetParent())->_vwptsInActiveFly, ((UI_ViewLocTab *)GetParent())->_activeFlyNames, 
                                       wxLB_HSCROLL|wxLB_NEEDED_SB, wxDefaultValidator, wxT("Active Flythrough Order"));    

   _flyModCtrlsSizer->Detach(_activeflyLabel);
   _flyModCtrlsSizer->Detach(blank3);
   _flyModCtrlsSizer->Detach(_addvptoflyLabel);
   _flyModCtrlsSizer->Detach(blank4);
   _flyModCtrlsSizer->Detach(_insertvpinflyLabel);
   _flyModCtrlsSizer->Detach(blank5);
   _flyModCtrlsSizer->Detach(_removevpfromflyLabel);
   _flyModCtrlsSizer->Detach(blank6);
   _flyModCtrlsSizer->Detach(_deleteflyLabel);
   _flyModCtrlsSizer->Detach(blank7);


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


   Refresh(); 
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
}


BEGIN_EVENT_TABLE(UI_ViewLocTab, wxPanel)
   EVT_BUTTON(VIEWLOC_LOAD_BUTTON,UI_ViewLocTab::_onLoad)
   EVT_BUTTON(VIEWLOC_ACCEPTNEWVPNAME_BUTTON,UI_ViewLocTab::_onAcceptNewVPName)
   EVT_BUTTON(VIEWLOC_CANCELNEWVPNAME_BUTTON,UI_ViewLocTab::_onCancelNewVPName)
   EVT_COMBOBOX(VIEWLOC_REMOVEVP_COMBOBOX,UI_ViewLocTab::_onRemoveVP)
   EVT_COMBOBOX(VIEWLOC_MOVETOVP_COMBOBOX,UI_ViewLocTab::_onMoveToVP)
   EVT_BUTTON(VIEWLOC_NEWFLY_BUTTON,UI_ViewLocTab::_onBuildNewFlyButton)
   EVT_BUTTON(VIEWLOC_ACCEPTNEWFLYNAME_BUTTON,UI_ViewLocTab::_onAcceptNewFlyName)
   EVT_BUTTON(VIEWLOC_CANCELNEWFLYNAME_BUTTON,UI_ViewLocTab::_onCancelNewFlyName)
   EVT_COMBOBOX(VIEWLOC_ACTIVEFLYSEL_COMBOBOX,UI_ViewLocTab::_onActiveFlySel)
   EVT_COMBOBOX(VIEWLOC_ADDVPTOFLYSEL_COMBOBOX,UI_ViewLocTab::_onAddVPtoFlySel)
   EVT_COMBOBOX(VIEWLOC_INSERTVPINFLYSEL_COMBOBOX,UI_ViewLocTab::_onInsertVPinFlySel)
   EVT_COMBOBOX(VIEWLOC_REMOVEVPFROMFLYSEL_COMBOBOX,UI_ViewLocTab::_onRemoveVPfromFlySel)
   EVT_COMBOBOX(VIEWLOC_DELETEFLYSEL_COMBOBOX,UI_ViewLocTab::_onDeleteFlySel)
   EVT_BUTTON(VIEWLOC_RUNFLY_BUTTON,UI_ViewLocTab::_onStartActiveFly)
   EVT_BUTTON(VIEWLOC_STOPFLY_BUTTON,UI_ViewLocTab::_onStopFly)
   EVT_LISTBOX(VIEWLOC_FLYBUILDER_LISTBOX,UI_ViewLocTab::_onFlyBuilderListBox)
   EVT_COMMAND_SCROLL(VIEWLOC_SPEED_CONTROL_SLIDER, UI_ViewLocTab::_onSpeedChange )
END_EVENT_TABLE()

///////////////
//Constructor//
///////////////
UI_ViewLocTab::UI_ViewLocTab(wxNotebook* tControl)
:wxPanel(tControl)
{
   _parent = tControl;
   numStoredLocations = 0;
   numStoredFlythroughs = 0;
   _vwptsInActiveFly = 0;
   _locationName = 0;
   _flythroughName = 0;
   _activeFlyNames = 0;
   numView_LocsGlobal = 0;
   flyThroughList.clear();

   _buildPage();
}

UI_ViewLocTab::~UI_ViewLocTab( void )
{
   delete [] _locationName;
}
//////////////////////////////
//build the viewing locations tab       //
//////////////////////////////
void UI_ViewLocTab::_buildPage()
{

   numStoredLocations = ((UI_Tabs *)_parent)->num_viewlocs;

   for (CORBA::ULong j=0; j<((UI_Tabs *)_parent)->flyThroughArray->length(); j++ )
   {
      std::vector<int> tempPts;
      for (CORBA::ULong k=0; k<((UI_Tabs *)_parent)->flyThroughArray[j].length(); k++ )
      {
         tempPts.push_back( ((UI_Tabs *)_parent)->flyThroughArray[j][k] );
      }    
      flyThroughList.push_back(tempPts);
      tempPts.clear();
   }

   if(numStoredLocations < 0)
   {
      numStoredLocations = 0;
   }

   numView_LocsGlobal = numStoredLocations;

   _rebuildNameArrays();
   if ( flyThroughList.size() > 0 )
   {
      _setUpActiveFlyThroughNames( 0 );
   }

   wxBoxSizer* mainSizer = new wxBoxSizer(wxHORIZONTAL);

   _viewLocScroll = new UI_ViewLocTabScroll(this);

   mainSizer->Add( _viewLocScroll,1,wxALIGN_LEFT|wxEXPAND);

   SetAutoLayout(true);

   SetSizer( mainSizer );

   _viewLocScroll->_newVPNameCtrlBox->Enable( false );
   _viewLocScroll->_newvwptNameCtrl->Enable( false );
   _viewLocScroll->_newvwptNameOKButton->Enable( false );
   _viewLocScroll->_newvwptNameCancelButton->Enable( false );

   _viewLocScroll->_newFlyNameCtrlBox->Enable( false );
   _viewLocScroll->_newflythroughNameCtrl->Enable( false );
   _viewLocScroll->_newflythroughNameOKButton->Enable( false );
   _viewLocScroll->_newflythroughNameCancelButton->Enable( false );

}


void UI_ViewLocTab::_rebuildNameArrays( void )
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
   if( numStoredLocations > 0 )
   {
      _locationName = new wxString[ numStoredLocations ];

      for( int i=0; i<numStoredLocations; i++)
      {
         std::ostringstream vwptstream;
         vwptstream << "View Location " << i ;
         _locationName[i] = vwptstream.str().c_str();
      }
   }
   else
   {
      numStoredLocations = 1;
      _locationName = new wxString[1];
      _locationName[0] = wxT("No Stored Locations");
   }

   if( !flyThroughList.empty() )
   {
      numStoredFlythroughs = flyThroughList.size();
      _flythroughName = new wxString[ numStoredFlythroughs ];

      for( int i=0; i<numStoredFlythroughs; i++)
      {
         std::ostringstream flynamestream;
         flynamestream << "Flythrough " << i ;
         _flythroughName[i] = flynamestream.str().c_str();
      }
 
   }
   else
   {
      numStoredFlythroughs = 1;
      _flythroughName = new wxString[1];
      _flythroughName[0] = wxT("No Flythroughs Built");
      _vwptsInActiveFly = 1;
      _activeFlyNames = new wxString[ _vwptsInActiveFly ];
      _activeFlyNames[0] = wxT("No Flythroughs Built");
   }
}

void UI_ViewLocTab::_setUpActiveFlyThroughNames( int index )
{
   if ( _activeFlyNames )
   {
      delete [] _activeFlyNames;
   }

   _vwptsInActiveFly = flyThroughList.at( index ).size();
   _activeFlyNames = new wxString[ _vwptsInActiveFly ];

   for ( int i=0; i<_vwptsInActiveFly; i++ )
   {
      std::ostringstream activeflynamestream;
      activeflynamestream << "View Location " << flyThroughList.at( index ).at( i ) ;
      _activeFlyNames[i] = activeflynamestream.str().c_str();
   }
  
}

void UI_ViewLocTab::_updateWithcfdQuatCamHandler( void )
{
   int tempindex;
   for(int i=0;i<flyThroughList.size();i++)
   {
	   if( _viewLocScroll->_activeflySel->GetValue() == _flythroughName[i])
      {
         tempindex = i;
      }
   }  
   
   flyThroughList.clear();
   VjObs::double2DArray_var  flyThroughArray;

   if ( !CORBA::is_nil( ((UI_Tabs *)_parent)->server_ref ) )
   {
      VjObs::obj_pd_var tempTest;
      int tempTestlocal = 0;

      while ( tempTestlocal == 0 )
      {
         tempTest = ((UI_Tabs *)_parent)->server_ref->getDouble1D( "getCompletionTest" );
         tempTestlocal = tempTest[0];
         wxSleep( 0.5 );
      }
      numStoredLocations = ((UI_Tabs *)_parent)->server_ref->getIsoValue();
   }
 
   if ( !CORBA::is_nil( ((UI_Tabs *)_parent)->server_ref ) )
   {
      flyThroughArray = ((UI_Tabs *)_parent)->server_ref->getDouble2D( "getFlythroughData" );
   }

   for (CORBA::ULong j=0; j<flyThroughArray->length(); j++ )
   {
      std::vector<int> tempPts;
      for (CORBA::ULong k=0; k<flyThroughArray[j].length(); k++ )
      {
         tempPts.push_back( flyThroughArray[j][k] );
      }    
      flyThroughList.push_back(tempPts);
      tempPts.clear();
   }

   if(numStoredLocations < 0)
   {
      numStoredLocations = 0;
   }

   numView_LocsGlobal = numStoredLocations;
 
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
 
   _viewLocScroll->_rebuildPage();
   _viewLocScroll->_activeflySel->SetValue( _flythroughName[ tempindex ] );

}
//////////////////
//event handling//
///////////////////

//////////////////////////////////////////////////
void UI_ViewLocTab::_onLoad(wxCommandEvent& WXUNUSED(event))
{
   ((UI_Tabs *)_parent)->cId = LOAD_NEW_VIEWPT;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
   _updateWithcfdQuatCamHandler();
}

void UI_ViewLocTab::_onAcceptNewVPName(wxCommandEvent& WXUNUSED(event))
{
   //This will be used once user defined names are implemented
}

void UI_ViewLocTab::_onCancelNewVPName(wxCommandEvent& WXUNUSED(event))
{
   //This will be used once user defined names are implemented
}

void UI_ViewLocTab::_onRemoveVP(wxCommandEvent& WXUNUSED(event))
{
   if ( numView_LocsGlobal > 0 )
   {
      for(int i=0;i<numStoredLocations;i++)
	   {
		   if( _viewLocScroll->_removevwptSel->GetStringSelection() == _locationName[i])
         {
            ((UI_Tabs *)_parent)->cIso_value = i;
            ((UI_Tabs *)_parent)->cId = REMOVE_SELECTED_VIEWPT;
            ((UI_Tabs *)_parent)->sendDataArrayToServer();
         }
	   } 
      _updateWithcfdQuatCamHandler();
   }
}

void UI_ViewLocTab::_onMoveToVP(wxCommandEvent& WXUNUSED(event))
{
   if ( numView_LocsGlobal > 0 )
   {
      for(int i=0;i<numStoredLocations;i++)
	   {
		   if( _viewLocScroll->_movetovwptSel->GetStringSelection() == _locationName[i])
         {
            ((UI_Tabs *)_parent)->cIso_value = i;
            ((UI_Tabs *)_parent)->cId = MOVE_TO_SELECTED_LOCATION;
            ((UI_Tabs *)_parent)->sendDataArrayToServer();
         }
	   }
   }
}

void UI_ViewLocTab::_onBuildNewFlyButton(wxCommandEvent& WXUNUSED(event))
{
   if ( numView_LocsGlobal > 0 )
   {
      ((UI_Tabs *)_parent)->cId = ADD_NEW_FLYTHROUGH;
      ((UI_Tabs *)_parent)->sendDataArrayToServer();
      _updateWithcfdQuatCamHandler();
   }
}

void UI_ViewLocTab::_onAcceptNewFlyName(wxCommandEvent& WXUNUSED(event))
{
   //This will be used once user defined names are implemented
}

void UI_ViewLocTab::_onCancelNewFlyName(wxCommandEvent& WXUNUSED(event))
{
   //This will be used once user defined names are implemented
}

void UI_ViewLocTab::_onActiveFlySel(wxCommandEvent& WXUNUSED(event))
{
   if ( _flythroughName[ 0 ] != wxT("No Flythroughs Built") )
   {
      int tempindex;
      for( int i=0; i<flyThroughList.size(); i++ )
	   {
		   if( _viewLocScroll->_activeflySel->GetStringSelection() == _flythroughName[ i ])
         {
            _setUpActiveFlyThroughNames( i );
            tempindex = i;
         }
	   }
      _viewLocScroll->_rebuildPage();
      _viewLocScroll->_activeflySel->SetValue( _flythroughName[ tempindex ] );
   }
}

void UI_ViewLocTab::_onAddVPtoFlySel(wxCommandEvent& WXUNUSED(event))
{
   if ( numView_LocsGlobal > 0 )
   {
      if ( flyThroughList.size() > 0 )
      {
         for( unsigned int i=0; i<flyThroughList.size(); i++ )
	      {
		      if( _viewLocScroll->_activeflySel->GetValue() == _flythroughName[i])
            {
               for ( unsigned int j=0; j<numStoredLocations; j++ )
               {
                  if( _viewLocScroll->_addvptoflySel->GetStringSelection() == _locationName[j])
                  {
                     ((UI_Tabs *)_parent)->cIso_value = i;
                     ((UI_Tabs *)_parent)->cSc = j;
                     ((UI_Tabs *)_parent)->cId = ADD_NEW_POINT_TO_FLYTHROUGH;
                     ((UI_Tabs *)_parent)->sendDataArrayToServer();
                  }
               }
            }
	      }
         _updateWithcfdQuatCamHandler();
      }
   }
   
}

void UI_ViewLocTab::_onInsertVPinFlySel(wxCommandEvent& WXUNUSED(event))
{
   if ( numView_LocsGlobal > 0 )
   {
      if ( flyThroughList.size() > 0 )
      {
         for( unsigned int i=0; i<flyThroughList.size(); i++ )
	      {
		      if( _viewLocScroll->_activeflySel->GetValue() == _flythroughName[i])
            {
               for ( unsigned int j=0; j<flyThroughList.at( i ).size(); j++ )
               {
                  if( _viewLocScroll->_flybuilderListBox->IsSelected( j ) )
                  {
                     for ( unsigned int k=0; k<numStoredLocations; k++ )
                     {
                        if( _viewLocScroll->_insertvpinflySel->GetStringSelection() == _locationName[k])
                        {
                           ((UI_Tabs *)_parent)->cIso_value = i;
                           ((UI_Tabs *)_parent)->cSc = j;
                           ((UI_Tabs *)_parent)->cMin = k;
                           ((UI_Tabs *)_parent)->cId = INSERT_NEW_POINT_IN_FLYTHROUGH;
                           ((UI_Tabs *)_parent)->sendDataArrayToServer();
                        }
                     }
                  }
               }
            }
	      }
         _updateWithcfdQuatCamHandler();
      }
   }
}

void UI_ViewLocTab::_onRemoveVPfromFlySel(wxCommandEvent& WXUNUSED(event))
{
   if ( numView_LocsGlobal > 0 )
   {
      if ( flyThroughList.size() > 0 )
      {
         for( unsigned int i=0; i<flyThroughList.size(); i++ )
	      {
		      if( _viewLocScroll->_activeflySel->GetValue() == _flythroughName[i])
            {
               for ( unsigned int j=0; j<flyThroughList.at( i ).size(); j++ )
               {
                  if( _viewLocScroll->_removevpfromflySel->GetStringSelection() == _activeFlyNames[j])
                  {
                     ((UI_Tabs *)_parent)->cIso_value = i;
                     ((UI_Tabs *)_parent)->cSc = j;
                     ((UI_Tabs *)_parent)->cId = REMOVE_POINT_FROM_FLYTHROUGH;
                     ((UI_Tabs *)_parent)->sendDataArrayToServer();
                  }
               }
            }
	      }
         _updateWithcfdQuatCamHandler();
      }
   }

}

void UI_ViewLocTab::_onDeleteFlySel(wxCommandEvent& WXUNUSED(event))
{
   if ( flyThroughList.size() > 0 )
   {
      for( unsigned int i=0; i<flyThroughList.size(); i++ )
	   {
		   if( _viewLocScroll->_deleteflySel->GetStringSelection() == _flythroughName[i])
         {
            ((UI_Tabs *)_parent)->cIso_value = i;
            ((UI_Tabs *)_parent)->cId = DELETE_ENTIRE_FLYTHROUGH;
            ((UI_Tabs *)_parent)->sendDataArrayToServer();
         }
	   }
      _updateWithcfdQuatCamHandler();
   }

}

void UI_ViewLocTab::_onStartActiveFly(wxCommandEvent& WXUNUSED(event))
{
   if ( numView_LocsGlobal > 0 )
   {
      if ( flyThroughList.size() > 0 )
      {
         for( unsigned int i=0; i<flyThroughList.size(); i++ )
	      {
		      if( _viewLocScroll->_activeflySel->GetValue() == _flythroughName[i])
            {
               ((UI_Tabs *)_parent)->cIso_value = i;
               ((UI_Tabs *)_parent)->cId = RUN_ACTIVE_FLYTHROUGH;
               ((UI_Tabs *)_parent)->sendDataArrayToServer();
            }
	      }
      }
   }
}

void UI_ViewLocTab::_onStopFly(wxCommandEvent& WXUNUSED(event))
{
   ((UI_Tabs *)_parent)->cId = STOP_ACTIVE_FLYTHROUGH;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

void UI_ViewLocTab::_onFlyBuilderListBox(wxCommandEvent& WXUNUSED(event))
{
   
}

void UI_ViewLocTab::_onSpeedChange( wxScrollEvent& WXUNUSED(event) )
{
   ((UI_Tabs *)_parent)->cIso_value = _viewLocScroll->_speedCtrlSlider->GetValue();;
   ((UI_Tabs *)_parent)->cId = CHANGE_MOVEMENT_SPEED;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}
