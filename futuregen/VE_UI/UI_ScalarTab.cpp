#include <iostream>
#include "UI_ScalarTab.h"
#include "UI_Tabs.h"
#include "cfdEnum.h"
#include "UI_Frame.h"

BEGIN_EVENT_TABLE(UI_ScalarTab, wxScrolledWindow)
   EVT_RADIOBOX(SCALAR_RAD_BOX, UI_ScalarTab::_onScalars)
   EVT_BUTTON(SCALAR_UPDATE_BUTTON, UI_ScalarTab::_onUpdate)   
   EVT_COMMAND_SCROLL(MIN_PER_SLIDER, UI_ScalarTab::_onMinMaxSlider)
   EVT_COMMAND_SCROLL(MAX_PER_SLIDER, UI_ScalarTab::_onMinMaxSlider)
END_EVENT_TABLE()

/////////////////////////////////////////////////
//Constructor                                  //
/////////////////////////////////////////////////
UI_ScalarTab::UI_ScalarTab( wxWindow* tControl)
:wxScrolledWindow(tControl)
{
   _parent = tControl;
   _minPercentSlider = 0;
   _maxPercentSlider = 0;
   _scalarRBox = 0;
   _scalarRangeBox = 0;
   _visUpdateButton = 0;
   _nScalars = 0;
   _leftSizer = 0;

   //build the page
   _buildPage();
   
}
////////////////////////////////////////////////////////////////////////////////
UI_ScalarTab::UI_ScalarTab( wxWindow* tControl, int nScalars, char** names)
:wxScrolledWindow(tControl)
{
   _parent = tControl;
   _minPercentSlider = 0;
   _maxPercentSlider = 0;
   _scalarRBox = 0;
   _scalarRangeBox = 0;
   _visUpdateButton = 0;
   _nScalars = 0;
   _leftSizer = 0;

   //set the scalar information
   updateScalarTabRadioBoxInfo(nScalars,names);
   
}
///////////////////////////////////////////////////
void UI_ScalarTab::setActiveScalar(int whichScalar)
{
   if(_scalarRBox){
      _scalarRBox->SetSelection(whichScalar);
      ((UI_Frame *)_parent)->_tabs->cSc = whichScalar;         // using zero-based scalar counting
      ((UI_Frame *)_parent)->_tabs->cMin = _minPercentSlider->GetValue();
      ((UI_Frame *)_parent)->_tabs->cMax = _maxPercentSlider->GetValue();
      ((UI_Frame *)_parent)->_tabs->cId  = CHANGE_SCALAR;
      ((UI_Frame *)_parent)->_tabs->sendDataArrayToServer();
   }
}
////////////////////////////////////
//disable radio box               //
////////////////////////////////////
void UI_ScalarTab::disableRadioBox()
{
   if(_scalarRBox){
      _scalarRBox->Enable(FALSE);
   }
}
///////////////////////////////////
//enable radio box               //
///////////////////////////////////
void UI_ScalarTab::enableRadioBox()
{
   if(_scalarRBox){
      _scalarRBox->Enable(TRUE);
   }
}
///////////////////////////////////////////////////////////////////////////////////////////////
void UI_ScalarTab::updateScalarTabRadioBoxInfo(int nScalars, wxString* scalarNames,int refresh)
{
   if(!_scalarRBox){
      _buildPage();
   }
   if(nScalars){
      if(_scalarRBox){
	_leftSizer->Remove(_scalarRBox);
	//	RemoveChild(_scalarRBox);
	delete _scalarRBox;
      }
      //update number of scalars
      _nScalars = nScalars;

      int nCols = 1;

      //need this in case there are a ton of scalars
      if(nScalars> 10){
        //whole integer
        if(nScalars%10)nCols = nScalars/10;
        else nCols = nScalars/10 +1;
      }


      //create the radio box
      _scalarRBox = new wxRadioBox(this,SCALAR_RAD_BOX, wxT("Scalars"),
                                     wxDefaultPosition, wxDefaultSize,_nScalars,
                                     scalarNames,nCols,wxRA_SPECIFY_COLS);
      _leftSizer->Prepend(_scalarRBox,6,wxALIGN_LEFT|wxEXPAND|wxALIGN_TOP);
   }
   //ridiculous hack to get the display
   //to refresh!!!!!!!!
   SetSize(GetSize());

   // Send initial data to VE-Xplorer
   this->setActiveScalar( 0 );
}
////////////////////////////////////////////////////////////////////////////////////////////
void UI_ScalarTab::updateScalarTabRadioBoxInfo(int nScalars, char** scalarNames,int refresh)
{
   if(!_scalarRBox){
      _buildPage();
   }
   if(nScalars){
      if(_scalarRBox){
	_leftSizer->Remove(_scalarRBox);
	//	RemoveChild(_scalarRBox);
	delete _scalarRBox;
      }
      //update number of scalars
      _nScalars = nScalars;
      int nCols = 1;

      //need this in case there are a ton of scalars
      if(nScalars> 10){
        //whole integer
        if(nScalars%10)nCols = nScalars/10;
        else nCols = nScalars/10 +1;
      }
      wxString* newNames = new wxString[_nScalars];

      //copy the names
      for(int i = 0; i < _nScalars; i++){
         newNames[i] = scalarNames[i];
      }
      //create the radio box
      _scalarRBox = new wxRadioBox(this,SCALAR_RAD_BOX, wxT("Scalars"),
                                     wxDefaultPosition, wxDefaultSize,_nScalars,
                                     newNames,nCols,wxRA_SPECIFY_COLS);
      _leftSizer->Prepend(_scalarRBox,6,wxALIGN_LEFT|wxEXPAND|wxALIGN_TOP);
      if(newNames){
         delete [] newNames;
      }
   }

   if(refresh)
      _scalarRBox->Refresh();

   // Send initial data to VE-Xplorer
   this->setActiveScalar( 0 );
}
///////////////////////////////
//build the scalars page     //
///////////////////////////////
void UI_ScalarTab::_buildPage()
{
//NEW!!!!!!!!!!!!!!!!!!!!!!!! - from VecTab
   wxString vectorName[] = {wxT("default vector")};

   //////////////////////////////////
   //Design the "Scalars" radio box//
   //////////////////////////////////

   //default radio button
   wxString noScalarString[] = {wxT("No Scalars")};

   //Create the radio box w/ the list of scalar names if we have them
   _scalarRBox = new wxRadioBox(this,SCALAR_RAD_BOX, wxT("Scalars"),
                                     wxDefaultPosition, wxDefaultSize,1,
                                     noScalarString,1,wxRA_SPECIFY_COLS);

//NEW!!!!!!!!!!!!!!!!!! - from VecTab
   _vectorRBox = new wxRadioBox(this, VECTOR_RAD_BOX, wxT("Vectors"),
                                wxDefaultPosition, wxDefaultSize, 
            1, vectorName, 1, wxRA_SPECIFY_COLS);


   //The "Update Visualization" button
   //_visUpdateButton = new wxButton(this, SCALAR_UPDATE_BUTTON, wxT("Update"));

  
   //The static box for the scalar range sliders
   _scalarRangeBox = new wxStaticBox(this, -1, wxT("Scalar Range"));

   //need a sizer for this box
   //The items will be placed  next (horizontally) to other rather than on top of each other
   //(vertically)
   wxStaticBoxSizer* sRangeBoxSizer = new wxStaticBoxSizer(_scalarRangeBox,wxHORIZONTAL);
   

   //the labels for the sliders
   wxStaticText* minLabel = new wxStaticText(this, -1, wxT("Min%"));
   wxStaticText* maxLabel = new wxStaticText(this, -1, wxT("Max%"));

   //Size of the slider
   wxSize slidesize(50, 300);

   //create the two sliders
   _minPercentSlider = new wxSlider(this, MIN_PER_SLIDER,0,0,100,wxDefaultPosition, slidesize,
                                  wxSL_VERTICAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_RIGHT ); 
   _maxPercentSlider = new wxSlider(this, MAX_PER_SLIDER,100,0,100,wxDefaultPosition, slidesize,
                                  wxSL_VERTICAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_RIGHT ); 

/*  
   //create the two sliders
   _minPercentSlider = new wxSlider(this, MIN_PER_SLIDER,0,0,100,wxDefaultPosition, slidesize,
                                  wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_RIGHT ); 
   _maxPercentSlider = new wxSlider(this, MAX_PER_SLIDER,100,0,200,wxDefaultPosition, slidesize,
                                  wxSL_VERTICAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_RIGHT ); 

*/
 
   //two sizers to group the sliders and their lables
   wxBoxSizer* minGroup = new wxBoxSizer(wxVERTICAL); 
   wxBoxSizer* maxGroup = new wxBoxSizer(wxVERTICAL); 

   minGroup->Add(minLabel,0,wxALIGN_LEFT);
   minGroup->Add(_minPercentSlider,1,wxALIGN_LEFT);

   maxGroup->Add(maxLabel,0,wxALIGN_RIGHT);
   maxGroup->Add(_maxPercentSlider,1,wxALIGN_RIGHT);

   sRangeBoxSizer->Add(minGroup, 1, wxALIGN_LEFT|wxEXPAND); 
   sRangeBoxSizer->Add(maxGroup, 1, wxALIGN_RIGHT|wxEXPAND); 

   //The main sizer
   wxBoxSizer* scalarPanelGroup = new wxBoxSizer(wxHORIZONTAL);

   //a sizer for the radio box and the button
   //wxBoxSizer* leftSizer = new wxBoxSizer(wxVERTICAL); 
   if(!_leftSizer)_leftSizer = new wxBoxSizer(wxVERTICAL); 
   wxBoxSizer* rightSizer = new wxBoxSizer(wxVERTICAL);  
   
   //add the controls to the left group
   _leftSizer->Add(_scalarRBox,6,wxALIGN_LEFT|wxEXPAND);
   _leftSizer->Add( _vectorRBox,2,wxALIGN_LEFT|wxEXPAND);
   //_leftSizer->Add(_visUpdateButton,0,wxALIGN_CENTER_HORIZONTAL);
   
   //add the static box to the right group
   rightSizer->Add(sRangeBoxSizer,1,wxALIGN_LEFT|wxEXPAND);

   scalarPanelGroup->Add(_leftSizer,0,wxALIGN_LEFT|wxEXPAND);
   scalarPanelGroup->Add(rightSizer,0,wxALIGN_LEFT|wxEXPAND);
   
   //set this flag and let wx handle alignment
   SetAutoLayout(true);

   //assign the group to the panel
   SetSizer(scalarPanelGroup);

   // Send initial data to VE-Xplorer
   this->setActiveScalar( 0 );
}
////////////////////////////
//Event handling functions//
////////////////////////////
///////////////////////////////////////////////////
//Update button                                  //
///////////////////////////////////////////////////
void UI_ScalarTab::_onUpdate(wxCommandEvent& event)
{
   // Needs to update the currently selected vis 
   // choice from the vis tab
   ((UI_Frame *)_parent)->_tabs->cSc = _scalarRBox->GetSelection();         // using zero-based scalar counting
   ((UI_Frame *)_parent)->_tabs->cMin = _minPercentSlider->GetValue();
   ((UI_Frame *)_parent)->_tabs->cMax = _maxPercentSlider->GetValue();
   ((UI_Frame *)_parent)->_tabs->cId  = CHANGE_SCALAR;
   ((UI_Frame *)_parent)->_tabs->sendDataArrayToServer();
}
////////////////////////////////////////////////////
//Scalar radiobox                                 //
////////////////////////////////////////////////////
void UI_ScalarTab::_onScalars(wxCommandEvent& event)
{
   ((UI_Frame *)_parent)->_tabs->cSc = _scalarRBox->GetSelection();         // using zero-based scalar counting
   ((UI_Frame *)_parent)->_tabs->cMin = _minPercentSlider->GetValue();
   ((UI_Frame *)_parent)->_tabs->cMax = _maxPercentSlider->GetValue();
   ((UI_Frame *)_parent)->_tabs->cId  = CHANGE_SCALAR;
   ((UI_Frame *)_parent)->_tabs->sendDataArrayToServer();

   //update the scalar tab to correctly display the current scalar  
   //Commented by Yang to make it compile
   //   ((UI_Frame *)_parent)->changeActiveScalarOnDataset(_scalarRBox->GetStringSelection());  
   
}


void UI_ScalarTab::_onMinMaxSlider(wxScrollEvent& event)
{
   ((UI_Frame *)_parent)->_tabs->cSc = _scalarRBox->GetSelection();         // using zero-based scalar counting
   ((UI_Frame *)_parent)->_tabs->cMin = _minPercentSlider->GetValue();
   ((UI_Frame *)_parent)->_tabs->cMax = _maxPercentSlider->GetValue();
   ((UI_Frame *)_parent)->_tabs->cId  = CHANGE_SCALAR_RANGE;
   ((UI_Frame *)_parent)->_tabs->sendDataArrayToServer();
}
