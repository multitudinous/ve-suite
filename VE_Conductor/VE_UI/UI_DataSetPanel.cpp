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
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/VE_UI/UI_DataSetPanel.h"
#include "VE_Conductor/VE_UI/UI_Tabs.h"
#include "VE_Xplorer/cfdEnum.h"
#include <iostream>
#include "VE_Conductor/VE_UI/UI_Frame.h"
#include "VE_Conductor/VE_UI/UI_ModelData.h"
#include "VE_Builder/Utilities/gui/spinctld.h"

UI_Scalars::UI_Scalars(wxString* scalarName)
{
   range[ 0 ] = 0.0f;
   range[ 1 ] = 0.0f;
   lastMinSetting = 0.0f;
   lastMaxSetting = 0.0f;
   _thisScalarName = (*scalarName);
   std::cout << "\tScalar Name : " << _thisScalarName << std::endl;
}

UI_Scalars::~UI_Scalars()
{

}

UI_DataSets::UI_DataSets()
{

}

UI_DataSets::~UI_DataSets()
{
	int i;
   for (i=0; i<_numOfScalars; i++)
      delete _Scalars[i];
   _Scalars.clear();

   for (i=0; i<_numOfVectors; i++)
      delete _Vectors[i];
   _Vectors.clear();
}

void UI_DataSets::_buildScalars(int _numScalars, wxString* scalarNames, std::vector< std::pair<double, double> > input )
{
   _numOfScalars = _numScalars;

   for (int i=0; i<_numOfScalars; i++)
   {
      //thisScalar = new UI_Scalars(&scalarNames[i]);
      _Scalars.push_back( new UI_Scalars(&scalarNames[i]) );
      _Scalars.back()->range[ 0 ] = input.at( i ).first;
      _Scalars.back()->range[ 1 ] = input.at( i ).second;
      _Scalars.back()->lastMinSetting = _Scalars.back()->range[ 0 ]; 
      _Scalars.back()->lastMaxSetting = _Scalars.back()->range[ 1 ];
      std::cout << "\t\t" << scalarNames[i] << " : " << _Scalars.back()->range[ 0 ] << " : " << _Scalars.back()->range[ 1 ] << std::endl;
   }
}

void UI_DataSets::_buildVectors(int _numVectors, wxString* vectorNames)
{
   _numOfVectors = _numVectors;

   for (int i=0; i<_numOfVectors; i++)
   {
      thisVector = new UI_Scalars(&vectorNames[i]);
      _Vectors.push_back(thisVector);
   }
}




BEGIN_EVENT_TABLE(UI_DatasetScroll, wxScrolledWindow)
END_EVENT_TABLE()
//////////////////////////////////////////////////
//Constructor                                   //
//////////////////////////////////////////////////
UI_DatasetScroll::UI_DatasetScroll(wxWindow* parent)
:wxScrolledWindow(parent, -1, wxDefaultPosition, wxDefaultSize,
		    wxHSCROLL | wxVSCROLL)
{
  int nUnitX=20;
  int nUnitY=10;
  int nPixX = 5;
  int nPixY = 10;
  SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );

   _3dRBox = new wxRadioBox(this, RBOX_3D, wxT("3D mesh"), //0
                                wxDefaultPosition, wxDefaultSize,
				((UI_DatasetPanel*)GetParent())->_no3DMesh, 
            ((UI_DatasetPanel*)GetParent())->meshArrayNames,
            1, wxRA_SPECIFY_COLS);
   /*_vertexRBox = new wxRadioBox(this, VERTEX_RBOX, wxT("Vertex Data"), //1
                                wxDefaultPosition, wxDefaultSize,
				((UI_DatasetPanel*)GetParent())->_noVertex, 
            ((UI_DatasetPanel*)GetParent())->vertexArrayNames,
            1, wxRA_SPECIFY_COLS);
   _polydataRBox = new wxRadioBox(this, POLYDATA_RBOX, wxT("Polydata"), //2
                                wxDefaultPosition, wxDefaultSize,
				((UI_DatasetPanel*)GetParent())->_noPolydata,
            ((UI_DatasetPanel*)GetParent())->polydataArrayNames,
            1, wxRA_SPECIFY_COLS);*/

   _col = new wxBoxSizer(wxVERTICAL);
   _col->Add(_3dRBox,1,wxALL|wxEXPAND|wxALIGN_CENTER_HORIZONTAL,5);
   /*_col->Add(_vertexRBox,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   _col->Add(_polydataRBox,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);*/

   SetSizer(_col);
}

void UI_DatasetScroll::changeActiveDatasetType(int index)
{
   _col->Detach(_3dRBox);
   delete _3dRBox;
   
   if (index == 1)
   {
      _3dRBox = new wxRadioBox(this, VERTEX_RBOX, wxT("Vertex Data"), //1
                                wxDefaultPosition, wxDefaultSize,
				((UI_DatasetPanel*)GetParent())->_noVertex, 
            ((UI_DatasetPanel*)GetParent())->vertexArrayNames,
            1, wxRA_SPECIFY_COLS);
   }
   else if (index == 2)
   {
      _3dRBox = new wxRadioBox(this, POLYDATA_RBOX, wxT("Polydata"), //2
                                wxDefaultPosition, wxDefaultSize,
				((UI_DatasetPanel*)GetParent())->_noPolydata,
            ((UI_DatasetPanel*)GetParent())->polydataArrayNames,
            1, wxRA_SPECIFY_COLS);
   }
   else
   {
       _3dRBox = new wxRadioBox(this, RBOX_3D, wxT("3D mesh"), //0
                                wxDefaultPosition, wxDefaultSize,
				((UI_DatasetPanel*)GetParent())->_no3DMesh, 
            ((UI_DatasetPanel*)GetParent())->meshArrayNames,
            1, wxRA_SPECIFY_COLS);
   }

   _col->Add(_3dRBox,1,wxALL|wxEXPAND|wxALIGN_CENTER_HORIZONTAL,5);


   Refresh(); 
   //Complete Hack needed to get the page to refresh properly
   SetSize(GetSize());
   //SetSizer(_col);
}

UI_DatasetScroll::~UI_DatasetScroll()
{
}

BEGIN_EVENT_TABLE(UI_ScalarScroll, wxScrolledWindow)
END_EVENT_TABLE()
//////////////////////////////////////////////////
//Constructor                                   //
//////////////////////////////////////////////////
UI_ScalarScroll::UI_ScalarScroll(wxWindow* parent)
:wxScrolledWindow(parent, -1, wxDefaultPosition, wxDefaultSize,
		    wxHSCROLL | wxVSCROLL)
{
   wxString empty[1];
   empty[0] = wxT("No scalars");

   int nUnitX=10;
   int nUnitY=10;
   int nPixX = 10;
   int nPixY = 10;
   SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );

   

   _scalarRBox = new wxRadioBox(this,SCALAR_PANEL_RAD_BOX, wxT("Scalars"),
                                     wxDefaultPosition, wxDefaultSize,
                                     1,empty,1,wxRA_SPECIFY_COLS);

   _vectorRBox = new wxRadioBox(this, VECTOR_PANEL_RAD_BOX, wxT("Vectors"),
                                wxDefaultPosition, wxDefaultSize, 
                                1,empty,1,wxRA_SPECIFY_COLS);

   _col = new wxBoxSizer(wxVERTICAL);  
   _col->Add(_scalarRBox,2,wxALL|wxALIGN_LEFT|wxEXPAND,5);
   _col->Add(_vectorRBox,2,wxALL|wxALIGN_LEFT|wxEXPAND,5);

   SetSizer(_col);
}

UI_ScalarScroll::~UI_ScalarScroll()
{
}

void UI_ScalarScroll::rebuildRBoxes(UI_DataSets* activeDataSet)
{
   _col->Detach(_scalarRBox);
   _col->Detach(_vectorRBox);

   if ( _scalarRBox )
   {
      delete _scalarRBox;
      _scalarRBox = 0;
   }
   
   if ( _vectorRBox )
   {
      delete _vectorRBox;
      _vectorRBox = 0;
   }

   // Create radio boxes
   if ( activeDataSet->_numOfScalars != 0 )
   {
      _scalarRBox = new wxRadioBox(this,SCALAR_PANEL_RAD_BOX, wxT("Scalars"),
                                       wxDefaultPosition, wxDefaultSize,
                                       activeDataSet->_numOfScalars,
                                       ((UI_DatasetPanel*)GetParent())->_scalarNames,
                                       1,wxRA_SPECIFY_COLS);
   }
   else
   {
      wxString empty[1];
      empty[0] = wxT("No Scalars");
      _scalarRBox = new wxRadioBox(this, VECTOR_PANEL_RAD_BOX, wxT("Scalars"),
                        wxDefaultPosition, wxDefaultSize, 
                        1, empty, 1, wxRA_SPECIFY_COLS);
   }

   // Add to the sizer...
   if ( activeDataSet->_numOfScalars != 0 )
   {
      _col->Prepend(_scalarRBox,2,wxALL|wxALIGN_LEFT|wxEXPAND,5);
   }
   else
   {
      _col->Prepend(_scalarRBox,0,wxALL|wxALIGN_LEFT|wxEXPAND,5);
   }

   // Create radio boxes
   if ( activeDataSet->_numOfVectors != 0 )
   {
      _vectorRBox = new wxRadioBox(this, VECTOR_PANEL_RAD_BOX, wxT("Vectors"),
                                wxDefaultPosition, wxDefaultSize, 
            activeDataSet->_numOfVectors,
            ((UI_DatasetPanel*)GetParent())->_vectorNames,
            1, wxRA_SPECIFY_COLS);
   }
   else
   {
      wxString empty[1];
      empty[0] = wxT("No Vectors");
      _vectorRBox = new wxRadioBox(this, VECTOR_PANEL_RAD_BOX, wxT("Vectors"),
                        wxDefaultPosition, wxDefaultSize, 
                        1, empty, 1, wxRA_SPECIFY_COLS);
   }
 
   // Add to the sizer...
   if ( activeDataSet->_numOfVectors != 0 )
   {
      _col->Prepend(_vectorRBox,2,wxALL|wxALIGN_LEFT|wxEXPAND,5);
   }
   else
   {
      _col->Prepend(_vectorRBox,0,wxALL|wxALIGN_LEFT|wxEXPAND,5);
   }

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
   //SetSize(GetSize());

   // Update VE-Xplorer with new data
   if ( activeDataSet->_numOfScalars != 0 )
   {  
      ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->cSc = 0; // using zero-based scalar counting      
      ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->cMin = 
                     ((UI_DatasetPanel*)GetParent())->_minPercentSlider->GetValue();
      ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->cMax = 
                     ((UI_DatasetPanel*)GetParent())->_maxPercentSlider->GetValue();
      ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->cId  = CHANGE_SCALAR;
      ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->sendDataArrayToServer();
   }
   // Need to add vector support Update VE-Xplorer with new data
   if ( activeDataSet->_numOfVectors != 0 )
   {
      ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->cSc = 0; // using zero-based scalar counting      
      ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->cMin = 
                     ((UI_DatasetPanel*)GetParent())->_minPercentSlider->GetValue();
      ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->cMax = 
                     ((UI_DatasetPanel*)GetParent())->_maxPercentSlider->GetValue();
      ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->cId  = CHANGE_VECTOR;
      ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->sendDataArrayToServer();
   }
}


BEGIN_EVENT_TABLE(UI_DatasetPanel, wxPanel)
   EVT_COMBOBOX(DATA_SET_SELECT_COMBO, UI_DatasetPanel::_onActiveSelection)
   EVT_RADIOBOX(RBOX_3D, UI_DatasetPanel::_on3d)
   EVT_RADIOBOX(VERTEX_RBOX, UI_DatasetPanel::_onVertex)
   EVT_RADIOBOX(POLYDATA_RBOX, UI_DatasetPanel::_onPolyData)
   EVT_RADIOBOX(SCALAR_PANEL_RAD_BOX, UI_DatasetPanel::_onScalars)
   EVT_RADIOBOX(VECTOR_PANEL_RAD_BOX, UI_DatasetPanel::_onVectors)
   EVT_BUTTON(SCALAR_PANEL_UPDATE_BUTTON, UI_DatasetPanel::_onUpdate)   
#ifdef WIN32
   EVT_COMMAND_SCROLL_ENDSCROLL(MIN_PER_SLIDER_PANEL, UI_DatasetPanel::_onMinMaxSlider)
   EVT_COMMAND_SCROLL_ENDSCROLL(MAX_PER_SLIDER_PANEL, UI_DatasetPanel::_onMinMaxSlider)
#else
   EVT_COMMAND_SCROLL(MIN_PER_SLIDER_PANEL, UI_DatasetPanel::_onMinMaxSlider)
   EVT_COMMAND_SCROLL(MAX_PER_SLIDER_PANEL, UI_DatasetPanel::_onMinMaxSlider)
#endif
   EVT_COMMAND_SCROLL(MIN_SPIN_CNTL_BOX, UI_DatasetPanel::_onMinSpinCtrl)
   //EVT_TEXT_ENTER(MIN_SPIN_CNTL_BOX, UI_DatasetPanel::_onMinSpinCtrl)
   EVT_COMMAND_SCROLL(MAX_SPIN_CNTL_BOX, UI_DatasetPanel::_onMaxSpinCtrl)
   //EVT_TEXT_ENTER(MAX_SPIN_CNTL_BOX, UI_DatasetPanel::_onMaxSpinCtrl)
   //EVT_TEXT(MIN_SPIN_CNTL_BOX, UI_DatasetPanel::_onMinMaxSpinCtrl)
   //EVT_TEXT(MAX_SPIN_CNTL_BOX, UI_DatasetPanel::_onMinMaxSpinCtrl)
END_EVENT_TABLE()
//////////////////////////////////////////////////
//Constructor                                   //
//////////////////////////////////////////////////
//UI_DatasetPanel::UI_DatasetPanel(wxWindow* tControl)
UI_DatasetPanel::UI_DatasetPanel(wxWindow* tControl, UI_ModelData* _model, int activeMod)
:wxPanel(tControl)
{
   _modelData = _model;
   _activeModIndex = activeMod;
   _vectorNames = 0;
   _scalarNames = 0;

   _datasetCombo = NULL;
   
   _buildDataSets();
   _buildPanel(); 

   if ( !_DataSets.empty() )
	  _setScalars(_DataSets[0]);   
}
///////////////////////////////
//Destructor                 //
///////////////////////////////
UI_DatasetPanel::~UI_DatasetPanel()
{
  if(_scalarNames){  
      delete [] _scalarNames;
      _scalarNames = 0;
   }
   
   if(_vectorNames){
      delete [] _vectorNames;
      _vectorNames = 0;
   }
}
////////////////////////////////
//Build the Dataset page      //
////////////////////////////////
void UI_DatasetPanel::_buildPanel()
{ 
    _organizeRadioBoxInfo();
    //_organizeActiveRBox();
   /*_activeRBox = new wxRadioBox(this, ACTIVE_RBOX, wxT("Select Active Dataset Type"), 
                                wxDefaultPosition, wxDefaultSize,
				3, datatypes,1, wxRA_SPECIFY_COLS);*/
  
   _scalarNames = new wxString[1];
   _scalarNames[0] = wxT("No Scalars");  

   _datasetTypesel[0] = wxT("3D mesh");
   _datasetTypesel[1] = wxT("Vertex Data");
   _datasetTypesel[2] = wxT("Polydata");

   _datasetCombo = new wxComboBox(this, DATA_SET_SELECT_COMBO, wxT("Select Active Dataset Type"),
                                    wxDefaultPosition, wxDefaultSize,3,_datasetTypesel, wxCB_DROPDOWN);
   
   //The "Update Visualization" button
   //_visUpdateButton = new wxButton(this, SCALAR_PANEL_UPDATE_BUTTON, wxT("Update"),wxDefaultPosition,wxDefaultSize,wxBU_EXACTFIT);

   //The static box for the scalar range sliders
   _scalarRangeBox = new wxStaticBox(this, -1, wxT("Scalar Range"));

   //need a sizer for this box
   //The items will be placed  next (horizontally) to other rather than on top of each other
   //(vertically)
   sRangeBoxSizer = new wxStaticBoxSizer(_scalarRangeBox,wxVERTICAL);
     
   //the labels for the sliders
   minLabel = new wxStaticText(this, -1, wxT("Min%"));
   maxLabel = new wxStaticText(this, -1, wxT("Max%"));

   //create the two sliders
   _minPercentSlider = new wxSlider(this, MIN_PER_SLIDER_PANEL,0,0,100,wxDefaultPosition, wxDefaultSize,
                                  wxSL_AUTOTICKS|wxSL_LABELS); 
   _maxPercentSlider = new wxSlider(this, MAX_PER_SLIDER_PANEL,100,0,100,wxDefaultPosition, wxDefaultSize,
                                  wxSL_AUTOTICKS|wxSL_LABELS); 
//commented out before
   if ( !_DataSets.empty() )
   {
      //create the two spinners
      _minSpinner = new wxSpinCtrlDbl( *this, MIN_SPIN_CNTL_BOX, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0, 
                                   _DataSets[ 0 ]->_Scalars[ 0 ]->range[ 0 ],
                                   _DataSets[ 0 ]->_Scalars[ 0 ]->range[ 1 ],
                                   _DataSets[ 0 ]->_Scalars[ 0 ]->range[ 0 ], 
                                   0.25, -1, wxEmptyString);

      _maxSpinner = new wxSpinCtrlDbl( *this, MAX_SPIN_CNTL_BOX, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0, 
                                   _DataSets[ 0 ]->_Scalars[ 0 ]->range[ 0 ],
                                   _DataSets[ 0 ]->_Scalars[ 0 ]->range[ 1 ],
                                   _DataSets[ 0 ]->_Scalars[ 0 ]->range[ 1 ], 
                                   0.25, -1, wxEmptyString);
   }
else //commented from here to first comment
   {
      //create the two spinners
      _minSpinner = new wxSpinCtrlDbl( *this, MIN_SPIN_CNTL_BOX, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0, 
                                   0, 1, 1, 0.25, -1, wxEmptyString);

      _maxSpinner = new wxSpinCtrlDbl( *this, MAX_SPIN_CNTL_BOX, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0, 
                                   0,1,1, 0.25, -1, wxEmptyString);
      _minPercentSlider->Enable( false );  //set to false
      _maxPercentSlider->Enable( false);  //set to false
      _minSpinner->Enable( false);  //set to false
      _maxSpinner->Enable( false );  //set to false
   }
   //sizers to pull together the scalar adjustment controls
   scalgroupmin = new wxBoxSizer(wxHORIZONTAL);
   scalgroupspacer = new wxBoxSizer(wxHORIZONTAL);
   scalgroupmax = new wxBoxSizer(wxHORIZONTAL);

   minGroupwspin = new wxBoxSizer(wxVERTICAL);
   maxGroupwspin = new wxBoxSizer(wxVERTICAL);

   //two sizers to group the sliders and their lables
   minGroup = new wxBoxSizer(wxHORIZONTAL); 
   maxGroup = new wxBoxSizer(wxHORIZONTAL); 
  
   minGroup->Add(minLabel,0,wxALIGN_LEFT);
   minGroup->Add(_minPercentSlider,1,wxALIGN_RIGHT|wxEXPAND);

   minGroupwspin->Add(minGroup,2,wxALIGN_TOP|wxEXPAND);
   minGroupwspin->Add(_minSpinner,1,wxALIGN_BOTTOM|wxALIGN_CENTER);

   maxGroup->Add(maxLabel,0,wxALIGN_LEFT);
   maxGroup->Add(_maxPercentSlider,1,wxALIGN_RIGHT|wxEXPAND);

   maxGroupwspin->Add(maxGroup,2,wxALIGN_TOP|wxEXPAND);
   maxGroupwspin->Add(_maxSpinner,1,wxALIGN_BOTTOM|wxALIGN_CENTER);

   scalgroupmin->Add(minGroupwspin,1,wxALIGN_LEFT|wxEXPAND);
   scalgroupmax->Add(maxGroupwspin,1,wxALIGN_LEFT|wxEXPAND);

   sRangeBoxSizer->Add(scalgroupmin, 2, wxALIGN_LEFT|wxEXPAND);
   sRangeBoxSizer->Add(scalgroupspacer, 1, wxALIGN_LEFT|wxEXPAND);
   sRangeBoxSizer->Add(scalgroupmax, 2, wxALIGN_LEFT|wxEXPAND);
  
   _colcombine1_2 = new wxBoxSizer(wxHORIZONTAL);
   _mastercol1 = new wxBoxSizer(wxVERTICAL);
   
   _col1 = new wxBoxSizer(wxVERTICAL);
   _col2 = new wxBoxSizer(wxVERTICAL);
   //_col3 = new wxBoxSizer(wxHORIZONTAL);
   //_col4 = new wxBoxSizer(wxVERTICAL);

   //new layout
   //Create the radio box w/ the list of scalar names if we have them
   _RBoxScroll = new UI_DatasetScroll(this);
   _ScalarScroll = new UI_ScalarScroll(this);

   _col1->Add(_RBoxScroll,1,wxALL|wxEXPAND|wxALIGN_CENTER_HORIZONTAL,5);
   
   _col2->Add(_ScalarScroll,1,wxALL|wxALIGN_LEFT|wxEXPAND,5);

   _colcombine1_2->Add(_col1,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   _colcombine1_2->Add(_col2,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   _mastercol1->Add(_datasetCombo,0,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   _mastercol1->Add(_colcombine1_2,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   //_col3->Add(sRangeBoxSizer,1,wxALL|wxALIGN_LEFT|wxEXPAND,5);

   //_col4->Add(_visUpdateButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   _dataHeadingBox = new wxStaticBox(this, -1, wxT("Data and Scalar Set Selection"));

   dHeadingBoxSizer = new wxStaticBoxSizer(_dataHeadingBox,wxVERTICAL);

   dHeadingBoxSizer->Add(_mastercol1,8,wxEXPAND|wxALIGN_CENTER_VERTICAL);
   dHeadingBoxSizer->Add(sRangeBoxSizer,4,wxALL|wxEXPAND|wxALIGN_CENTER_VERTICAL,5);
   //dHeadingBoxSizer->Add(_col4,2,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   datasetPanelGroup = new wxBoxSizer(wxHORIZONTAL);

   datasetPanelGroup->Add(dHeadingBoxSizer,4,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   //set this flag and let wx handle alignment
   SetAutoLayout(true);

   //assign the group to the panel
   SetSizer(datasetPanelGroup);

   //_RBoxScroll->_3dRBox->Enable(TRUE);
   //_RBoxScroll->_vertexRBox->Enable(FALSE);
   //_RBoxScroll->_polydataRBox->Enable(FALSE);

   for (int i=0; i<_numSteadyStateDataSets; i++)
   {
	   if ( _DataSets.empty() )
	   {
         _setScalarsnoDatasets();
	   }
	   else if( _RBoxScroll->_3dRBox->GetString(_RBoxScroll->_3dRBox->GetSelection()) == _DataSets[i]->_dataSetName )
      {
         _setScalars(_DataSets[i]);
         break;
      }
      else
         std::cerr << " ERROR : Problem with UI_DatasetPanel::_buildPanel " << std::endl;
         
   }
}

void UI_DatasetPanel::_buildDataSets( void )
{
	int k;
   if ( !_DataSets.empty())
   {
      std::cout<<"DataSets successfully cleared"<<std::endl;
      for (int i=0; i<_numSteadyStateDataSets; i++)
         delete _DataSets[i];
      _DataSets.clear();
   }

   _numSteadyStateDataSets = _modelData->GetNubmerofDataSets(_activeModIndex);

   std::cout<<"numdatasets: "<<_numSteadyStateDataSets<<std::endl;

   //CORBA::ULong indexScalar = 0;
   //CORBA::ULong indexVector = 0;
      
   if (_numSteadyStateDataSets > 0)
   {
      VjObs::Datasets dataVector = 
            VjObs::Datasets( *_modelData->GetDataSets(_activeModIndex) );
      
      /*VjObs::scalar_p datasetNames = 
            VjObs::scalar_p( *_modelData->GetDataSetNames(_activeModIndex) );*/

      VjObs::obj_p   datasetTypes = 
            VjObs::obj_p( *_modelData->GetDataSetTypes(_activeModIndex) );
      
      VjObs::obj_p   numScalarsPerDataset = 
            VjObs::obj_p( *_modelData->GetNumberOfScalarsPerDataSet(_activeModIndex) );
      
      VjObs::obj_p   numVectorsPerDataset = 
            VjObs::obj_p( *_modelData->GetNumberOfVectorsPerDataSet(_activeModIndex) );

      /*VjObs::scalar_p scalarNames = 
            VjObs::scalar_p( *_modelData->GetScalarNames(_activeModIndex) );  

      VjObs::scalar_p vectorNames = 
            VjObs::scalar_p( *_modelData->GetVectorNames(_activeModIndex) );  
*/
      for (CORBA::ULong i = 0; i<(unsigned int)_numSteadyStateDataSets; i++)
      {
         _DataSets.push_back( new UI_DataSets() );
         
         _DataSets.at( i )->_dataSetName = dataVector[ i ].datasetname; //datasetNames[i];
         std::cout << " DataSet Name[ " << i << " ] = " 
                     << _DataSets.at( i )->_dataSetName << std::endl;
         _DataSets.at( i )->_dataSetType = datasetTypes[i];

		   wxString* thisDataScalarNames;
		   thisDataScalarNames = new wxString[numScalarsPerDataset[i]];

		   wxString* thisDataVectorNames;
		   thisDataVectorNames = new wxString[numVectorsPerDataset[i]];

         // another for loop to construct per dataset scalar names
         std::vector< std::pair< double, double > > ranges;
         for (k=0; k<numScalarsPerDataset[i]; k++)
         {
            CORBA::ULong nameNumber = k;
            thisDataScalarNames[k] = dataVector[ i ].scalarVector[ nameNumber ].scalarnames;
            ranges.push_back
               ( 
                  std::pair< double, double >
                  ( (double)dataVector[ i ].scalarVector[ nameNumber ].scalarrange[ 0 ], (double)dataVector[ i ].scalarVector[ nameNumber ].scalarrange[ 1 ] ) 
               );
         }

         // another for loop to construct per dataset vector names
         for (k=0; k<numVectorsPerDataset[i]; k++)
         {
            CORBA::ULong nameNumber = k;
            thisDataVectorNames[k] = dataVector[ i ].vectornames[ nameNumber ];
         }
        

         _DataSets.at( i )->_buildScalars(numScalarsPerDataset[i], thisDataScalarNames, ranges);
         _DataSets.at( i )->_buildVectors(numVectorsPerDataset[i], thisDataVectorNames);
         
         //clean up the names array
         delete [] thisDataScalarNames;                     
         delete [] thisDataVectorNames;                     
      }
      // Need to add in same functionality for vectors as is done for scalars   
   } 
}


void UI_DatasetPanel::_rebuildDataSets( int _activeMod )
{
///////////////////////////////////////////////////////////////////////////////////////////
   minGroup->Detach(minLabel);
   minGroup->Detach(_minPercentSlider);
   maxGroup->Detach(maxLabel);
   maxGroup->Detach(_maxPercentSlider);
   sRangeBoxSizer->Detach(_scalarRangeBox);
   sRangeBoxSizer->Detach(minGroup); 
   sRangeBoxSizer->Detach(maxGroup);
   _col1->Detach(_RBoxScroll);
   _col2->Detach(_ScalarScroll);
   _colcombine1_2->Detach(_col1);
   _colcombine1_2->Detach(_col2);
   _mastercol1->Detach(_datasetCombo);
   _mastercol1->Detach(_colcombine1_2);
   //_col3->Detach(sRangeBoxSizer);
   //_col4->Detach(_visUpdateButton);
   dHeadingBoxSizer->Detach(_mastercol1);
   dHeadingBoxSizer->Detach(sRangeBoxSizer);
   //dHeadingBoxSizer->Detach(_col4);
   datasetPanelGroup->Detach(dHeadingBoxSizer);
   delete _RBoxScroll;
   
   if ( _scalarNames )
   {
      delete [] _scalarNames;
      _scalarNames = 0;
   }
   
   if ( _vectorNames )
   {
      delete [] _vectorNames;
      _vectorNames = 0;
   }
   delete _ScalarScroll;
   delete _datasetCombo;
   //delete _visUpdateButton;
   delete _scalarRangeBox;
   delete minLabel;
   delete maxLabel;
   delete _minPercentSlider; 
   delete _maxPercentSlider; 
   delete _dataHeadingBox;

   ////////////////////////////////////////////
   ////////////////////////////////////////////
   _activeModIndex = _activeMod;

   _buildDataSets();
   _buildPanel(); 

   if ( !_DataSets.empty() )
	  _setScalars(_DataSets[0]);

}

void UI_DatasetPanel::_setScalars(UI_DataSets* activeDataSet)
{
	int i;
   // This function is guarded above if there are no datasets   
   if(_scalarNames)
   {  
      delete [] _scalarNames;
      _scalarNames = 0;
   }
   
   if(_vectorNames)
   {
      delete [] _vectorNames;
      _vectorNames = 0;
   }

   _scalarNames = new wxString[activeDataSet->_numOfScalars];
   _vectorNames = new wxString[activeDataSet->_numOfVectors];
   
   for (i=0; i<activeDataSet->_numOfScalars; i++)
   {
      _scalarNames[i] = activeDataSet->_Scalars[i]->_thisScalarName;
   }

   for (i=0; i<activeDataSet->_numOfVectors; i++)
   {
      _vectorNames[i] = activeDataSet->_Vectors[i]->_thisScalarName;
   }

   _ScalarScroll->rebuildRBoxes(activeDataSet);

   Refresh();
   SetSize(GetSize());
}

void UI_DatasetPanel::_setScalarsnoDatasets()
{
   wxString empty[1];
   empty[0] = wxT("No scalars");
   _col2->Detach(_ScalarScroll);

   delete _ScalarScroll;
   _ScalarScroll = new UI_ScalarScroll(this);

   _col2->Prepend(_ScalarScroll,2,wxALIGN_LEFT|wxEXPAND);

   Refresh(); 
   //Complete Hack needed to get the page to refresh properly
   SetSize(GetSize());
}

void UI_DatasetPanel::_organizeRadioBoxInfo()
{
   _no3DMesh = 0;
   _noVertex = 0;
   _noPolydata = 0;
   wxString* dummy;
   std::vector<wxString> meshArray;
   std::vector<wxString> vertexArray;
   std::vector<wxString> polydataArray;

   if(_numSteadyStateDataSets >0)
   {
      for(int i=0; i<_numSteadyStateDataSets; i++)
      {
            if(_DataSets[i]->_dataSetType == 0)
            {
               _no3DMesh += 1;
               meshArray.push_back(_DataSets[i]->_dataSetName);
            }
            else if(_DataSets[i]->_dataSetType == 1)
            {
               _noVertex += 1;
               vertexArray.push_back(_DataSets[i]->_dataSetName);
            }
            else if(_DataSets[i]->_dataSetType == 2)
            {
               _noPolydata += 1;
               polydataArray.push_back(_DataSets[i]->_dataSetName);
            }
      }   
   }
   else
   {
      _numSteadyStateDataSets = 1;
      dummy = new wxString[ _numSteadyStateDataSets ];
      dummy[0] = wxT("no datasets");
      meshArray.push_back(dummy[0]);
      vertexArray.push_back(dummy[0]);
      polydataArray.push_back(dummy[0]);
      _no3DMesh = 1; 
      _noVertex = 1; 
      _noPolydata = 1;
   }


   if (_no3DMesh > 0)
   {
      meshArrayNames = new wxString[_no3DMesh];
      for(int i=0; i< _no3DMesh; i++)
         meshArrayNames[i] = meshArray[i];
   }
   else
   {
      _no3DMesh = 1;
      meshArrayNames = new wxString[_no3DMesh];
      meshArrayNames[0] = wxT("no datasets");  
   }

   if (_noVertex > 0)    
   {
      vertexArrayNames = new wxString[_noVertex];
      for(int i=0; i< _noVertex; i++)
         vertexArrayNames[i] = vertexArray[i]; 
   }
   else
   {
      _noVertex = 1;
      vertexArrayNames = new wxString[_noVertex];
      vertexArrayNames[0] = wxT("no datasets");  
   }
 

   if (_noPolydata > 0)
   {
      polydataArrayNames = new wxString[_noPolydata];
      for(int j=0; j< _noPolydata; j++)
         polydataArrayNames[j] = polydataArray[j];
   }
   else
   {
      _noPolydata = 1;
      polydataArrayNames = new wxString[_noPolydata];
      polydataArrayNames[0] = wxT("no datasets");  
   }
}

/*void UI_DatasetPanel::_organizeActiveRBox()
{
   if (_numSteadyStateDataSets >0)
   {  
      if(_DataSets[0]->_dataSetType == 0)
      {
         _RBoxScroll->_3dRBox->Enable(TRUE);
         _RBoxScroll->_vertexRBox->Enable(FALSE);
         _RBoxScroll->_polydataRBox->Enable(FALSE);
         
         datatypes[0] = wxT("3D mesh");
         datatypes[1] = wxT("Vertex Data");
         datatypes[2] = wxT("Polydata");
      }
      else if(_DataSets[0]->_dataSetType == 1)
      {
         _RBoxScroll->_3dRBox->Enable(FALSE);
         _RBoxScroll->_vertexRBox->Enable(TRUE);
         _RBoxScroll->_polydataRBox->Enable(FALSE);
         datatypes[1] = wxT("3D mesh");
         datatypes[0] = wxT("Vertex Data");
         datatypes[2] = wxT("Polydata");
      }
      else if(_DataSets[0]->_dataSetType == 2)
      {
         _RBoxScroll->_3dRBox->Enable(FALSE);
         _RBoxScroll->_vertexRBox->Enable(FALSE);
         _RBoxScroll->_polydataRBox->Enable(TRUE);
         datatypes[1] = wxT("3D mesh");
         datatypes[2] = wxT("Vertex Data");
         datatypes[0] = wxT("Polydata");
      }     
   }
   else
   {
      _RBoxScroll->_3dRBox->Enable(FALSE);
      _RBoxScroll->_vertexRBox->Enable(FALSE);
      _RBoxScroll->_polydataRBox->Enable(FALSE);
      datatypes[0] = wxT("No Datasets Available");

   }

}*/

void UI_DatasetPanel::_resetScalarAdjustment( int dataSetSelected, int scalarSetSelected )
{
   if ( _minPercentSlider )
   {
   minGroup->Detach( _minPercentSlider );
   delete _minPercentSlider;
   _minPercentSlider = 0;
   }

   if ( _minSpinner )
   {
   minGroupwspin->Detach( _minSpinner );
   delete _minSpinner;
   _minSpinner = 0;
   }

   if ( _maxPercentSlider )
   {
   maxGroup->Detach( _maxPercentSlider );
   delete _maxPercentSlider;
   _maxPercentSlider = 0;
   }

   if ( _maxSpinner )
   {
   maxGroupwspin->Detach( _maxSpinner );
   delete _maxSpinner;
   _maxSpinner = 0;
   }

   if ( !_DataSets.empty() )
   {
      double minScalar = _DataSets[ dataSetSelected ]->_Scalars[ scalarSetSelected ]->range[ 0 ];
      double maxScalar = _DataSets[ dataSetSelected ]->_Scalars[ scalarSetSelected ]->range[ 1 ];

      double tempminslid = 0;
      double tempmaxslid = 0;

      if ( minScalar == maxScalar )
      {
         tempminslid = minScalar;
         tempmaxslid = maxScalar;
      }
      else
      {
         tempminslid =  
            (_DataSets[ dataSetSelected ]->_Scalars[ scalarSetSelected ]->lastMinSetting -
            minScalar ) / ( maxScalar - minScalar ) * 100;  

         tempmaxslid =
            ( (maxScalar - minScalar) - ( maxScalar -
            _DataSets[ dataSetSelected ]->_Scalars[ scalarSetSelected ]->lastMaxSetting) ) /
            ( maxScalar - minScalar) * 100; 
      }

      //create the two sliders
      _minPercentSlider = new wxSlider(this, MIN_PER_SLIDER_PANEL,(int)tempminslid,0,100,wxDefaultPosition, wxDefaultSize,
                                  wxSL_HORIZONTAL|wxSL_LABELS ); 
      _maxPercentSlider = new wxSlider(this, MAX_PER_SLIDER_PANEL,(int)tempmaxslid,0,100,wxDefaultPosition, wxDefaultSize,
                                  wxSL_HORIZONTAL|wxSL_LABELS ); 

      //create the two spinners
      _minSpinner = new wxSpinCtrlDbl( *this, MIN_SPIN_CNTL_BOX, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0, 
                                   minScalar, maxScalar,
                                   _DataSets[ dataSetSelected ]->_Scalars[ scalarSetSelected ]->lastMinSetting , 
                                   0.25, -1, wxEmptyString);

      _maxSpinner = new wxSpinCtrlDbl( *this, MAX_SPIN_CNTL_BOX, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0, 
                                   minScalar, maxScalar,
                                   _DataSets[ dataSetSelected ]->_Scalars[ scalarSetSelected ]->lastMaxSetting ,
                                   0.25, -1, wxEmptyString);
   }
   else
   {
      //create the two sliders
      _minPercentSlider = new wxSlider(this, MIN_PER_SLIDER_PANEL,0,0,100,wxDefaultPosition, wxDefaultSize,
                                  wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS ); 
      _maxPercentSlider = new wxSlider(this, MAX_PER_SLIDER_PANEL,100,0,100,wxDefaultPosition, wxDefaultSize,
                                  wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS ); 

      //create the two spinners
      _minSpinner = new wxSpinCtrlDbl( *this, MIN_SPIN_CNTL_BOX, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0, 
                                   0, 1,
                                   1 , 
                                   0.25, -1, wxEmptyString);

      _maxSpinner = new wxSpinCtrlDbl( *this, MAX_SPIN_CNTL_BOX, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0, 
                                   0, 1,
                                   1 ,
                                   0.25, -1, wxEmptyString);
      _minPercentSlider->Enable( false );
      _maxPercentSlider->Enable( false );
      _minSpinner->Enable( false );
      _maxSpinner->Enable( false );
   }

   minGroup->Insert(1,_minPercentSlider,1,wxALIGN_RIGHT|wxEXPAND);   
   minGroupwspin->Insert(1,_minSpinner,1,wxALIGN_BOTTOM|wxALIGN_CENTER);

   maxGroup->Insert(1,_maxPercentSlider,1,wxALIGN_RIGHT|wxEXPAND);
   maxGroupwspin->Insert(1,_maxSpinner,1,wxALIGN_BOTTOM|wxALIGN_CENTER);

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

void UI_DatasetPanel::_onActiveSelection(wxCommandEvent& WXUNUSED(event))
{
   if ( _DataSets.empty() )
      return;

   _RBoxScroll->changeActiveDatasetType(_datasetCombo->GetSelection());

   for (int i=0; i<_numSteadyStateDataSets; i++)
   {
      std::cout << _RBoxScroll->_3dRBox->GetString(_RBoxScroll->_3dRBox->GetSelection()) << " : " << _DataSets[i]->_dataSetName<<std::endl;
      if ( _DataSets.empty() )
	   {
         _setScalarsnoDatasets();
	   }
	   else if( _RBoxScroll->_3dRBox->GetString(_RBoxScroll->_3dRBox->GetSelection()) == _DataSets[i]->_dataSetName )
      {
         ((UI_Frame*)GetParent())->_tabs->setActiveDataset(i);
         _setScalars(_DataSets[i]);
         break;
      }
      else
	   {
         std::cerr << " ERROR : Problem with UI_DatasetPanel::_buildPanel " << std::endl;
		   _setScalarsnoDatasets();
	   }
   }

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
   //Refresh();      
   //Complete Hack needed to get the page to refresh properly
   //SetSize(GetSize());
   
   /*if ( _datasetCombo->GetSelection() == 0 || _datasetCombo->GetSelection() == -1)
   {  
      _RBoxScroll->_3dRBox->Enable(TRUE);
      _RBoxScroll->_vertexRBox->Enable(FALSE);
      _RBoxScroll->_polydataRBox->Enable(FALSE);
      for (int i=0; i<_numSteadyStateDataSets; i++)
         if(_DataSets[i]->_dataSetType == 0 && _RBoxScroll->_3dRBox->GetString(_RBoxScroll->_3dRBox->GetSelection()) == _DataSets[i]->_dataSetName)
         {
            _setScalars(_DataSets[i]);
            i = _numSteadyStateDataSets;
         }
         else
            _setScalarsnoDatasets();
   }
   if ( _datasetCombo->GetSelection() == 1)
   {  
      _RBoxScroll->_3dRBox->Enable(FALSE);
      _RBoxScroll->_vertexRBox->Enable(TRUE);
      _RBoxScroll->_polydataRBox->Enable(FALSE);
      for (int i=0; i<_numSteadyStateDataSets; i++)
         if(_DataSets[i]->_dataSetType == 1 && _RBoxScroll->_vertexRBox->GetString(_RBoxScroll->_vertexRBox->GetSelection()) == _DataSets[i]->_dataSetName)
         {
            _setScalars(_DataSets[i]);
            i = _numSteadyStateDataSets;
         }
         else
            _setScalarsnoDatasets();
   }
   if ( _datasetCombo->GetSelection() == 2)
   {  
      _RBoxScroll->_3dRBox->Enable(FALSE);
      _RBoxScroll->_vertexRBox->Enable(FALSE);
      _RBoxScroll->_polydataRBox->Enable(TRUE);
      for (int i=0; i<_numSteadyStateDataSets; i++)
         if(_DataSets[i]->_dataSetType == 2 && _RBoxScroll->_polydataRBox->GetString(_RBoxScroll->_polydataRBox->GetSelection()) == _DataSets[i]->_dataSetName)
         {
            _setScalars(_DataSets[i]);
            i = _numSteadyStateDataSets;
         }
         else
            _setScalarsnoDatasets();
   }*/
}

void UI_DatasetPanel::_on3d(wxCommandEvent& WXUNUSED(event))
{
   if ( _DataSets.empty() )
      return;

   for (int i=0; i<_numSteadyStateDataSets; i++)
      if (_DataSets[i]->_dataSetType == 0)
         if (_RBoxScroll->_3dRBox->GetString(_RBoxScroll->_3dRBox->GetSelection()) == _DataSets[i]->_dataSetName)
         {
            _setScalars(_DataSets[i]);
            ((UI_Frame*)GetParent())->_tabs->setActiveDataset(i);
         }           
}

void UI_DatasetPanel::_onVertex(wxCommandEvent& WXUNUSED(event))
{
   if ( _DataSets.empty() )
      return;

   for (int i=0; i<_numSteadyStateDataSets; i++)
      if (_DataSets[i]->_dataSetType == 1)
         if (_RBoxScroll->_3dRBox->GetString(_RBoxScroll->_3dRBox->GetSelection()) == _DataSets[i]->_dataSetName)
         {
            _setScalars(_DataSets[i]);
            ((UI_Frame*)GetParent())->_tabs->setActiveDataset(i);
         }  
}

void UI_DatasetPanel::_onPolyData(wxCommandEvent& WXUNUSED(event))
{
   if ( _DataSets.empty() )
      return;

   for (int i=0; i<_numSteadyStateDataSets; i++)
      if (_DataSets[i]->_dataSetType == 2)
         if (_RBoxScroll->_3dRBox->GetString(_RBoxScroll->_3dRBox->GetSelection()) == _DataSets[i]->_dataSetName)
         {
            _setScalars(_DataSets[i]);
            ((UI_Frame*)GetParent())->_tabs->setActiveDataset(i);
         }
}

void UI_DatasetPanel::_onVectors( wxCommandEvent& WXUNUSED(event) )
{
   if ( _DataSets.empty() )
      return;

   ((UI_Frame *)GetParent())->_tabs->cSc = _ScalarScroll->_vectorRBox->GetSelection();         // using zero-based scalar counting
   ((UI_Frame *)GetParent())->_tabs->cMin = _minPercentSlider->GetValue();
   ((UI_Frame *)GetParent())->_tabs->cMax = _maxPercentSlider->GetValue();
   ((UI_Frame *)GetParent())->_tabs->cId  = CHANGE_VECTOR;
   ((UI_Frame *)GetParent())->_tabs->sendDataArrayToServer();
}

void UI_DatasetPanel::_onScalars(wxCommandEvent& WXUNUSED(event))
{
   if ( _DataSets.empty() )
      return;

   _resetScalarAdjustment( _RBoxScroll->_3dRBox->GetSelection(), _ScalarScroll->_scalarRBox->GetSelection() );

   ((UI_Frame *)GetParent())->_tabs->cId  = CHANGE_SCALAR;
   ((UI_Frame *)GetParent())->_tabs->cSc  = _ScalarScroll->_scalarRBox->GetSelection();         // using zero-based scalar counting
   ((UI_Frame *)GetParent())->_tabs->cMin = _minPercentSlider->GetValue();
   ((UI_Frame *)GetParent())->_tabs->cMax = _maxPercentSlider->GetValue();
   ((UI_Frame *)GetParent())->_tabs->sendDataArrayToServer();

   // this is a hack until we get the gui reworked
   ((UI_Frame *)GetParent())->_tabs->cId        = CHANGE_VECTOR_THRESHOLD;
   ((UI_Frame *)GetParent())->_tabs->cMin       = 0;
   ((UI_Frame *)GetParent())->_tabs->cMax       = 100;
   ((UI_Frame *)GetParent())->_tabs->sendDataArrayToServer();   
}

void UI_DatasetPanel::_onUpdate(wxCommandEvent& WXUNUSED(event))
{
   if ( _DataSets.empty() )
      return;

   ((UI_Frame *)GetParent())->_tabs->cSc = _ScalarScroll->_scalarRBox->GetSelection();         // using zero-based scalar counting
   ((UI_Frame *)GetParent())->_tabs->cMin = _minPercentSlider->GetValue();
   ((UI_Frame *)GetParent())->_tabs->cMax = _maxPercentSlider->GetValue();
   ((UI_Frame *)GetParent())->_tabs->cId  = CHANGE_SCALAR;
   ((UI_Frame *)GetParent())->_tabs->sendDataArrayToServer();
}

void UI_DatasetPanel::_onMinMaxSlider(wxScrollEvent& WXUNUSED(event))
{
   double minScalar, maxScalar;

   GetMinMaxScalar( minScalar, maxScalar );

   _minSpinner->SetValue( ( maxScalar - minScalar ) * 
               ( (double)_minPercentSlider->GetValue() / 100) + minScalar );

   _maxSpinner->SetValue( maxScalar - ( ( maxScalar - minScalar ) - 
               ( ( maxScalar - minScalar ) * 
               ( (double)_maxPercentSlider->GetValue() / 100) ) ) );
   
   _DataSets[_RBoxScroll->_3dRBox->GetSelection()]->
            _Scalars[ _ScalarScroll->_scalarRBox->GetSelection() ]->lastMinSetting = 
            _minSpinner->GetValue();

   _DataSets[_RBoxScroll->_3dRBox->GetSelection()]->
            _Scalars[ _ScalarScroll->_scalarRBox->GetSelection() ]->lastMaxSetting = 
            _maxSpinner->GetValue();

   ConstructCommandId();
}

void UI_DatasetPanel::_onMinSpinCtrl(wxScrollEvent& WXUNUSED(event))
{
   double minScalar, maxScalar;

   GetMinMaxScalar( minScalar, maxScalar );
   int minValue = 0;

   if ( minScalar == maxScalar )
   {
      minValue = (int)minScalar;
   }
   else
   {
      minValue = (int)( ( _minSpinner->GetValue() - 
                  minScalar ) / ( maxScalar - minScalar ) * 100);
   }

   _minPercentSlider->SetValue( minValue );

   _DataSets[_RBoxScroll->_3dRBox->GetSelection()]->
            _Scalars[ _ScalarScroll->_scalarRBox->GetSelection() ]->
            lastMinSetting = _minSpinner->GetValue();

   ConstructCommandId();
}

void UI_DatasetPanel::_onMaxSpinCtrl(wxScrollEvent& WXUNUSED(event))
{
   double minScalar, maxScalar;

   GetMinMaxScalar( minScalar, maxScalar );
   int maxValue = 0;

   if ( minScalar == maxScalar )
   {
      maxValue = (int)maxScalar;
   }
   else
   {
      maxValue = (int)( ( ( maxScalar - minScalar ) -
                  ( maxScalar - _maxSpinner->GetValue() ) ) /
                  ( maxScalar - minScalar ) * 100 );
   }

   _maxPercentSlider->SetValue( maxValue );

   _DataSets[_RBoxScroll->_3dRBox->GetSelection()]->
            _Scalars[ _ScalarScroll->_scalarRBox->GetSelection() ]->
            lastMaxSetting = _maxSpinner->GetValue();

   ConstructCommandId();
}

//////////////
//Internal Utility Functions
//////////////
void UI_DatasetPanel::GetMinMaxScalar( double& min, double& max)
{
   if ( !_DataSets.empty() )
   {
      max = _DataSets[ _RBoxScroll->_3dRBox->GetSelection() ]->
                                 _Scalars[ _ScalarScroll->
                                 _scalarRBox->GetSelection() ]->range[ 1 ];

      min = _DataSets[ _RBoxScroll->_3dRBox->GetSelection() ]->
                                 _Scalars[ _ScalarScroll->
                                 _scalarRBox->GetSelection() ]->range[ 0 ];
   }
   else
   {
      max = 1;
      min = 0;
   }
}

void UI_DatasetPanel::ConstructCommandId( void )
{
   ((UI_Frame *)GetParent())->_tabs->cSc = _ScalarScroll->_scalarRBox->GetSelection();         // using zero-based scalar counting
   ((UI_Frame *)GetParent())->_tabs->cMin = _minPercentSlider->GetValue();
   ((UI_Frame *)GetParent())->_tabs->cMax = _maxPercentSlider->GetValue();
   ((UI_Frame *)GetParent())->_tabs->cId  = CHANGE_SCALAR_RANGE;
   ((UI_Frame *)GetParent())->_tabs->sendDataArrayToServer();
}
