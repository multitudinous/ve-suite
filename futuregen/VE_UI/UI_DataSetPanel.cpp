#include "UI_DataSetPanel.h"
#include "UI_Tabs.h"
#include "cfdEnum.h"
#include <iostream>
#include "UI_Frame.h"
#include "UI_ModelData.h"

UI_Scalars::UI_Scalars(wxString* scalarName)
{
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
   for (int i=0; i<_numOfScalars; i++)
      delete _Scalars[i];
   _Scalars.clear();

   for (int i=0; i<_numOfVectors; i++)
      delete _Vectors[i];
   _Vectors.clear();
}

void UI_DataSets::_buildScalars(int _numScalars, wxString* scalarNames)
{
   _numOfScalars = _numScalars;

   for (int i=0; i<_numOfScalars; i++)
   {
      thisScalar = new UI_Scalars(&scalarNames[i]);
      _Scalars.push_back(thisScalar);
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
   _col->Remove(_3dRBox);
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

  // Update VE-Xplorer with new data
  ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->cSc = 0; // using zero-based scalar counting      
  ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->cMin = 0;
  ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->cMax = 100;
  ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->cId  = CHANGE_SCALAR;
  ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->sendDataArrayToServer();

   // Need to add vector support Update VE-Xplorer with new data
  ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->cSc = 0; // using zero-based scalar counting      
  ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->cMin = 0;
  ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->cMax = 100;
  ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->cId  = CHANGE_VECTOR;
  ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->sendDataArrayToServer();
}

UI_ScalarScroll::~UI_ScalarScroll()
{
}

void UI_ScalarScroll::rebuildRBoxes(UI_DataSets* activeDataSet)
{
   _col->Remove(_scalarRBox);
   _col->Remove(_vectorRBox);

   delete _scalarRBox;
   delete _vectorRBox;

   _scalarRBox = new wxRadioBox(this,SCALAR_PANEL_RAD_BOX, wxT("Scalars"),
                                     wxDefaultPosition, wxDefaultSize,
            activeDataSet->_numOfScalars,
            ((UI_DatasetPanel*)GetParent())->_scalarNames,
            1,wxRA_SPECIFY_COLS);

   _vectorRBox = new wxRadioBox(this, VECTOR_PANEL_RAD_BOX, wxT("Vectors"),
                                wxDefaultPosition, wxDefaultSize, 
            activeDataSet->_numOfVectors,
            ((UI_DatasetPanel*)GetParent())->_vectorNames,
            1, wxRA_SPECIFY_COLS);
   
   // Get number of vectors and scalar names
   _col->Prepend(_vectorRBox,2,wxALL|wxALIGN_LEFT|wxEXPAND,5);
   _col->Prepend(_scalarRBox,2,wxALL|wxALIGN_LEFT|wxEXPAND,5);

   Refresh(); 
   //Complete Hack needed to get the page to refresh properly
   SetSize(GetSize());

   // Update VE-Xplorer with new data
   ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->cSc = 0; // using zero-based scalar counting      
   ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->cMin = 0;
   ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->cMax = 100;
   ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->cId  = CHANGE_SCALAR;
   ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->sendDataArrayToServer();

   // Need to add vector support Update VE-Xplorer with new data
   ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->cSc = 0; // using zero-based scalar counting      
   ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->cMin = 0;
   ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->cMax = 100;
   ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->cId  = CHANGE_VECTOR;
   ((UI_Frame *)((UI_DatasetPanel *)GetParent())->GetParent())->_tabs->sendDataArrayToServer();
}


BEGIN_EVENT_TABLE(UI_DatasetPanel, wxPanel)
   EVT_COMBOBOX(DATA_SET_SELECT_COMBO, UI_DatasetPanel::_onActiveSelection)
   EVT_RADIOBOX(RBOX_3D, UI_DatasetPanel::_on3d)
   EVT_RADIOBOX(VERTEX_RBOX, UI_DatasetPanel::_onVertex)
   EVT_RADIOBOX(POLYDATA_RBOX, UI_DatasetPanel::_onPolyData)
   EVT_RADIOBOX(SCALAR_PANEL_RAD_BOX, UI_DatasetPanel::_onScalars)
   EVT_RADIOBOX(VECTOR_PANEL_RAD_BOX, UI_DatasetPanel::_onVectors)
   EVT_BUTTON(SCALAR_PANEL_UPDATE_BUTTON, UI_DatasetPanel::_onUpdate)   
   EVT_COMMAND_SCROLL(MIN_PER_SLIDER_PANEL, UI_DatasetPanel::_onMinMaxSlider)
   EVT_COMMAND_SCROLL(MAX_PER_SLIDER_PANEL, UI_DatasetPanel::_onMinMaxSlider)
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

}
////////////////////////////////
//Build the Dataset page      //
////////////////////////////////
void UI_DatasetPanel::_buildPanel()
{ 
    _organizeRadioBoxInfo();
   _RBoxScroll = new UI_DatasetScroll(this);
  

   //_organizeActiveRBox();
   
   /*_activeRBox = new wxRadioBox(this, ACTIVE_RBOX, wxT("Select Active Dataset Type"), 
                                wxDefaultPosition, wxDefaultSize,
				3, datatypes,1, wxRA_SPECIFY_COLS);*/

  
   _scalarNames = new wxString[1];
   _scalarNames[0] = wxT("No Scalars");
   
   //Create the radio box w/ the list of scalar names if we have them
   _ScalarScroll = new UI_ScalarScroll(this);
  

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
                                  wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_RIGHT ); 
   _maxPercentSlider = new wxSlider(this, MAX_PER_SLIDER_PANEL,100,0,100,wxDefaultPosition, wxDefaultSize,
                                  wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS|wxSL_RIGHT ); 

   //two sizers to group the sliders and their lables
   minGroup = new wxBoxSizer(wxHORIZONTAL); 
   maxGroup = new wxBoxSizer(wxHORIZONTAL); 
  
   minGroup->Add(minLabel,0,wxALIGN_LEFT);
   minGroup->Add(_minPercentSlider,1,wxALIGN_LEFT);

   maxGroup->Add(maxLabel,0,wxALIGN_LEFT);
   maxGroup->Add(_maxPercentSlider,1,wxALIGN_LEFT);

   sRangeBoxSizer->Add(minGroup, 1, wxALIGN_LEFT|wxEXPAND); 
   sRangeBoxSizer->Add(maxGroup, 1, wxALIGN_LEFT|wxEXPAND);
  
   _colcombine1_2 = new wxBoxSizer(wxHORIZONTAL);
   _mastercol1 = new wxBoxSizer(wxVERTICAL);
   
   _col1 = new wxBoxSizer(wxVERTICAL);
   _col2 = new wxBoxSizer(wxVERTICAL);
   //_col3 = new wxBoxSizer(wxHORIZONTAL);
   //_col4 = new wxBoxSizer(wxVERTICAL);

   //new layout
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
   if ( !_DataSets.empty())
   {
      std::cout<<"DataSets successfully cleared"<<std::endl;
      for (int i=0; i<_numSteadyStateDataSets; i++)
         delete _DataSets[i];
      _DataSets.clear();
   }

   _numSteadyStateDataSets = _modelData->GetNubmerofDataSets(_activeModIndex);

   std::cout<<"numdatasets: "<<_numSteadyStateDataSets<<std::endl;

   CORBA::ULong indexScalar = 0;
   CORBA::ULong indexVector = 0;
      
   if (_numSteadyStateDataSets > 0)
   {
      VjObs::scalar_p datasetNames = 
            VjObs::scalar_p( *_modelData->GetDataSetNames(_activeModIndex) );
      
      VjObs::obj_p   datasetTypes = 
            VjObs::obj_p( *_modelData->GetDataSetTypes(_activeModIndex) );
      
      VjObs::obj_p   numScalarsPerDataset = 
            VjObs::obj_p( *_modelData->GetNumberOfScalarsPerDataSet(_activeModIndex) );
      
      VjObs::obj_p   numVectorsPerDataset = 
            VjObs::obj_p( *_modelData->GetNumberOfVectorsPerDataSet(_activeModIndex) );

      VjObs::scalar_p scalarNames = 
            VjObs::scalar_p( *_modelData->GetScalarNames(_activeModIndex) );  

      VjObs::scalar_p vectorNames = 
            VjObs::scalar_p( *_modelData->GetVectorNames(_activeModIndex) );  

      for (CORBA::ULong i = 0; i<(unsigned int)_numSteadyStateDataSets; i++)
      {
          _DataSets.push_back( new UI_DataSets() );
         
         _DataSets.at( i )->_dataSetName = datasetNames[i];
         std::cout << " DataSet Name[ " << i << " ] = " 
                     << _DataSets.at( i )->_dataSetName << std::endl;
         _DataSets.at( i )->_dataSetType = datasetTypes[i];

		   wxString* thisDataScalarNames;
		   thisDataScalarNames = new wxString[numScalarsPerDataset[i]];

		   wxString* thisDataVectorNames;
		   thisDataVectorNames = new wxString[numVectorsPerDataset[i]];

         for (int k=0; k<numScalarsPerDataset[i]; k++)
         {
            thisDataScalarNames[k] = scalarNames[indexScalar];
            indexScalar++;
         }

         // another for loop to construct per dataset vector names
         for (int k=0; k<numVectorsPerDataset[i]; k++)
         {
            thisDataVectorNames[k] = vectorNames[indexVector];
            indexVector++;
         }
        
         _DataSets.at( i )->_buildScalars(numScalarsPerDataset[i], thisDataScalarNames);
         
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
   minGroup->Remove(minLabel);
   minGroup->Remove(_minPercentSlider);
   maxGroup->Remove(maxLabel);
   maxGroup->Remove(_maxPercentSlider);
   sRangeBoxSizer->Remove(_scalarRangeBox);
   sRangeBoxSizer->Remove(minGroup); 
   sRangeBoxSizer->Remove(maxGroup);
   _col1->Remove(_RBoxScroll);
   _col2->Remove(_ScalarScroll);
   _colcombine1_2->Remove(_col1);
   _colcombine1_2->Remove(_col2);
   _mastercol1->Remove(_datasetCombo);
   _mastercol1->Remove(_colcombine1_2);
   //_col3->Remove(sRangeBoxSizer);
   //_col4->Remove(_visUpdateButton);
   dHeadingBoxSizer->Remove(_mastercol1);
   dHeadingBoxSizer->Remove(sRangeBoxSizer);
   //dHeadingBoxSizer->Remove(_col4);
   datasetPanelGroup->Remove(dHeadingBoxSizer);
   delete _RBoxScroll;
   delete [] _scalarNames;
   delete [] _vectorNames;
   delete _ScalarScroll;
   delete _datasetCombo;
   //delete _visUpdateButton;
   delete _scalarRangeBox;
   delete minLabel;
   delete maxLabel;
   delete _minPercentSlider; 
   delete _maxPercentSlider; 
   delete _dataHeadingBox;

/////////////////////////////////////////////////////////////////////////////////////////////////////////////
   _activeModIndex = _activeMod;
   _buildDataSets();
   _buildPanel(); 

   if ( !_DataSets.empty() )
	  _setScalars(_DataSets[0]);

}

void UI_DatasetPanel::_setScalars(UI_DataSets* activeDataSet)
{
   //activeDataSet = thisactiveDataSet;

   //_noScalars = activeDataSet->_numofScalars;
   //for (int p=0; p<_ScalarScroll->_scalarRBox->GetCount(); p++)
   delete [] _scalarNames;
   delete [] _vectorNames;

   _scalarNames = new wxString[activeDataSet->_numOfScalars];
   _vectorNames = new wxString[activeDataSet->_numOfVectors];
   
   for (int i=0; i<activeDataSet->_numOfScalars; i++)
   {
      _scalarNames[i] = activeDataSet->_Scalars[i]->_thisScalarName;
   }

   for (int i=0; i<activeDataSet->_numOfVectors; i++)
   {
      _vectorNames[i] = activeDataSet->_Vectors[i]->_thisScalarName;
   }

   _ScalarScroll->rebuildRBoxes(activeDataSet);

   Refresh();
   SetSize(GetSize());

   /*_col2->Remove(_ScalarScroll);

   delete _ScalarScroll;
   _ScalarScroll = new UI_ScalarScroll(this);
   _scalarRBox = new wxRadioBox(this,SCALAR_RAD_BOX, wxT("Scalars"),
                                     wxDefaultPosition, wxDefaultSize,activeDataSet->_numofScalars,
                                     _scalarNames,1,wxRA_SPECIFY_COLS);
   _col2->Prepend(_ScalarScroll,6,wxALIGN_LEFT|wxEXPAND);
   
   //Complete Hack needed to get the page to refresh properly
   SetSize(GetSize());*/
}

void UI_DatasetPanel::_setScalarsnoDatasets()
{
   wxString empty[1];
   empty[0] = wxT("No scalars");
   _col2->Remove(_ScalarScroll);

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

void UI_DatasetPanel::_onActiveSelection(wxCommandEvent& event)
{
   _RBoxScroll->changeActiveDatasetType(_datasetCombo->GetSelection());

   for (int i=0; i<_numSteadyStateDataSets; i++)
   {
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

void UI_DatasetPanel::_on3d(wxCommandEvent& event)
{
   for (int i=0; i<_numSteadyStateDataSets; i++)
      if (_DataSets[i]->_dataSetType == 0)
         if (_RBoxScroll->_3dRBox->GetString(_RBoxScroll->_3dRBox->GetSelection()) == _DataSets[i]->_dataSetName)
         {
            _setScalars(_DataSets[i]);
            ((UI_Frame*)GetParent())->_tabs->setActiveDataset(i);
         }           
}

void UI_DatasetPanel::_onVertex(wxCommandEvent& event)
{
   for (int i=0; i<_numSteadyStateDataSets; i++)
      if (_DataSets[i]->_dataSetType == 1)
         if (_RBoxScroll->_3dRBox->GetString(_RBoxScroll->_3dRBox->GetSelection()) == _DataSets[i]->_dataSetName)
         {
            _setScalars(_DataSets[i]);
            ((UI_Frame*)GetParent())->_tabs->setActiveDataset(i);
         }
   
}

void UI_DatasetPanel::_onPolyData(wxCommandEvent& event)
{
   for (int i=0; i<_numSteadyStateDataSets; i++)
      if (_DataSets[i]->_dataSetType == 2)
         if (_RBoxScroll->_3dRBox->GetString(_RBoxScroll->_3dRBox->GetSelection()) == _DataSets[i]->_dataSetName)
         {
            _setScalars(_DataSets[i]);
            ((UI_Frame*)GetParent())->_tabs->setActiveDataset(i);
         }
   
}

void UI_DatasetPanel::_onVectors( wxCommandEvent& event )
{
   ((UI_Frame *)GetParent())->_tabs->cSc = _ScalarScroll->_vectorRBox->GetSelection();         // using zero-based scalar counting
   ((UI_Frame *)GetParent())->_tabs->cMin = _minPercentSlider->GetValue();
   ((UI_Frame *)GetParent())->_tabs->cMax = _maxPercentSlider->GetValue();
   ((UI_Frame *)GetParent())->_tabs->cId  = CHANGE_VECTOR;
   ((UI_Frame *)GetParent())->_tabs->sendDataArrayToServer();
}

void UI_DatasetPanel::_onScalars(wxCommandEvent& event)
{
   ((UI_Frame *)GetParent())->_tabs->cSc = _ScalarScroll->_scalarRBox->GetSelection();         // using zero-based scalar counting
   ((UI_Frame *)GetParent())->_tabs->cMin = _minPercentSlider->GetValue();
   ((UI_Frame *)GetParent())->_tabs->cMax = _maxPercentSlider->GetValue();
   ((UI_Frame *)GetParent())->_tabs->cId  = CHANGE_SCALAR;
   ((UI_Frame *)GetParent())->_tabs->sendDataArrayToServer();
}

void UI_DatasetPanel::_onUpdate(wxCommandEvent& event)
{
   ((UI_Frame *)GetParent())->_tabs->cSc = _ScalarScroll->_scalarRBox->GetSelection();         // using zero-based scalar counting
   ((UI_Frame *)GetParent())->_tabs->cMin = _minPercentSlider->GetValue();
   ((UI_Frame *)GetParent())->_tabs->cMax = _maxPercentSlider->GetValue();
   ((UI_Frame *)GetParent())->_tabs->cId  = CHANGE_SCALAR;
   ((UI_Frame *)GetParent())->_tabs->sendDataArrayToServer();
}

void UI_DatasetPanel::_onMinMaxSlider(wxScrollEvent& event)
{
   ((UI_Frame *)GetParent())->_tabs->cSc = _ScalarScroll->_scalarRBox->GetSelection();         // using zero-based scalar counting
   ((UI_Frame *)GetParent())->_tabs->cMin = _minPercentSlider->GetValue();
   ((UI_Frame *)GetParent())->_tabs->cMax = _maxPercentSlider->GetValue();
   ((UI_Frame *)GetParent())->_tabs->cId  = CHANGE_SCALAR_RANGE;
   ((UI_Frame *)GetParent())->_tabs->sendDataArrayToServer();
}
