#include <iostream>
#include "UI_Tabs.h"
#include "cfdEnum.h"
#include "UI_Frame.h"
#include "UI_DataSetPanel.h"



UI_Scalars::UI_Scalars(wxString scalarName)
{
   _thisScalarName = scalarName;
}

UI_Scalars::~UI_Scalars()
{

}





UI_DataSets::UI_DataSets()
{

}

UI_DataSets::~UI_DataSets()
{

}

void UI_DataSets::_buildScalars(int _numScalars, wxString* scalarNames)
{
   _numofScalars = _numScalars;

   for (int i=0; i<_numScalars; i++)
   {
      thisScalar = new UI_Scalars(scalarNames[i]);
      _Scalars.push_back(thisScalar);
   }
}


BEGIN_EVENT_TABLE(UI_DatasetScrollable, wxScrolledWindow)
END_EVENT_TABLE()
//////////////////////////////////////////////////
//Constructor                                   //
//////////////////////////////////////////////////
UI_DatasetScrollable::UI_DatasetScrollable(wxWindow* parent)
:wxScrolledWindow(parent)
{
   std::cout<<"testing1"<<std::endl;
   _dataSetPanel = new UI_DatasetPanel(this);
   
   _3dScrollable = new UI_Dataset3dScrollable(this);
   _vertScrollable = new UI_DatasetVertexScrollable(this);
   _polyScrollable = new UI_DatasetPolyScrollable(this);
   
   buildPage();
   //_dataSetPanel->_setScalars(_dataSetPanel->_DataSets[0]);
   
}

UI_DatasetScrollable::~UI_DatasetScrollable()
{
}

void UI_DatasetScrollable::buildPage()
{
   _col1 = new wxBoxSizer(wxVERTICAL);
   _col2 = new wxBoxSizer(wxVERTICAL);
   _col3 = new wxBoxSizer(wxVERTICAL);

   //new layout
   _col1->Add(_3dScrollable,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   _col1->Add(_vertScrollable,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   _col1->Add(_polyScrollable,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL); 
   std::cout<<"testing3"<<std::endl;
   _col2->Add(_dataSetPanel->_scalarRBox,6,wxALIGN_LEFT|wxEXPAND);
   _col2->Add(_dataSetPanel->_vectorRBox,2,wxALIGN_LEFT|wxEXPAND);
   _col2->Add(_dataSetPanel->_visUpdateButton,0,wxALIGN_CENTER_HORIZONTAL);

   _col3->Add(_dataSetPanel->sRangeBoxSizer,1,wxALIGN_LEFT|wxEXPAND);

   _dataHeadingBox = new wxStaticBox(this, -1, wxT("---------------------Data and Scalar Set Selection-------------------"));
   wxStaticBoxSizer* dHeadingBoxSizer = new wxStaticBoxSizer(_dataHeadingBox,wxHORIZONTAL);

   dHeadingBoxSizer->Add(_col1,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   dHeadingBoxSizer->Add(_col2,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   dHeadingBoxSizer->Add(_col3,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   _top = new wxBoxSizer(wxHORIZONTAL);
   _bottom = new wxBoxSizer(wxHORIZONTAL);

   _top->Add(_dataSetPanel->_activeRBox,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   _bottom->Add(dHeadingBoxSizer,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   wxBoxSizer* datasetPanelGroup = new wxBoxSizer(wxVERTICAL);
   datasetPanelGroup->Add(_top,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   datasetPanelGroup->Add(_bottom,10,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   //set this flag and let wx handle alignment
   SetAutoLayout(true);

   //assign the group to the panel
   SetSizer(datasetPanelGroup);
}

BEGIN_EVENT_TABLE(UI_Dataset3dScrollable, wxScrolledWindow)
END_EVENT_TABLE()
//////////////////////////////////////////////////
//Constructor                                   //
//////////////////////////////////////////////////
UI_Dataset3dScrollable::UI_Dataset3dScrollable(wxScrolledWindow* parent)
:wxScrolledWindow(parent)
{
   //_3dRBox;
   _3dRBox = new wxRadioBox(this, RBOX_3D, wxT("3D mesh"), //0
                                wxDefaultPosition, wxDefaultSize,
				((UI_DatasetScrollable*)GetParent())->_dataSetPanel->_no3DMesh, 
            ((UI_DatasetScrollable*)GetParent())->_dataSetPanel->meshArrayNames,
            1, wxRA_SPECIFY_COLS);
}

UI_Dataset3dScrollable::~UI_Dataset3dScrollable()
{
}


BEGIN_EVENT_TABLE(UI_DatasetVertexScrollable, wxScrolledWindow)
END_EVENT_TABLE()
//////////////////////////////////////////////////
//Constructor                                   //
//////////////////////////////////////////////////
UI_DatasetVertexScrollable::UI_DatasetVertexScrollable(wxScrolledWindow* parent)
:wxScrolledWindow(parent)
{
   //_vertexRBox;
   _vertexRBox = new wxRadioBox(this, VERTEX_RBOX, wxT("Vertex Data"), //1
                                wxDefaultPosition, wxDefaultSize,
				((UI_DatasetScrollable*)GetParent())->_dataSetPanel->_noVertex, 
            ((UI_DatasetScrollable*)GetParent())->_dataSetPanel->vertexArrayNames,
            1, wxRA_SPECIFY_COLS);
}

UI_DatasetVertexScrollable::~UI_DatasetVertexScrollable()
{
}



BEGIN_EVENT_TABLE(UI_DatasetPolyScrollable, wxScrolledWindow)
END_EVENT_TABLE()
//////////////////////////////////////////////////
//Constructor                                   //
//////////////////////////////////////////////////
UI_DatasetPolyScrollable::UI_DatasetPolyScrollable(wxScrolledWindow* parent)
:wxScrolledWindow(parent)
{
   //_polydataRBox;
   _polydataRBox = new wxRadioBox(this, POLYDATA_RBOX, wxT("Polydata"), //2
                                wxDefaultPosition, wxDefaultSize,
				((UI_DatasetScrollable*)GetParent())->_dataSetPanel->_noPolydata,
            ((UI_DatasetScrollable*)GetParent())->_dataSetPanel->polydataArrayNames,
            1, wxRA_SPECIFY_COLS);
}

UI_DatasetPolyScrollable::~UI_DatasetPolyScrollable()
{
}




BEGIN_EVENT_TABLE(UI_DatasetPanel, wxPanel)
   EVT_RADIOBOX(ACTIVE_RBOX, UI_DatasetPanel::_onActive)
   EVT_RADIOBOX(RBOX_3D, UI_DatasetPanel::_on3d)
   EVT_RADIOBOX(VERTEX_RBOX, UI_DatasetPanel::_onVertex)
   EVT_RADIOBOX(POLYDATA_RBOX, UI_DatasetPanel::_onPolyData)
   EVT_RADIOBOX(SCALAR_RAD_BOX, UI_DatasetPanel::_onScalars)
   EVT_BUTTON(SCALAR_UPDATE_BUTTON, UI_DatasetPanel::_onUpdate)   
   EVT_COMMAND_SCROLL(MIN_PER_SLIDER, UI_DatasetPanel::_onMinMaxSlider)
   EVT_COMMAND_SCROLL(MAX_PER_SLIDER, UI_DatasetPanel::_onMinMaxSlider)
END_EVENT_TABLE()
//////////////////////////////////////////////////
//Constructor                                   //
//////////////////////////////////////////////////
UI_DatasetPanel::UI_DatasetPanel(wxWindow* tControl)
:wxPanel(tControl)
{
   _activeRBox;
   //_3dRBox;
   //_vertexRBox;
   //_polydataRBox;
   _scalarRBox;
   _vectorRBox;
   //_3dScroll = tControl;

   
   _buildDataSets();
   
   _buildPanel();
   
   
   std::cout<<"testing2"<<std::endl;
   
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


   /*_3dRBox = new wxRadioBox(this, RBOX_3D, wxT("3D mesh"), //0
                                wxDefaultPosition, wxDefaultSize,
				_no3DMesh, meshArrayNames,1, wxRA_SPECIFY_COLS);
   _vertexRBox = new wxRadioBox(this, VERTEX_RBOX, wxT("Vertex Data"), //1
                                wxDefaultPosition, wxDefaultSize,
				_noVertex, vertexArrayNames,1, wxRA_SPECIFY_COLS);
   _polydataRBox = new wxRadioBox(this, POLYDATA_RBOX, wxT("Polydata"), //2
                                wxDefaultPosition, wxDefaultSize,
				_noPolydata, polydataArrayNames,1, wxRA_SPECIFY_COLS);*/

   
   _organizeActiveRBox();
   
   _activeRBox = new wxRadioBox(this, ACTIVE_RBOX, wxT("Select Active Dataset Type"), 
                                wxDefaultPosition, wxDefaultSize,
				3, datatypes,3, wxRA_SPECIFY_COLS);

  
   _scalarNames = new wxString[1];
   _scalarNames[0] = wxT("No Scalars");
   
   //Create the radio box w/ the list of scalar names if we have them
   _scalarRBox = new wxRadioBox(this,SCALAR_RAD_BOX, wxT("Scalars"),
                                     wxDefaultPosition, wxDefaultSize,
            1,_scalarNames,1,wxRA_SPECIFY_COLS);

   _vectorRBox = new wxRadioBox(this, VECTOR_RAD_BOX, wxT("Vectors"),
                                wxDefaultPosition, wxDefaultSize, 
            1, _scalarNames, 1, wxRA_SPECIFY_COLS);


   //The "Update Visualization" button
   _visUpdateButton = new wxButton(this, SCALAR_UPDATE_BUTTON, wxT("Update"));

 
   //The static box for the scalar range sliders
   _scalarRangeBox = new wxStaticBox(this, -1, wxT("Scalar Range"));

   //need a sizer for this box
   //The items will be placed  next (horizontally) to other rather than on top of each other
   //(vertically)
   sRangeBoxSizer = new wxStaticBoxSizer(_scalarRangeBox,wxHORIZONTAL);
   

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

   //two sizers to group the sliders and their lables
   wxBoxSizer* minGroup = new wxBoxSizer(wxVERTICAL); 
   wxBoxSizer* maxGroup = new wxBoxSizer(wxVERTICAL); 

   minGroup->Add(minLabel,0,wxALIGN_LEFT);
   minGroup->Add(_minPercentSlider,1,wxALIGN_LEFT);

   maxGroup->Add(maxLabel,0,wxALIGN_RIGHT);
   maxGroup->Add(_maxPercentSlider,1,wxALIGN_RIGHT);

   sRangeBoxSizer->Add(minGroup, 1, wxALIGN_LEFT|wxEXPAND); 
   sRangeBoxSizer->Add(maxGroup, 1, wxALIGN_RIGHT|wxEXPAND);
   
   /*_col1 = new wxBoxSizer(wxVERTICAL);
   _col2 = new wxBoxSizer(wxVERTICAL);
   _col3 = new wxBoxSizer(wxVERTICAL);

   //new layout
   _col1->Add(_3dRBox,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   _col1->Add(_polydataRBox,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   _col1->Add(_vertexRBox,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL); 

   _col2->Add(_scalarRBox,6,wxALIGN_LEFT|wxEXPAND);
   _col2->Add( _vectorRBox,2,wxALIGN_LEFT|wxEXPAND);
   _col2->Add(_visUpdateButton,0,wxALIGN_CENTER_HORIZONTAL);

   _col3->Add(sRangeBoxSizer,1,wxALIGN_LEFT|wxEXPAND);

   _dataHeadingBox = new wxStaticBox(this, -1, wxT("---------------------Data and Scalar Set Selection-------------------"));
   wxStaticBoxSizer* dHeadingBoxSizer = new wxStaticBoxSizer(_dataHeadingBox,wxHORIZONTAL);

   dHeadingBoxSizer->Add(_col1,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   dHeadingBoxSizer->Add(_col2,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   dHeadingBoxSizer->Add(_col3,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   _top = new wxBoxSizer(wxHORIZONTAL);
   _bottom = new wxBoxSizer(wxHORIZONTAL);

   _top->Add(_activeRBox,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   _bottom->Add(dHeadingBoxSizer,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   wxBoxSizer* datasetPanelGroup = new wxBoxSizer(wxVERTICAL);
   datasetPanelGroup->Add(_top,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   datasetPanelGroup->Add(_bottom,10,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   //set this flag and let wx handle alignment
   SetAutoLayout(true);

   //assign the group to the panel
   SetSizer(datasetPanelGroup);*/
}

void UI_DatasetPanel::_buildDataSets( void )
{
   UI_Tabs* tempTabs = ((UI_Frame*)((UI_DatasetScrollable*)GetParent())->GetParent())->_tabs;
   _numSteadyStateDataSets = tempTabs->datasetNum;
  
   CORBA::ULong index = 0;
      
   if (_numSteadyStateDataSets > 0)
   {
      for (CORBA::ULong i = 0; i<_numSteadyStateDataSets; i++)
      {
         thisDataSet = new UI_DataSets();
         thisDataSet->_dataSetName = tempTabs->datasetNames[i];
         
         thisDataSet->_dataSetType = tempTabs->datasetTypes[i];
         
         wxString scalarNames[tempTabs->numScalarsPerDataset[i]];
         
         for (int k=0; k<tempTabs->numScalarsPerDataset[i]; k++)
         {
            scalarNames[k] = tempTabs->sc_attrib[index];
            index++;
         }
         
         thisDataSet->_buildScalars(tempTabs->numScalarsPerDataset[i], scalarNames);
         
         _DataSets.push_back(thisDataSet);

         //clean up the names array
         for(int p = tempTabs->numScalarsPerDataset[i] -1; p >= 0; p--)
         {
            delete [] scalarNames[p];   
         }                  
      }
   }  
}

void UI_DatasetPanel::_setScalars(UI_DataSets* activeDataSet)
{

   //_noScalars = activeDataSet->_numofScalars;
   for (int p=0; p<_scalarRBox->GetCount(); p++)
      delete [] _scalarNames[p];
   
   _scalarNames = new wxString[activeDataSet->_numofScalars];
   
   for (int i=0; i<activeDataSet->_numofScalars; i++)
   {
      _scalarNames[i] = activeDataSet->_Scalars[i]->_thisScalarName;
   }

   ((UI_DatasetScrollable*)GetParent())->_col2->Remove(_scalarRBox);

   delete _scalarRBox;
   _scalarRBox = new wxRadioBox(this,SCALAR_RAD_BOX, wxT("Scalars"),
                                     wxDefaultPosition, wxDefaultSize,activeDataSet->_numofScalars,
                                     _scalarNames,1,wxRA_SPECIFY_COLS);
   ((UI_DatasetScrollable*)GetParent())->_col2->Prepend(_scalarRBox,6,wxALIGN_LEFT|wxEXPAND);
   
   //Complete Hack needed to get the page to refresh properly
   SetSize(GetSize());
}

void UI_DatasetPanel::_setScalarsnoDatasets()
{
   wxString empty[1];
   empty[0] = wxT("No scalars");
   _col2->Remove(_scalarRBox);
   delete _scalarRBox;
   _scalarRBox = new wxRadioBox(this,SCALAR_RAD_BOX, wxT("Scalars"),
                                     wxDefaultPosition, wxDefaultSize,1,
                                     empty,1,wxRA_SPECIFY_COLS);
   _col2->Prepend(_scalarRBox,6,wxALIGN_LEFT|wxEXPAND);

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

void UI_DatasetPanel::_organizeActiveRBox()
{
   if (_numSteadyStateDataSets >0)
   {  
      if(_DataSets[0]->_dataSetType == 0)
      {
         //((UI_DatasetScrollable*)GetParent())->_3dScrollable->_3dRBox->Enable(TRUE);
         //((UI_DatasetScrollable*)GetParent())->_vertScrollable->_vertexRBox->Enable(FALSE);
         //((UI_DatasetScrollable*)GetParent())->_polyScrollable->_polydataRBox->Enable(FALSE);
         
         datatypes[0] = wxT("3D mesh");
         datatypes[1] = wxT("Vertex Data");
         datatypes[2] = wxT("Polydata");
      }
      else if(_DataSets[0]->_dataSetType == 1)
      {
         //((UI_DatasetScrollable*)GetParent())->_3dScrollable->_3dRBox->Enable(FALSE);
         //((UI_DatasetScrollable*)GetParent())->_vertScrollable->_vertexRBox->Enable(TRUE);
         //((UI_DatasetScrollable*)GetParent())->_polyScrollable->_polydataRBox->Enable(FALSE);
         datatypes[1] = wxT("3D mesh");
         datatypes[0] = wxT("Vertex Data");
         datatypes[2] = wxT("Polydata");
      }
      else if(_DataSets[0]->_dataSetType == 2)
      {
         //((UI_DatasetScrollable*)GetParent())->_3dScrollable->_3dRBox->Enable(FALSE);
         //((UI_DatasetScrollable*)GetParent())->_vertScrollable->_vertexRBox->Enable(FALSE);
         //((UI_DatasetScrollable*)GetParent())->_polyScrollable->_polydataRBox->Enable(TRUE);
         datatypes[1] = wxT("3D mesh");
         datatypes[2] = wxT("Vertex Data");
         datatypes[0] = wxT("Polydata");
      }     
   }
   else
   {
      //((UI_DatasetScrollable*)GetParent())->_3dScrollable->_3dRBox->Enable(FALSE);
      //((UI_DatasetScrollable*)GetParent())->_vertScrollable->_vertexRBox->Enable(FALSE);
      //((UI_DatasetScrollable*)GetParent())->_polyScrollable->_polydataRBox->Enable(FALSE);
      datatypes[0] = wxT("No Datasets Available");

   }

}

void UI_DatasetPanel::_onActive(wxCommandEvent& event)
{
   /*if ( _activeRBox->GetStringSelection() == "3D mesh")
   {  
      ((UI_DatasetScrollable*)GetParent())->_3dScrollable->_3dRBox->Enable(TRUE);
      _vertexRBox->Enable(FALSE);
      _polydataRBox->Enable(FALSE);
      for (int i=0; i<_numSteadyStateDataSets; i++)
         if(_DataSets[i]->_dataSetType == 0 && _3dRBox->GetString(_3dRBox->GetSelection()) == _DataSets[i]->_dataSetName)
         {
            _setScalars(_DataSets[i]);
            i = _numSteadyStateDataSets;
         }
         else
            _setScalarsnoDatasets();
   }
   if ( _activeRBox->GetStringSelection() == "Vertex Data")
   {  
      ((UI_DatasetScrollable*)GetParent())->_3dScrollable->_3dRBox->Enable(FALSE);
      _vertexRBox->Enable(TRUE);
      _polydataRBox->Enable(FALSE);
      for (int i=0; i<_numSteadyStateDataSets; i++)
         if(_DataSets[i]->_dataSetType == 1 && _vertexRBox->GetString(_vertexRBox->GetSelection()) == _DataSets[i]->_dataSetName)
         {
            _setScalars(_DataSets[i]);
            i = _numSteadyStateDataSets;
         }
         else
            _setScalarsnoDatasets();
   }
   if ( _activeRBox->GetStringSelection() == "Polydata")
   {  
      ((UI_DatasetScrollable*)GetParent())->_3dScrollable->_3dRBox->Enable(FALSE);
      _vertexRBox->Enable(FALSE);
      _polydataRBox->Enable(TRUE);
      for (int i=0; i<_numSteadyStateDataSets; i++)
         if(_DataSets[i]->_dataSetType == 2 && _polydataRBox->GetString(_polydataRBox->GetSelection()) == _DataSets[i]->_dataSetName)
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
   /*for (int i=0; i<_numSteadyStateDataSets; i++)
      if (_DataSets[i]->_dataSetType == 0)
         if (_3dRBox->GetString(_3dRBox->GetSelection()) == _DataSets[i]->_dataSetName)
         {
            _setScalars(_DataSets[i]);
            ((UI_Frame*)GetParent())->_tabs->setActiveDataset(i);
         }
         
      */     
}

void UI_DatasetPanel::_onVertex(wxCommandEvent& event)
{
   /*for (int i=0; i<_numSteadyStateDataSets; i++)
      if (_DataSets[i]->_dataSetType == 1)
         if (_vertexRBox->GetString(_vertexRBox->GetSelection()) == _DataSets[i]->_dataSetName)
         {
            _setScalars(_DataSets[i]);
            ((UI_Frame*)GetParent())->_tabs->setActiveDataset(i);
         }
   */
}

void UI_DatasetPanel::_onPolyData(wxCommandEvent& event)
{
   /*for (int i=0; i<_numSteadyStateDataSets; i++)
      if (_DataSets[i]->_dataSetType == 2)
         if (_polydataRBox->GetString(_polydataRBox->GetSelection()) == _DataSets[i]->_dataSetName)
         {
            _setScalars(_DataSets[i]);
            ((UI_Frame*)GetParent())->_tabs->setActiveDataset(i);
         }
   */
}

void UI_DatasetPanel::_onScalars(wxCommandEvent& event)
{
   ((UI_Frame *)GetParent())->_tabs->cSc = _scalarRBox->GetSelection();         // using zero-based scalar counting
   ((UI_Frame *)GetParent())->_tabs->cMin = _minPercentSlider->GetValue();
   ((UI_Frame *)GetParent())->_tabs->cMax = _maxPercentSlider->GetValue();
   ((UI_Frame *)GetParent())->_tabs->cId  = CHANGE_SCALAR;
   ((UI_Frame *)GetParent())->_tabs->sendDataArrayToServer();
}

void UI_DatasetPanel::_onUpdate(wxCommandEvent& event)
{
   ((UI_Frame *)GetParent())->_tabs->cSc = _scalarRBox->GetSelection();         // using zero-based scalar counting
   ((UI_Frame *)GetParent())->_tabs->cMin = _minPercentSlider->GetValue();
   ((UI_Frame *)GetParent())->_tabs->cMax = _maxPercentSlider->GetValue();
   ((UI_Frame *)GetParent())->_tabs->cId  = CHANGE_SCALAR;
   ((UI_Frame *)GetParent())->_tabs->sendDataArrayToServer();
}

void UI_DatasetPanel::_onMinMaxSlider(wxCommandEvent& event)
{
   ((UI_Frame *)GetParent())->_tabs->cSc = _scalarRBox->GetSelection();         // using zero-based scalar counting
   ((UI_Frame *)GetParent())->_tabs->cMin = _minPercentSlider->GetValue();
   ((UI_Frame *)GetParent())->_tabs->cMax = _maxPercentSlider->GetValue();
   ((UI_Frame *)GetParent())->_tabs->cId  = CHANGE_SCALAR_RANGE;
   ((UI_Frame *)GetParent())->_tabs->sendDataArrayToServer();
}



















