#include <iostream>
#include "UI_Tabs.h"
#include "UI_Frame.h"
#include "UI_DataSetTab.h"

BEGIN_EVENT_TABLE(UI_DatasetTab, wxScrolledWindow)
   EVT_RADIOBOX(RBOX_TABS_3D, UI_DatasetTab::_on3d)
   EVT_RADIOBOX(VERTEX_TABS_RBOX, UI_DatasetTab::_onVertex)
   EVT_RADIOBOX(POLYDATA_TABS_RBOX, UI_DatasetTab::_onPolyData)
   EVT_RADIOBOX(SCALAR_P_RBOX, UI_DatasetTab::_onScalar1)
   EVT_RADIOBOX(SCALAR_V_RBOX, UI_DatasetTab::_onScalar2)
   EVT_RADIOBOX(SCALAR_3D_RBOX, UI_DatasetTab::_onScalar3)
END_EVENT_TABLE()
////////////////////////////////
//Dataset constructors        //
////////////////////////////////
UI_DatasetInfo::UI_DatasetInfo()
{
   _type = -1;
   _nScalars = -1;
   _name = 0;
   _scalarNames = 0;
}
////////////////////////////////////////////////////
UI_DatasetInfo::UI_DatasetInfo(const char* name, int type)
{
   _nScalars = -1;
   _name = 0;
   _scalarNames = 0;

   //set the type
   _type = type;
   
   //set the name
   setDatasetName(name);
}
/////////////////////////////////
//Destructor                   //
/////////////////////////////////
UI_DatasetInfo::~UI_DatasetInfo()
{
   if(_name){
      delete [] _name;
      _name = 0;
   }

   //need to clean this up first
   if(_scalarNames){
      for(int i = _nScalars -1; i >=0; i--){
         delete [] _scalarNames[i];
      }
      delete [] _scalarNames;
      _scalarNames = 0;
   }
}
////////////////////////////////////////////////////////////////////
//equal operator                                                  //
////////////////////////////////////////////////////////////////////
UI_DatasetInfo& UI_DatasetInfo::operator=(const UI_DatasetInfo& rhs)
{
   if(_name){
      delete [] _name;
      _name = 0;
   }

   //need to clean this up first
   if(_scalarNames){
      for(int i = _nScalars -1; i >=0; i--){
         delete [] _scalarNames[i];
      }
         delete [] _scalarNames;
      _scalarNames = 0;
   }

   _nScalars = rhs._nScalars;
   _type = rhs._type;
   setDatasetName(rhs._name);
   setScalarData(rhs._scalarNames,rhs._nScalars);
   
   return  *this;
}
////////////////////////////////////////////////////////////////////
UI_DatasetInfo& UI_DatasetInfo::operator=(const UI_DatasetInfo* rhs)
{
  if(_name){
      delete [] _name;
      _name = 0;
   }

   //need to clean this up first
   if(_scalarNames){
      for(int i = _nScalars -1; i >=0; i--){
         delete [] _scalarNames[i];
      }
      delete [] _scalarNames;
      _scalarNames = 0;
   }

   _nScalars = rhs->_nScalars;
   _type = rhs->_type;
   setDatasetName(rhs->_name);
   setScalarData(rhs->_scalarNames,rhs->_nScalars);
   
   return  *this;
}
/////////////////////////////////////////////////////
//set the name of this dataset                     //
/////////////////////////////////////////////////////
void UI_DatasetInfo::setDatasetName(const char* name)
{
   if(_name){
      delete [] _name;
      _name = 0;
   }
   //set the name
   if(name){
      _name = new char[strlen(name)+1];
      strcpy(_name,name);
   }else{
   }
   
}
////////////////////////////////////////////////////////////////////
void UI_DatasetInfo::setScalarData(char** names, int nScalars)
{
   //need to clean this up first
   if(_scalarNames){
      for(int i = _nScalars -1; i >=0; i--){
         delete [] _scalarNames[i];
      }
      delete [] _scalarNames;
      _scalarNames = 0;
   }

   //reset number of scalars
   _nScalars = nScalars;

   //re allocate
   _scalarNames = new char*[_nScalars];
   for(int i = 0; i < _nScalars; i++){
      _scalarNames[i] = new char[strlen(names[i])+1];   
      strcpy(_scalarNames[i],names[i]);
   }
}
/////////////////////////////////////////////////
//get the name for a specified scalar          //
/////////////////////////////////////////////////
const char* UI_DatasetInfo::scalarName(int whichScalar)
{
   if(_scalarNames[whichScalar]){
      return _scalarNames[whichScalar];
   }else{
      cout<<"Invalid index for scalar: "<<whichScalar<<endl;
      return 0;
   }
}

//////////////////////////////////////////////////
//Constructor                                   //
//////////////////////////////////////////////////
UI_DatasetTab::UI_DatasetTab(wxWindow* tControl)
:wxScrolledWindow(tControl)
{
   _3dRBox;
   _vertexRBox;
   _polydataRBox;
   _scalarVRBox;
   _scalarPRBox;
   _scalarMRBox;

   _col1 = 0;
   _col2 = 0;
   _col3 = 0;
   _numAddedDSets = 0;
   _nDatasets = 0;
   _dSetInfo = 0;
   _currentDataSet = 0;
   _foundTypes = 0;
   _buildPage();
}
///////////////////////////////
//Destructor                 //
///////////////////////////////
UI_DatasetTab::~UI_DatasetTab()
{
   if(_foundTypes){
      delete [] _foundTypes;
      _foundTypes = 0;
   }
   if(_dSetInfo){
      delete [] _dSetInfo;
   }
}
////////////////////////////////
//Build the Dataset page      //
////////////////////////////////
void UI_DatasetTab::_buildPage()
{


   //create 6(?) radio boxes
   wxString defaultNames[] = {wxT("default")};
   _3dRBox = new wxRadioBox(this, RBOX_3D, wxT("3D mesh"),
                                wxDefaultPosition, wxDefaultSize,
				1, defaultNames,1, wxRA_SPECIFY_COLS);
   _vertexRBox = new wxRadioBox(this, VERTEX_RBOX, wxT("Vertex Data"),
                                wxDefaultPosition, wxDefaultSize,
				1, defaultNames,1, wxRA_SPECIFY_COLS);
   _polydataRBox = new wxRadioBox(this, POLYDATA_RBOX, wxT("Polydata"),
                                wxDefaultPosition, wxDefaultSize,
				1, defaultNames,1, wxRA_SPECIFY_COLS);
/*   _scalarVRBox = new wxRadioBox(this, SCALAR_V_RBOX, wxT("Scalars"),
                                wxDefaultPosition, wxDefaultSize,
				1, defaultNames,1, wxRA_SPECIFY_COLS);
   _scalarPRBox = new wxRadioBox(this, SCALAR_P_RBOX, wxT("Scalars"),
                                wxDefaultPosition, wxDefaultSize,
				1, defaultNames,1, wxRA_SPECIFY_COLS);
   _scalarMRBox = new wxRadioBox(this, SCALAR_3D_RBOX, wxT("Scalars"),
                                wxDefaultPosition, wxDefaultSize,
				1, defaultNames,1, wxRA_SPECIFY_COLS);*/


   //size these in 3 cols 
//   if(!_col1)_col1 = new wxBoxSizer(wxVERTICAL);
//   if(!_col2)_col2 = new wxBoxSizer(wxVERTICAL);
   //if(!_col3)_col3 = new wxBoxSizer(wxVERTICAL);  

   _col1 = new wxBoxSizer(wxVERTICAL);
   //new layout
   _col1->Add(_3dRBox,1,wxALIGN_TOP);
   _col1->Add(_polydataRBox,1,wxALIGN_CENTER_HORIZONTAL);
   _col1->Add(_vertexRBox,1,wxALIGN_BOTTOM); 

//   _col2->Add(_scalarVRBox,2,wxALIGN_TOP);
//   _col2->Add(_vectorRBox,1,wxALIGN_BOTTOM);

   wxBoxSizer* datasetPanelGroup = new wxBoxSizer(wxHORIZONTAL);
   datasetPanelGroup->Add(_col1,0,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
//   datasetPanelGroup->Add(_col2,0,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);


/*   //add the 2 boxes to the first col
   _col1->Add(_3dRBox,1,wxALIGN_TOP);
   _col1->Add(_scalarMRBox,1,wxALIGN_BOTTOM);

   //the second col
   _col2->Add(_vertexRBox,1,wxALIGN_TOP);
   _col2->Add(_scalarVRBox,1,wxALIGN_BOTTOM);

   //the third col
   _col3->Add(_polydataRBox,1,wxALIGN_TOP);
   _col3->Add(_scalarPRBox,1,wxALIGN_BOTTOM);

   //the main panel sizer
   wxBoxSizer* datasetPanelGroup = new wxBoxSizer(wxHORIZONTAL);
   datasetPanelGroup->Add(_col1,0,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   datasetPanelGroup->Add(_col2,0,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   datasetPanelGroup->Add(_col3,0,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);*/

   //set this flag and let wx handle alignment
   SetAutoLayout(true);

   //assign the group to the panel
   SetSizer(datasetPanelGroup);

}
/////////////////////////////////////////////////////////
void UI_DatasetTab::makeActiveScalarOnDataset(const char* name)
{
   if(!name)return;
   if(!name)return;
   UI_DatasetInfo curDataset;           

   //get the current dataset info
   if(_dSetInfo){
      curDataset = _dSetInfo[_currentDataSet];          
   }else{
      cout<<"Datasets not specified yet!!"<<endl;
      return;
   }
   //need to update the correct radio box
   switch (curDataset.type()){
      case 2:
         //polydata
         if(_polydataRBox){
            //update the display in the polydata scalar radio box
            if(_scalarPRBox){
               _scalarPRBox->SetStringSelection(name);
            }
         }        
         break;
      case 1:
         //vertexed based
         if(_vertexRBox){
            //update the polydata scalar radio box
            if(_scalarVRBox){
               _scalarVRBox->SetStringSelection(name);
            }
         }        
         break;
      case 0:
         //3d mesh
         if(_3dRBox){
            //update the 3d scalar radio box
            if(_scalarMRBox){
               _scalarMRBox->SetStringSelection(name);
            }
            
         }        
         break;
   };
}
//////////////////////////////////////////
int UI_DatasetTab::findDataset(char* name)
{
 
   if(!_dSetInfo){
      cout<<"No datasets found!!"<<endl;
      return -1;
   }
   //search the array and find the one 
   //w/ the matching name
   for(int i = 0; i < _nDatasets; i++){
      if(!strcmp(_dSetInfo[i].name(),name)){
         return i;
      }
   }
   return -1;
   
}
////////////////////////////////
void UI_DatasetTab::updateView()
{
   //updatet the page w/ the correct names
   if(!_dSetInfo){
      cout<<"Datasets not specified"<<endl;
      return; 
   }

   if(!_foundTypes){
      cout<<"No matching types found for datasets!!"<<endl; 
      return ;      
   }
   //the index of the first found dataset
   //for a specific type
   int firstIndex = 0;

   //probably a MUCH better way to do this
   //three differnt types of datasets
   for(int i= 0;i< 3; i++){

      //all names of type "i"
      wxString* names = new wxString[_foundTypes[i]];
      firstIndex = 0;

      //loop through datasets and get the names
      int index = 0;
      for(int k =0; k < _nDatasets; k++){
         if(_dSetInfo[k].type() ==i){
           //this is not correct!!
            names[index++] = _dSetInfo[k].name();
            if(!firstIndex)firstIndex = k;
         }
      } 

      //depending on the type update the radio box
      switch(i){
         case 2 :
         //poly
            if(_polydataRBox && _foundTypes[i]){
	      _col3->Remove(_polydataRBox);
               delete  _polydataRBox;
               //create the radio box
               _polydataRBox = new wxRadioBox(this,POLYDATA_RBOX, wxT("Polydata"),
                                     wxDefaultPosition, wxDefaultSize,_foundTypes[i],
                                     names,1,wxRA_SPECIFY_COLS);

               //add the box to the sizer
               _col3->Add(_polydataRBox,1,wxALIGN_BOTTOM);

            }
            break;
         case 1 :
         //vertex
            if(_vertexRBox&& _foundTypes[i]){
	      _col2->Remove(_vertexRBox);
               delete _vertexRBox;
               //create the radio box
               _vertexRBox = new wxRadioBox(this,VERTEX_RBOX, wxT("Vertex Data"),
                                     wxDefaultPosition, wxDefaultSize,_foundTypes[i],
                                     names,1,wxRA_SPECIFY_COLS);

               //add the box to the sizer
               _col2->Add(_vertexRBox,1,wxALIGN_BOTTOM);
            }
            break;
         case 0 :
         //3dMesh
            if(_3dRBox&&_foundTypes[i]){
	      _col1->Remove(_3dRBox);
               delete  _3dRBox;
              //create the radio box
               _3dRBox = new wxRadioBox(this,RBOX_3D, wxT("3D Mesh"),
                                     wxDefaultPosition, wxDefaultSize,_foundTypes[i],
                                     names,1,wxRA_SPECIFY_COLS);

               //add the box to the sizer
               _col1->Add(_3dRBox,1,wxALIGN_BOTTOM);
            }
            break;
      };

      //the first dataset of this type 
      updateScalarBoxForDataset(firstIndex);

      //should be ok to clean up
      if(names){
         delete [] names;
      }
   }

   
}
////////////////////////////////////////////////
void UI_DatasetTab::setCurrentDataset(int index)
{
   //check if we need an update
   if(_currentDataSet == index) return;

   //the current data set
   _currentDataSet = index;

   //tell the UI_Tabs which dataset is active
   ((UI_Frame*)GetParent())->_tabs->setActiveDataset(_currentDataSet);
   

   //update the radio box for the current data set
   updateScalarBoxForDataset(_currentDataSet);
   
}
///////////////////////////////////////////////////////
void UI_DatasetTab::addDataset(UI_DatasetInfo* dataset)
{
   //check if we have set the number of datasets
   if(_nDatasets){
      if(!_dSetInfo){
         _dSetInfo = new UI_DatasetInfo[_nDatasets];
      }
      //are we trying to add more than we've allocated?
      if(_numAddedDSets < _nDatasets){
         //we're good
         _dSetInfo[_numAddedDSets] = dataset;
         _numAddedDSets++;
         return;
      }else{
         cout<<"Not enough space to add dataset!"<<endl;
         cout<<"Number of datasets already added: "
             <<_numAddedDSets<<endl;
         cout<<"Number of allowable Datasets: "<<_nDatasets<<endl;
         return;
      }
   } 
}
////////////////////////////////////////////////
void UI_DatasetTab::setFoundDataTypes(int* nType)
{
  if(!_foundTypes)_foundTypes = new int[3];
  _foundTypes[0] = nType[0]; 
  _foundTypes[1] = nType[1]; 
  _foundTypes[2] = nType[2]; 
}
////////////////////////////////////////////////////////
//this actually changes the scalar and rboxtab        //
////////////////////////////////////////////////////////
void UI_DatasetTab::updateScalarBoxForDataset(int index)
{
   UI_DatasetInfo curDataset;           
   int nScalars = 0;

   int i;
   
   //get the current dataset info
   if(_dSetInfo)
      curDataset = _dSetInfo[index];          
   else{
      cout<<"Datasets not specified yet!!"<<endl;
      return;
   }

   //biv--think there are only 3 data set types???
   int nCols = 1;
   nScalars = curDataset.numberOfScalars();

   //need this in case there are a ton of scalars
   if(nScalars> 10){
     //whole integer
     if(nScalars%10)nCols = nScalars/10;
     else nCols = nScalars/10 +1;
   }

   switch (curDataset.type()){
      case 2:
         //polydata
         if(_polydataRBox){


            //rebuild the polydata scalar radio box
            if(_scalarPRBox){
	      _col3->Remove(_scalarPRBox); // delete _scalarPRBox;   
	      //	      RemoveChild(_scalarPRBox);
	      delete _scalarPRBox;
            }
            wxString* newNames = new wxString[nScalars];

            //copy the names
            for(int i = 0; i < nScalars; i++){
               newNames[i] = curDataset.scalarName(i);

            }
            //create the radio box
            _scalarPRBox = new wxRadioBox(this,SCALAR_P_RBOX, wxT("Scalars"),
                                     wxDefaultPosition, wxDefaultSize,nScalars,
                                     newNames,nCols,wxRA_SPECIFY_COLS);

            //add the box to the sizer
            _col3->Add(_scalarPRBox,1,wxALIGN_BOTTOM);

            //update the scalar page
            //((UI_Frame*)GetParent())->_tabs->updateScalarPage(newNames,nScalars,1);
            //((UI_Frame*)GetParent())->_tabs->setActiveScalar(_scalarPRBox->GetSelection()); 
            exit(1);
            //should be ok to clean up
            if(newNames){
               delete [] newNames;
            }
            
         }        
         break;
      case 1:
         //vertexed based
         if(_vertexRBox){
            //rebuild the polydata scalar radio box
            if(_scalarVRBox){
               _col2->Remove(_scalarVRBox);
	       //	       RemoveChild(_scalarVRBox);
	       delete _scalarVRBox;

            }
            wxString* newNames = new wxString[nScalars];

            //copy the names
            for(int i = 0; i < nScalars; i++){
               newNames[i] = curDataset.scalarName(i);

            }
            //create the radio box
            _scalarVRBox = new wxRadioBox(this,SCALAR_V_RBOX, wxT("Scalars"),
                                     wxDefaultPosition, wxDefaultSize,nScalars,
                                     newNames,nCols,wxRA_SPECIFY_COLS);

            //add the box to the sizer
            _col2->Add(_scalarVRBox,1,wxALIGN_BOTTOM);

            //update the scalar page
            //((UI_Frame*)GetParent())->_tabs->updateScalarPage(newNames,nScalars,1);
            //((UI_Frame*)GetParent())->_tabs->setActiveScalar(_scalarVRBox->GetSelection()); 
            exit(1);
            //should be ok to clean up
            if(newNames){
               delete [] newNames;
            }
            
         }        
         break;
      case 0:
         //3d mesh
         if(_3dRBox){
            //rebuild the 3d scalar radio box
            if(_scalarMRBox){
	      _col1->Remove(_scalarMRBox);
	      //	      RemoveChild(_scalarMRBox);   
	      delete _scalarMRBox;

            }
            wxString* newNames = new wxString[nScalars];

            //copy the names
            for(int i = 0; i < nScalars; i++){
               newNames[i] = curDataset.scalarName(i);

            }
            //create the radio box
            _scalarMRBox = new wxRadioBox(this,SCALAR_3D_RBOX, wxT("Scalars"),
                                     wxDefaultPosition, wxDefaultSize,nScalars,
                                     newNames,nCols,wxRA_SPECIFY_COLS);

            //add the box to the sizer
            _col1->Add(_scalarMRBox,1,wxALIGN_BOTTOM);

            //update the scalar page
            //((UI_Frame*)GetParent())->_tabs->updateScalarPage(newNames,nScalars,1);
            //((UI_Frame*)GetParent())->_tabs->setActiveScalar(_scalarMRBox->GetSelection()); 
            exit(1);
            //should be ok to clean up
            if(newNames){
               delete [] newNames;
            }
            
         }        
         break;
   };
   //hack!! window only places new rbox correctly
   //if we call resize!!!?!?!?
    int width, height;
   GetParent()->GetParent()->GetSize(& width, & height);
   GetParent()->GetParent()->SetSize(-1, -1, width+1, height+1);
   return;
}
//////////////////
//event handlers//
//////////////////

////////////////////////////////////////////////
void UI_DatasetTab::_on3d(wxCommandEvent& event)
{
   int datasetIndex =-1;
   //get the current dataset that is selected in 
   //the 3d radio box
   char* name = new char[strlen(event.GetString())+1];
   strcpy(name,event.GetString());
   datasetIndex = findDataset(name);
   //set the current dataset and update the scalars
   if(datasetIndex > -1){
      setCurrentDataset(datasetIndex);
   }

   if(name){
      delete []name;
      name = 0;
   }

}
////////////////////////////////////////////////
void UI_DatasetTab::_onVertex(wxCommandEvent& event)
{
   int vertDatasetIndex =-1;
   //get the current dataset that is selected in 
   //the vertex radio box
   char* name = new char[strlen(event.GetString())+1];
   strcpy(name,event.GetString());
   vertDatasetIndex = findDataset(name);

   //set the current dataset and update the scalars
   if(vertDatasetIndex > -1){
      setCurrentDataset(vertDatasetIndex);
   }

   if(name){
      delete []name;
      name = 0;
   }
}
////////////////////////////////////////////////
void UI_DatasetTab::_onPolyData(wxCommandEvent& event)
{
   int polyDataIndex =-1;
   //get the current dataset that is selected in 
   //the polydata radio box
   char* name = new char[strlen(event.GetString())+1];
   strcpy(name,event.GetString());
   polyDataIndex = findDataset(name);

   //set the current dataset and update the scalars
   if(polyDataIndex > -1){
      setCurrentDataset(polyDataIndex);
   }

   if(name){
      delete []name;
      name = 0;
   }
}
////////////////////////////////////////////////
void UI_DatasetTab::_onScalar1(wxCommandEvent& event)
{

   //update scalar info sent to the app
   //((UI_Tabs *)GetParent())->cSc = _scalarPRBox->GetSelection();         // using zero-based scalar counting
   //((UI_Tabs *)GetParent())->cId  = CHANGE_SCALAR;
   //((UI_Tabs *)GetParent())->sendDataArrayToServer();
  //((UI_Frame*)GetParent())->_tabs->setActiveScalar(_scalarPRBox->GetSelection()); 
   exit(1);
}
////////////////////////////////////////////////
void UI_DatasetTab::_onScalar2(wxCommandEvent& event)
{
   //update scalar info sent to the app
  // ((UI_Tabs *)GetParent())->cSc = _scalarVRBox->GetSelection();         // using zero-based scalar counting
  // ((UI_Tabs *)GetParent())->cId  = CHANGE_SCALAR;
  // ((UI_Tabs *)GetParent())->sendDataArrayToServer();
  //((UI_Frame*)GetParent())->_tabs->setActiveScalar(_scalarVRBox->GetSelection()); 
   exit(1);
}
////////////////////////////////////////////////
void UI_DatasetTab::_onScalar3(wxCommandEvent& event)
{
   //update scalar info sent to the app
  //   ((UI_Tabs *)GetParent())->cSc = _scalarMRBox->GetSelection();         // using zero-based scalar counting
  // ((UI_Tabs *)GetParent())->cId  = CHANGE_SCALAR;
  // ((UI_Tabs *)GetParent())->sendDataArrayToServer();

  //((UI_Frame*)GetParent())->_tabs->setActiveScalar(_scalarMRBox->GetSelection()); 
  exit(1);
}
