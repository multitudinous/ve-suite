#ifndef _VE_UI_DATASET_TAB_H_
#define _VE_UI_DATASET_TAB_H_
#ifdef WIN32
#include <winsock2.h>
#endif

#include <wx/wx.h>
#include <wx/notebook.h>
//#include "controlIds.h"
enum DATASETS_TAB_IDS{
   RBOX_TABS_3D,
   VERTEX_TABS_RBOX,
   POLYDATA_TABS_RBOX,
   SCALAR_V_RBOX,
   SCALAR_P_RBOX,
   SCALAR_3D_RBOX
};

//this class contains individual dataset info
class UI_DatasetInfo{
public:
   UI_DatasetInfo();
   UI_DatasetInfo(const char* name,int type);
   ~UI_DatasetInfo();

   //set the name of this dataset
   void setDatasetName(const char* name);

   //set the type of this dataset
   void setDatasetType(int type){_type = type;}

   //set the scalar data for this dataset
   void setScalarData(char** names, int numScalars);


   //get the type of the dataset
   int type(){return _type;}


   //get the name of this dataset
   char* name(){return _name;}

   //get the scalar name
   const char* scalarName(int whichScalar);

   //the number of scalars
   int numberOfScalars(){return _nScalars;}

   //equal operator
   UI_DatasetInfo& operator=(const UI_DatasetInfo& rhs);
   UI_DatasetInfo& operator=(const UI_DatasetInfo* rhs);

protected:
   int _type;
   char* _name;

   char** _scalarNames;
   int _nScalars;
} ;



class UI_DatasetTab: public wxScrolledWindow{
public:
   UI_DatasetTab(wxWindow* tControl);
   ~UI_DatasetTab();

   //set the number of datasets
   void setNumberOfDatasets(int nDataSets){_nDatasets = nDataSets;}

   //void set current dataset
   void setCurrentDataset(int index);

   //update the dataset tab page to reflect changes in the dataset
   void updateView(); 

   //update the radio box for a particular data set
   void updateScalarBoxForDataset(int whichDataSet);
  
   //add dataset
   void addDataset(UI_DatasetInfo* dataset);

   //found data types
   void setFoundDataTypes(int* foundTypes);

   //update the scalar box to reflect changes 
   //made on the scalar tab
   void makeActiveScalarOnDataset(const char* name);

   // searches an array of dataset infos for a specific
   //name and returns the index of the dsi in the array
   int findDataset(char* name);

protected:
   void _buildPage();

   //number of found types
   int* _foundTypes;

   //the current dataset's index
   int _currentDataSet;

   //the number of added datasets
   int _numAddedDSets;

   //the number of datasets
   int _nDatasets;
  
   int* _dataSetType;

   //info for the datasets
   UI_DatasetInfo* _dSetInfo;   

   //the controls
   wxRadioBox* _3dRBox;
   wxRadioBox* _vertexRBox;
   wxRadioBox* _polydataRBox;
   wxRadioBox* _scalarPRBox;
   wxRadioBox* _scalarMRBox;
   wxRadioBox* _scalarVRBox;


   wxBoxSizer* _col1;
   wxBoxSizer* _col2;
   wxBoxSizer* _col3;

   //event handlers
   void _on3d(wxCommandEvent& event);
   void _onVertex(wxCommandEvent& event);
   void _onPolyData(wxCommandEvent& event);
   void _onScalar1(wxCommandEvent& event);
   void _onScalar2(wxCommandEvent& event);
   void _onScalar3(wxCommandEvent& event);

   DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_DATASET_TAB_H_
