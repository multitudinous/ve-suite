#ifndef _VE_UI_DATASET_PANEL_H_
#define _VE_UI_DATASET_PANEL_H_
#ifdef WIN32
#include <winsock2.h>
#endif

#include <stdlib.h>
#include <vector>
#include <wx/wx.h>
#include <wx/notebook.h>
#include "VjObsC.h"


enum DATASETS_PANEL_IDS{
   ACTIVE_RBOX,
   RBOX_3D,
   VERTEX_RBOX,
   POLYDATA_RBOX,
   SCALAR_RAD_BOX,
   SCALAR_UPDATE_BUTTON,
   MIN_PER_SLIDER,
   MAX_PER_SLIDER,
   VECTOR_RAD_BOX
};

class UI_Dataset3dScrollable;
class UI_DatasetVertexScrollable;
class UI_DatasetPolyScrollable;
class UI_DatasetPanel;


class UI_Scalars{
public:
   UI_Scalars(wxString);
   ~UI_Scalars();

   wxString _thisScalarName;

};

class UI_DataSets{
public:
   UI_DataSets();
   ~UI_DataSets();

   UI_Scalars* thisScalar;
   vector<UI_Scalars*> _Scalars;

   void _buildScalars(int, wxString*);

   wxString _dataSetName;
   int _dataSetType;
   int _numofScalars;

};



class UI_DatasetScroll: public wxScrolledWindow{
public:
   UI_DatasetScroll(wxWindow* parent);
   ~UI_DatasetScroll();

   wxRadioBox* _3dRBox;
   wxRadioBox* _vertexRBox;
   wxRadioBox* _polydataRBox;

   DECLARE_EVENT_TABLE()
};



class UI_DatasetPanel: public wxPanel{
public:
   UI_DatasetPanel(wxWindow* tControl);
   ~UI_DatasetPanel();

   UI_DataSets* thisDataSet;

   int _numSteadyStateDataSets;

   vector<UI_DataSets*> _DataSets;

   UI_DatasetScroll* _RBoxScroll;
   
  
   int _maxnoScalars;
   int _noScalars;
   wxString* _scalarNames;

   int _no3DMesh;
   int _noVertex;
   int _noPolydata;
   wxString* meshArrayNames;  
   wxString* vertexArrayNames;
   wxString* polydataArrayNames;
   wxString datatypes[3];

   //Building the panel
   void _buildPanel();

   wxScrolledWindow* _3dScroll;

   //the controls
   wxRadioBox* _activeRBox;
   wxRadioBox* _scalarRBox;
   wxRadioBox* _vectorRBox;

   wxButton* _visUpdateButton;

   wxStaticBox* _scalarRangeBox;
   wxSlider* _maxPercentSlider;
   wxSlider* _minPercentSlider;

   wxStaticBox* _dataHeadingBox;

   wxBoxSizer* _top;
   wxBoxSizer* _bottom;

   wxStaticBoxSizer* sRangeBoxSizer;

   wxSizer* _3dSizer;

   wxBoxSizer* _col1;
   wxBoxSizer* _col2;
   wxBoxSizer* _col3;
   void _setScalars(UI_DataSets*);
protected:
   void _buildDataSets();
   
   void _setScalarsnoDatasets();
   void _organizeRadioBoxInfo();
   void _organizeActiveRBox();
   void _onActive(wxCommandEvent& event);
   void _on3d(wxCommandEvent& event);
   void _onVertex(wxCommandEvent& event);
   void _onPolyData(wxCommandEvent& event);
   void _onScalars(wxCommandEvent& event);
   void _onUpdate(wxCommandEvent& event);
   void _onMinMaxSlider(wxCommandEvent& event);

   DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_DATASET_PANEL_H_



