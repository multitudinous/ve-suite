#ifndef _VE_UI_TABS_H_
#define _VE_UI_TABS_H_
#ifdef WIN32
#include <winsock2.h>
#endif

#include "wx/wx.h"
#include "wx/notebook.h"

#ifdef _TAO
#include "VjObsC.h"
#else
#include "VjObs.h"
#endif


#include <string>

class UI_NavigationTab;
class UI_StreamTab;
class UI_TeacherTab;
class UI_SoundTab;
class UI_GeometryTab;
class UI_ViewLocTab;
class UI_DesignParTab;
class UI_VertTab;
class UI_TransTab;
class UI_ModelData;
class UI_VisualizationTab;
class UI_VectorTab;
class UI_StreamlineTab;

#ifdef VE_PATENTED
#ifdef CFD_USE_SHADERS
class UI_AdvectionPanel;
#endif
#endif

//////////////////////////////////////////////
//the control for the different pages "tabs"//
//on the VE_UI                              //
//////////////////////////////////////////////
class UI_Tabs: public wxNotebook{
public:
   UI_Tabs(VjObs_ptr ref, wxWindow* parent, UI_ModelData* _model,
            int activeMod, 
            wxWindowID =-1,
            const wxPoint& pos = wxDefaultPosition,
            const wxSize& size = wxDefaultSize, 
            long style = 0);
/*
   UI_Tabs(VjObs_ptr ref, wxWindow* parent, wxWindowID =-1,
            const wxPoint& pos = wxDefaultPosition,
            const wxSize& size = wxDefaultSize, 
            long style = 0);*/

   ~UI_Tabs();

   void getData( void );
   void updateScalarPage(char** scalarNames,int numScalars, int refresh =0); 
   void updateScalarPage(wxString* scalarNames,int numScalars, int refresh =0); 

   //create the individual pages for the notebook
   void createTabPages();
  
   //set active scalar
   void setActiveScalar(int whichScalar);

   //set active dataset
   void setActiveDataset(int whichDataset);

   void rebuildTabPages(int activeModIndex);

#ifdef VE_PATENTED
#ifdef CFD_USE_SHADERS
   void DisableAdvectionPage();
   void EnableAdvectionPage();
#endif
#endif
   UI_ModelData* _modelData;
   int _activeModIndex;

   //send scalar page selection changes
   //to the dataset tab
   //void changeActiveScalarOnDataset(const char* activeScalarName);
//protected:
   //void _initDatasetPage(UI_DatasetTab* _datasetPage);

   //the tab pages
   UI_VisualizationTab* _visPage;
   //UI_ScalarTab* _scalarPage;
   UI_VectorTab* _vectorPage;
   UI_StreamlineTab* _streamlinePage;
   //UI_DatasetTab* _datasetPage;
   UI_SoundTab* _soundPage;
   UI_GeometryTab* _geometryPage;
   UI_TeacherTab* _teacherPage;
   UI_NavigationTab* _navPage;
   UI_VertTab* _vertPage;
   UI_TransTab* _transPage;
   UI_ViewLocTab* _viewlocPage;
   UI_DesignParTab* _designparPage;

#ifdef VE_PATENTED
#ifdef CFD_USE_SHADERS
   UI_AdvectionPanel* _advectionPage;
#endif
#endif
///////copied from old Tabs.h///////
   //the pointer to the server
   VjObs_var server_ref;

   //short    datasetNum;
   //int      numScalarsInActiveDataset;
   short    num_geo;
   short    num_sounds;
   short    num_teacher;
   short    num_viewlocs;
   //short    numSteadyStateDataSets;
   //bool     hasXPostData;
   //bool     hasYPostData;
   //bool     hasZPostData;
   //VjObs::scalar_p_var   sc_attrib;
   //VjObs::scalar_p_var   datasetNames;
   //VjObs::obj_p_var   datasetTypes;
   //VjObs::obj_p_var   numScalarsPerDataset;
   VjObs::scalar_p   geoNameArray;
   VjObs::scalar_p_var   soundNameArray;
   VjObs::scalar_p_var   teacher_attrib;
   VjObs::scalar_p_var   viewlocNameArray;
   VjObs::Models_var   _models;
   //VjObs::scalar_p_var   viewlocNewPointName;
   std::string viewlocNewPointName;
   std::string   dest;
   short    dest_num;
   short    dest_id;
   int*   sc_min;
   int*   sc_max;

   int cuttingDirection;

   // State variables passed to VjObs_i interface
   short cNumScalars;
   short cNnumVectors;
   short cNumGeoArrays;
   int   cClients;
   int   cIso_value;
   int   cSc;
   int   cMin;
   int   cMax;
   long  cId;
   long  cGeo_state;
   short cPostdata_state;
   bool  cPre_state;
   short cTimesteps;
   short cNumTeacherArrays;
   short cTeacher_state;
   //short cGetClientInfo;
   
   // Must be the same size as specified in VjObs_i.h
   int numOfClientInfo;
   VjObs::obj_pd_var clientInfoArray;
   void sendDataArrayToServer( void );


  // DECLARE_EVENT_TABLE();
};
#endif //_VE_UI_TABS_H_
