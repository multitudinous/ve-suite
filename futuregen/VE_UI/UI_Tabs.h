#ifndef _VE_UI_TABS_H_
#define _VE_UI_TABS_H_
#ifdef WIN32
#include <winsock2.h>
#endif
#include "wx/wx.h"
#include "wx/notebook.h"
#include "VjObsC.h"
#include "controlIds.h"
#include <string>

#include "UI_NavTab.h"
#include "UI_ScalarTab.h"
#include "UI_VisTab.h"
#include "UI_VecTab.h"
#include "UI_StreamTab.h"
#include "UI_DataSetTab.h"
#include "UI_TeacherTab.h"
#include "UI_SoundsTab.h"
#include "UI_GeometryTab.h"

///copied from original Tabs.h//////
enum cfdGeodeEnum {
   // Everything below has to be mirrored exactly in 
   // VE_Suite/VE_Xplorer/UMI/hello/config/mapping.config
   CONTOUR, X_CONTOUR, Y_CONTOUR, Z_CONTOUR,
   X_CONTOURS, Y_CONTOURS, Z_CONTOURS, 
   MOMENTUM, X_MOMENTUM, Y_MOMENTUM,Z_MOMENTUM,
   X_MOMENTUMS, Y_MOMENTUMS, Z_MOMENTUMS, 
   VECTOR, X_VECTOR, Y_VECTOR, Z_VECTOR,
   X_VECTORS, Y_VECTORS, Z_VECTORS, 
   STREAMLINES, ISOSURFACE,
   IMAGE_EX,   //yang-REI: changed due to conflict with /usr/include/Performer/image.h
   POLYDATA,
   SWITCH_CURSOR,
   ANIMATED_STREAMLINES,
   ANIMATED_IMAGES,
   PARTICLES,
   TRANS_GEOM,
   X_TRANSIENT,
   Y_TRANSIENT,
   Z_TRANSIENT,
   PARTICLE_TRANSIENT,
   // Everything below has to be mirrored exactly in 
   // VE_Suite/VE_Xplorer/UMI/hello/config/mapping2.config
   // The first of the following non-geode related commands specifies an 
   // offset: this offset must be the same as that used to set up the
   // instance of id_mapper class called "my_mapper2" in client.java
   CHANGE_SCALAR=100,
   CHANGE_SCALAR_RANGE, //101
   UPDATE_GEOMETRY,//102
   SEND_DRAW,//103
   UPDATE_SEND_PARAM,//104
   RECORD_SCENE,//105
   CLEAR_ALL,//106
   SET_TRANSIENT_OPTIONS,
   TRANSIENT_RESET,
   TRANSIENT_BACKWARD,
   LOAD_PFB_FILE,
   CLEAR_PFB_FILE,
   TRANSIENT_FORWARD,
   TRANSIENT_STOP,
   COMPUTE_STREAMLINES,
   USE_LAST_STREAMLINE_SEEDPOINTS,
   CHANGE_STREAMLINE_CURSOR,
   NO_CURSOR,
   POINT_CURSOR,
   X_LINE_CURSOR,
   Y_LINE_CURSOR,
   Z_LINE_CURSOR,
   X_PLANE_CURSOR,
   Y_PLANE_CURSOR,
   Z_PLANE_CURSOR,
   BLUE_MENU_TOGGLE,
   SCALAR_BAR_TOGGLE,
   CHANGE_STEADYSTATE_DATASET,
   CHANGE_VECTOR_THRESHOLD,
   CHANGE_VECTOR_MASK_RATIO,
   CHANGE_VECTOR_SCALE,
   SCALE_BY_VECTOR_MAGNITUDE,
   BACKWARD_INTEGRATION,
   FORWARD_INTEGRATION,
   TWO_DIRECTION_INTEGRATION,
   CHANGE_PROPAGATION_TIME,
   CHANGE_INT_STEP_LENGTH,
   CHANGE_STEP_LENGTH,
   CHANGE_CONTOUR_FILL,
   UPDATE_SOUNDS,
   CHANGE_PARTICLE_VIEW_OPTION,
   CHANGE_SPHERE_SIZE,
   LOAD_POINT,
   WRITE_POINTS_TO_FILE,
   READ_POINTS_FROM_FILE,
   MOVE_TO_SELECTED_LOCATION,
   EXIT,
   //biv--added these for the navigation page
   GUI_NAV,
   NAV_UP,
   NAV_DOWN,
   NAV_LEFT,
   NAV_RIGHT,
   NAV_FWD,
   NAV_BKWD,
   NAV_CCW,
   NAV_CW
};
//end Tabs.h copy/////////

//////////////////////////////////////////////
//the control for the different pages "tabs"//
//on the VE_UI                              //
//////////////////////////////////////////////
class UI_Tabs: public wxNotebook{
public:
/*   UI_Tabs(wxWindow* parent, wxWindowID =-1,
            const wxPoint& pos = wxDefaultPosition,
            const wxSize& size = wxDefaultSize, 
            long style = 0);
*/
   UI_Tabs(VjObs_ptr ref, wxWindow* parent, wxWindowID =-1,
            const wxPoint& pos = wxDefaultPosition,
            const wxSize& size = wxDefaultSize, 
            long style = 0);

   void updateScalarPage(char** scalarNames,int numScalars, int refresh =0); 
   void updateScalarPage(wxString* scalarNames,int numScalars, int refresh =0); 

   //create the individual pages for the notebook
   void createTabPages();
  
   //set active scalar
   void setActiveScalar(int whichScalar);

   //set active dataset
   void setActiveDataset(int whichDataset);

   //send scalar page selection changes
   //to the dataset tab
   void changeActiveScalarOnDataset(const char* activeScalarName);
//protected:
   void _initDatasetPage();

   //the tab pages
   UI_VisualizationTab* _visPage;
   UI_ScalarTab* _scalarPage;
   UI_VectorTab* _vectorPage;
   UI_StreamlineTab* _streamlinePage;
   UI_DatasetTab* _datasetPage;
   UI_SoundTab* _soundPage;
   UI_GeometryTab* _geometryPage;
   UI_TeacherTab* _teacherPage;
   UI_NavigationTab* _navPage;
///////copied from old Tabs.h///////
   //the pointer to the server
   VjObs_var server_ref;

   short    datasetNum;
   int      numScalarsInActiveDataset;
   short    num_geo;
   short    numSounds;
   short    num_teacher;
   short    numSteadyStateDataSets;
   bool     hasXPostData;
   bool     hasYPostData;
   bool     hasZPostData;
   VjObs::scalar_p_var   sc_attrib;
   VjObs::scalar_p_var   datasetNames;
   VjObs::obj_p_var   datasetTypes;
   VjObs::obj_p_var   numScalarsPerDataset;
   VjObs::scalar_p_var   geoNameArray;
   VjObs::scalar_p_var   soundNameArray;
   VjObs::scalar_p_var   teacher_attrib;
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
   short cGetClientInfo;
   
   // Must be the same size as specified in VjObs_i.h
   int numOfClientInfo;
   VjObs::obj_p_var clientInfoArray;
   void sendDataArrayToServer( void );


  // DECLARE_EVENT_TABLE();
};
#endif //_VE_UI_TABS_H_
