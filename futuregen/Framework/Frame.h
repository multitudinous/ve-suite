#ifndef APP_FRAME_H
#define APP_FRAME_H
#ifdef WIN32
#include <winsock2.h>
#endif
#include <wx/wx.h>
#include <wx/config.h> 
#include <wx/splitter.h>
#include "Avail_Modules.h"
#include "Network.h"
#include "UI_Tabs.h"
#include "moduleC.h"
#include <orbsvcs/CosNamingC.h>
#include "UI_i.h"


const wxString LOCATION = _T("Framesize");
const wxString LOCATION_X = _T("LocationX");
const wxString LOCATION_Y = _T("LocationY");
const wxString LOCATION_W = _T("LocationW");
const wxString LOCATION_H = _T("LocationH");

enum {
  v21ID_CONNECT,
  v21ID_CONNECT_VE,
  v21ID_SUBMIT,
  v21ID_LOAD, 
  v21ID_DISCONNECT, 
  v21ID_UNDO, 
  v21ID_REDO, 
  v21ID_ZOOMIN, 
  v21ID_ZOOMOUT,
  v21ID_START_CALC, 
  v21ID_STOP_CALC,
  v21ID_PAUSE_CALC,
  v21ID_RESUME_CALC, 
  v21ID_VIEW_RESULT,
  v21ID_GLOBAL_PARAM,
  v21ID_BASE,
  v21ID_SOUR,
  v21ID_REI_BASE,
  v21ID_REI_SOUR,
  v21ID_SWEET,
  v21ID_CO_DISPOSAL,
  MYLOG,
  v21ID_HELP
};

class OrbThread;
class PEThread;

class AppFrame : public wxFrame
{
 public:
  
  AppFrame() {};
  AppFrame(wxWindow* parent, wxWindowID id, const wxString& title);
  void OnClose (wxCloseEvent &event);

  wxSplitterWindow * wx_log_splitter;
  wxSplitterWindow * wx_ve_splitter;
  wxSplitterWindow * wx_nw_splitter;
  wxMenuBar* menubar;
  wxToolBar* toolbar;
  wxIcon* icon;
  
  wxTextCtrl *logwindow;
  Avail_Modules* av_modules;
  Network* network;
  
  UI_Tabs *m_tabs;
  	
  wxImageList *m_imageList;
  CORBA::ORB_var orb;
  PortableServer::POA_var poa;
  CosNaming::NamingContext_var naming_context;
  Body_UI_i* p_ui_i;
  wxMenu *file_menu;
  wxMenu *con_menu;
  wxMenu *run_menu;
  wxMenu *edit_menu;
  wxMenu *help_menu;
  wxMenu *config_menu;
  
  PEThread* pelog;

  void Log(const char* msg);
  //ACE_Thread_Mutex _mutex; //public mutex for the execution order
 protected:
  int m_frameNr;
  wxString fname;
  wxString directory;
  wxString path;	
	
  VjObs_var vjobs;
  
 private:
  bool is_orb_init;

 protected:
  wxRect DetermineFrameSize (wxConfig* config);
  void StoreFrameSize (wxRect rect, wxConfig* config);
  void CreateMenu();
  void ZoomIn(wxCommandEvent &event);
  void ZoomOut(wxCommandEvent &evetn);
  void Save(wxCommandEvent &event);
  void SaveAs(wxCommandEvent &event);

  void Open(wxCommandEvent &event);
  void SubmitToServer(wxCommandEvent &event);
  void LoadFromServer(wxCommandEvent &event);
  void StartCalc(wxCommandEvent &event);	
  void StopCalc(wxCommandEvent &event);	
  void PauseCalc(wxCommandEvent &event);	
  void ResumeCalc(wxCommandEvent &event);	
  void ViewResult(wxCommandEvent &event);
  void ViewHelp(wxCommandEvent &event);
  void GlobalParam(wxCommandEvent &event);
  void ConExeServer(wxCommandEvent &event);
  void DisConExeServer(wxCommandEvent &event);
  void ConVEServer(wxCommandEvent &event);
  void LoadBase(wxCommandEvent &event);
  void LoadSour(wxCommandEvent &event);
  void LoadREIBase(wxCommandEvent &event);
  void LoadREISour(wxCommandEvent &event);
  void New(wxCommandEvent &event);
  bool init_orb_naming();
  void CreateVETab();
  void OnUpdateUIPop(wxUpdateUIEvent& event);

  DECLARE_EVENT_TABLE()
};


#endif


//How's the scheduler's going to use
