#ifndef TABS_H
#define TABS_H

#include <wx/wx.h>
#include <wx/notebook.h>

// Define a new application
class MyApp : public wxApp
{
   public:
      bool OnInit();
      void InitObserver();
};

DECLARE_APP(MyApp);


enum {
  R_CATEGORY,
  R_CONTOUR_TYPE,
  R_DIRECTION,
  PRECOMP,
  SINGLE,
  CYCLE,
  NEAREST,
  SLIDER,
  UPDATE,
  BLUE_MENU,
  SCALAR_BAR,
  RECORD,
  CLEAR,
  EXIT
};

//
class Tabs : public wxNotebook
{
 public:
  Tabs(wxWindow *parent, wxWindowID id = -1,
       const wxPoint& pos = wxDefaultPosition,
       const wxSize& size = wxDefaultSize, long style = 0);
  
  wxRadioBox *R_Category;
  wxRadioBox *R_Contour_type;
  wxRadioBox *R_Direction;
  
  wxRadioButton *R_precomp_but;
  wxRadioButton *R_single_but;
  wxCheckBox *C_cycle_but;
  wxCheckBox *C_nearest_but;

  wxSlider *S_value;
  wxButton *B_update;

  wxCheckBox *C_blue_menu;
  wxCheckBox *C_scalar_bar;
  wxButton *B_record;
  wxButton *B_clear;
  wxButton *B_exit;


  void  CreateInitialPages();

 protected:
  wxPanel* CreateFirstPage();
  wxPanel* CreateSecondPage();

  void OnCategory(wxCommandEvent& event);
  void OnContour(wxCommandEvent& event);
  void OnDirection(wxCommandEvent& event);
  void OnPreComp(wxCommandEvent& event);
  void OnSingle(wxCommandEvent& event);
  void OnNearest(wxCommandEvent& event);
 
  void OnUpdate(wxCommandEvent& event);
  void OnBlueMenu(wxCommandEvent& event);
  void OnScalarBar(wxCommandEvent& event);
  void OnRecord(wxCommandEvent& event);
  void OnExit(wxCommandEvent& event);

  DECLARE_EVENT_TABLE()
};

//

class MyFrame : public wxFrame
{
 public:
  
  MyFrame(const wxString& title, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxDEFAULT_FRAME_STYLE);
  virtual ~MyFrame();
  
  Tabs *m_tabs;
  wxBoxSizer *m_sizerFrame;
  wxImageList *m_imageList;
};
   
#endif
