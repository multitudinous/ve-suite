#ifndef _VE_UI_VISUALIZATION_TAB_H_
#define _VE_UI_VISUALIZATION_TAB_H_
#ifdef WIN32
#include <winsock2.h>
#endif
#include "wx/wx.h"
#include "wx/notebook.h"
#include <iostream>
using namespace std;

//#include "controlIds.h"
//Visualizaton tab control ids
enum VIS_TAB_IDS{
   CATEGORY_RAD_BOX,
   CONTOUR_RAD_BOX,
   DIRECTION_RBOX,
   PRE_COMP_SURF_BUTTON,
   SINGLE_PLANE_BUTTON,
   CYCLE_CHECK_BOX,
   NEAREST_PLANE_CHECK_BOX,
   VIS_SLIDER,
   UPDATE_BUTTON,
   BLUE_MENU_CHECK_BOX,
   SCALAR_BAR_CHECK_BOX,
   RECORD_BUTTON,
   CLEAR_BUTTON,
   EXIT_BUTTON
};


class UI_VisualizationTab : public wxPanel {
public:

   UI_VisualizationTab(wxNotebook* tControl);
   ~UI_VisualizationTab(){};

protected:
   wxNotebook* _parent;
   //the controls
   wxRadioBox* _categoryRBox;
   wxRadioBox* _contourRBox;
   wxRadioBox* _directionRBox;
   wxRadioButton* _pcsButton;
   wxRadioButton* _spButton;
   wxCheckBox* _cycleCBox;
   wxCheckBox* _nearestCBox;
   wxSlider* _slider;
   wxButton* _sliderUpdate;
   wxCheckBox* _bMenuCBox;
   wxCheckBox* _scalarBarCBox;
   wxButton* _recordButton;
   wxButton* _clearButton;
   wxButton* _exitButton;

   //create this page
   void _buildPage();
   void createCommandId();
   //vispage control event callbacks
   void _onCategory(wxCommandEvent& event);
   void _onContour(wxCommandEvent& event);
   void _onDirection(wxCommandEvent& event);
   void _onPreComp(wxCommandEvent& event);
   void _onSingle(wxCommandEvent& event);
   void _onNearest(wxCommandEvent& event);
   void _onUpdate(wxCommandEvent& event);
   void _onBlueMenu(wxCommandEvent& event);
   void _onScalarBar(wxCommandEvent& event);
   void _onRecord(wxCommandEvent& event);
   void _onExit(wxCommandEvent& event);
   void _onSlider(wxCommandEvent& event);
   void _onClear(wxCommandEvent& event);
  DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_VISUALIZATION_TAB_H_
