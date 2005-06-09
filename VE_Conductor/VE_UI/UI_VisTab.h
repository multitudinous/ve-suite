#ifndef _VE_UI_VISUALIZATION_TAB_H_
#define _VE_UI_VISUALIZATION_TAB_H_
#ifdef WIN32
#include <winsock2.h>
#endif

#include <wx/wx.h>

//#include "controlIds.h"
//Visualizaton tab control ids
enum VIS_TAB_IDS
{
   CATEGORY_RAD_BOX,
   CONTOUR_RAD_BOX,
   DIRECTION_RBOX,
   PRE_COMP_SURF_BUTTON,
   POLYDATA_WARPED_DURFACE,
   SINGLE_PLANE_BUTTON,
   CYCLE_CHECK_BOX,
   NEAREST_PLANE_CHECK_BOX,
   VIS_SLIDER,
   UPDATE_BUTTON,
   SCALAR_BAR_CHECK_BOX,
   RECORD_BUTTON,
   CLEAR_BUTTON,
   EXIT_BUTTON,
   CUSTOM_VIS_BUTTON,
   TRANSIENT_CHECK_BOX,
   CFD_VIS_OPTION,
   MIRROR_CHECK_BOX
};

class UI_VisualizationTab : public wxScrolledWindow
{
   public:

      UI_VisualizationTab(wxNotebook* tControl);
      ~UI_VisualizationTab(){;}

      void _onExit(wxCommandEvent& event);

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
      wxCheckBox* _scalarBarCBox;
      wxCheckBox* _transientCheckBox;
      wxCheckBox* _visOptionCheckBox;
      wxCheckBox* mirrorOptionCheckBox;
      wxButton* _recordButton;
      wxButton* _clearButton;
      wxButton* _exitButton;
      wxButton* _customVisButton;

      //create this page
      void _buildPage( void );
      void createCommandId( void );
      void createTransientCommandId( void );
      //vispage control event callbacks
      void _onCategory(wxCommandEvent& event);
      void _onContour(wxCommandEvent& event);
      void _onDirection(wxCommandEvent& event);
      void _onPreComp(wxCommandEvent& event);
      void _onSingle(wxCommandEvent& event);
      void _onNearest(wxCommandEvent& event);
      void _onUpdate(wxCommandEvent& event);
      void _onScalarBar(wxCommandEvent& event);
      void _onRecord(wxCommandEvent& event);
   
      void _onSlider(wxScrollEvent& event);
      void _onClear(wxCommandEvent& event);
      void _onCustomVis(wxCommandEvent& event);
      void _onTextureBasedVisual(wxCommandEvent& event);
      void _onMirrorVisualization(wxCommandEvent& event);
  
   DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_VISUALIZATION_TAB_H_
