#ifndef _VE_UI_STREAMLINE_TAB_H_
#define _VE_UI_STREAMLINE_TAB_H_
#ifdef WIN32
#include <winsock2.h>
#endif
#include <wx/wx.h>

enum STREAMLINE_TAB_IDS{
   CURSOR_SELECT_RBOX,
   DIR_RBOX,
   INTEGRATE_DIR_RBOX,
   COMP_STREAMLINE_BUTTON,
   PARTICLE_TRACK_BUTTON,
   SEED_POINTS_CHK,
   NUM_PTS_SLIDER,
   SIZE_SLIDER,
   PROP_SLIDER,
   INT_STEP_SLIDER,
   STEP_SLIDER
};


class UI_StreamlineTab : public wxPanel{
public:
   UI_StreamlineTab(wxNotebook* tControl);
protected:
   void _buildPage();

   wxNotebook* _parent;
   //the event controls
   wxSlider* _propSlider;
   wxSlider* _iStepSlider;
   wxSlider* _stepSlider;
   wxSlider* _nPtsSlider;
   wxSlider* _sizePerSlider;
   wxRadioBox* _cursorRBox;
   wxRadioBox* _directionRBox;
   wxRadioBox* _integrationDirRBox;
   wxButton* _compStreamButton;
   wxButton* _parTrackingButton;
   wxCheckBox* _lastSeedPtChk;

   //event handling callbacks
   void _onCursorSelect(wxCommandEvent& event);
   void _onDirection(wxCommandEvent& event);
   void _onIntegrateDir(wxCommandEvent& event);
   void _onParticleTrack(wxCommandEvent& event);
   void _onCompStreamline(wxCommandEvent& event);
   void _onCheck(wxCommandEvent& event);
   void _oniStepSlider(wxScrollEvent& event);
   void _onPropSlider(wxScrollEvent& event);
   void _onStepSlider(wxScrollEvent& event);
   void _onnPointsSlider(wxScrollEvent& event);

   void ConstructCommandId( void );

   DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_STREAMLINE_TAB_H_
