#ifndef _VE_UI_STREAMLINE_TAB_H_
#define _VE_UI_STREAMLINE_TAB_H_
#ifdef WIN32
#include <winsock2.h>
#endif
#include <wx/wx.h>

enum STREAMLINE_TAB_IDS
{
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
   STEP_SLIDER,
   DIAMETER_SLIDER,
   ARROW_POINTS_CHK,
   PARTICLE_DIALOG,
   SPHERE_SCALE_SLIDER
};

class UI_TransientDialog;

class UI_StreamlineTab : public wxScrolledWindow
{
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
   wxSlider* _diameterSlider;
   wxSlider* sphereScaleSlider;
   wxRadioBox* _cursorRBox;
   wxRadioBox* _directionRBox;
   wxRadioBox* _integrationDirRBox;
   wxButton* _compStreamButton;
   wxButton* _parTrackingButton;
   wxCheckBox* _lastSeedPtChk;
   wxCheckBox* arrowPointsChk;

   UI_TransientDialog* particleControls;

   //event handling callbacks
   void _onCursorSelect(wxCommandEvent& );
   void _onDirection(wxCommandEvent& );
   void _onIntegrateDir(wxCommandEvent& );
   void _onParticleTrack(wxCommandEvent& );
   void _onCompStreamline(wxCommandEvent& );
   void _onCheck(wxCommandEvent& );
   void OnArrowCheck( wxCommandEvent& );
   void _oniStepSlider(wxScrollEvent& );
   void _onPropSlider(wxScrollEvent& );
   void _onStepSlider(wxScrollEvent& );
   void _onnPointsSlider(wxScrollEvent& );
   void _onDiameterSlider(wxScrollEvent& );
   void onScaleSlider( wxScrollEvent& );

   void ConstructCommandId( void );

   DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_STREAMLINE_TAB_H_
