#ifndef _VE_UI_VECTOR_TAB_H_
#define _VE_UI_VECTOR_TAB_H_

#ifdef WIN32
#include <winsock2.h>
#endif

#include <wx/wx.h>
#include <wx/notebook.h>
//#include "controlIds.h"

//Vector tab control ids
enum VECTOR_TAB_IDS{
   VECTOR_UPDATE_BUTTON,
   SCALE_VEC_MAG_CHK,
   MIN_THRESH_SLIDER,
   MAX_THRESH_SLIDER,
   RATIO_SLIDER,
   SCALE_SLIDER,
   VECTOR_RAD_BOX

};

class UI_VectorTab : public wxPanel{
public:
   UI_VectorTab(wxNotebook* tControl);
protected:
   void _buildPage();

   wxNotebook* _parent;
   //the controls
   wxSlider* _vThresholdMinSlider;
   wxSlider* _vThresholdMaxSlider;
   wxSlider* _vRatioSlider;
   wxSlider* _vScaleSlider;
   wxRadioBox* _vectorRBox;
   wxCheckBox* _scaleVecMagChk;
   wxButton* _updateButton;

   //event callback fucntions
   void _onUpdate(wxCommandEvent& event);
   void _onCheck(wxCommandEvent& event);
   void _onvRatioSlider(wxScrollEvent& event);
   void _onvScaleSlider(wxScrollEvent& event);
   void _onThresholdSlider(wxScrollEvent& event);

   DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_VECTOR_TAB_H_
