#ifndef _VE_UI_SCALARS_TAB_H_
#define _VE_UI_SCALARS_TAB_H_
#ifdef WIN32
#include <winsock2.h>
#endif

#include <wx/wx.h>

//Scalars tab control ids
enum SCALAR_TAB_IDS{
   SCALAR_RAD_BOX,
   SCALAR_UPDATE_BUTTON,
   MIN_PER_SLIDER,
   MAX_PER_SLIDER,
   VECTOR_RAD_BOX
};


class UI_ScalarTab: public wxScrolledWindow{
public:
   UI_ScalarTab(wxWindow* tabControl);
   UI_ScalarTab(wxWindow* tabControl,int numScalars,char** scalarNames);

   //set the scalar names and number of scalars
   void updateScalarTabRadioBoxInfo(int numScalars, char** scalarNames, int refresh =0);
   void updateScalarTabRadioBoxInfo(int numScalars, wxString* scalarNames, int refresh =0);

   //disable access to the radiobox
   //used if there are no scalars present
   void disableRadioBox();

   //enable access to the radiobox
   void enableRadioBox();
 
   //set the active scalar to reflect changes on dataset page
   void setActiveScalar(int whichScalar);
protected:
   wxWindow* _parent;
   //scalar info
   int _nScalars;

   //the controls
   wxRadioBox* _scalarRBox;
//NEW!!!!!!!!!!!!!!!!!! - from VecTab
   wxRadioBox* _vectorRBox;

   wxButton* _visUpdateButton;

   wxStaticBox* _scalarRangeBox;
   wxSlider* _maxPercentSlider;
   wxSlider* _minPercentSlider;

   wxBoxSizer* _leftSizer;
   //build this page
   void _buildPage();

   //control event callbacks
   void _onScalars(wxCommandEvent& event);
   void _onUpdate(wxCommandEvent& event);
   void _onMinMaxSlider(wxScrollEvent& event);
   
   DECLARE_EVENT_TABLE()

};
#endif //_VE_UI_SCALARS_TAB_H_

