#ifndef _VE_UI_TEACHER_TAB_H_
#define _VE_UI_TEACHER_TAB_H_
#ifdef WIN32
#include <winsock2.h>
#endif

#include <wx/wx.h>
#include <wx/notebook.h>
//#include "controlIds.h"
enum TEACHER_TAB_IDS{
   TEACHER_RBOX,
   TEACHER_CLEAR_BUTTON
};


class UI_TeacherTab : public wxPanel{
public:
   UI_TeacherTab(wxNotebook* tControl);
protected:
   void _buildPage();
   
   wxNotebook* _parent;
   //the controls
   wxRadioBox* _teacherRBox;
   wxButton* _clearButton;
   //event handlers
   void _onTeacher(wxCommandEvent& event);
   void _onClear(wxCommandEvent& event);

   DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_TEACHER_TAB_H_
