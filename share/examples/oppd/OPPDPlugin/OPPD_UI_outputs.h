#ifndef _OPPD_UI_OUTPUTS_H_
#define _OPPD_UI_OUTPUTS_H_

#include "OPPD_UI.h"
#include <wx/wx.h>
#include <wx/notebook.h>

class OPPD_UI_Dialog;

enum {

};


class OPPD_UI_outputs: public wxPanel
{
public:


	OPPD_UI_outputs(wxNotebook* parent);

	~OPPD_UI_outputs();

	OPPD_UI_Dialog* Dialog;

	wxTextCtrl* _tsec;
	wxTextCtrl* _tmin;
	wxTextCtrl* _hrrkw;
	wxTextCtrl* _hrrbtu;
	wxTextCtrl* _detsprinktime;
	wxTextCtrl* _detsmtime;
	wxTextCtrl* _detfthtime;
	wxTextCtrl* _flwallinehgt;
	wxTextCtrl* _flcornerhgt;
	wxTextCtrl* _flwallhgt;
	wxTextCtrl* _hrrhrr;
	wxTextCtrl* _hrrburndur;
	wxTextCtrl* _hrrhgthesk;
	wxTextCtrl* _hrrhgtthom;
	wxTextCtrl* _pltemp;
	wxTextCtrl* _tcltemp;
	wxTextCtrl* _visdist;
	wxButton*   _loadresultsButton;
protected:
  void _buildPage();


	DECLARE_EVENT_TABLE();

};
#endif //_OPPD_UI_OUTPUTS_H_